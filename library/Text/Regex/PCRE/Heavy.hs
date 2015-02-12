{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-unused-binds #-}
{-# LANGUAGE UndecidableInstances, OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances, BangPatterns #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | A usable regular expressions library on top of pcre-light.
module Text.Regex.PCRE.Heavy (
  -- * Matching
  (=~)
, scan
, scanO
  -- * Replacement
, sub
, subO
, gsub
, gsubO
  -- * QuasiQuoter
, re
, mkRegexQQ
  -- * Types and stuff from pcre-light
, Regex
, PCREOption
, PCRE.compileM
  -- * Advanced raw stuff
, rawMatch
, rawSub
) where

import           Language.Haskell.TH hiding (match)
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax
import qualified Text.Regex.PCRE.Light as PCRE
import           Text.Regex.PCRE.Light.Base
import           Control.Applicative ((<$>))
import           Data.Maybe (isJust, fromMaybe)
import           Data.List (unfoldr)
import           Data.Stringable
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Internal as BS
import           System.IO.Unsafe (unsafePerformIO)
import           Foreign

substr :: BS.ByteString -> (Int, Int) -> BS.ByteString
substr s (f, t) = BS.take (t - f) . BS.drop f $ s

behead :: [a] -> (a, [a])
behead (h:t) = (h, t)
behead [] = error "no head to behead"

reMatch :: Stringable a => Regex -> a -> Bool
reMatch r s = isJust $ PCRE.match r (toByteString s) []

-- | Checks whether a string matches a regex.
--
-- >>> :set -XQuasiQuotes
-- >>> "https://unrelenting.technology" =~ [re|^http.*|]
-- True
(=~) :: Stringable a => a -> Regex -> Bool
(=~) = flip reMatch

-- | Does raw PCRE matching (you probably shouldn't use this directly).
-- 
-- >>> :set -XOverloadedStrings
-- >>> rawMatch [re|\w{2}|] "a a ab abc ba" 0 []
-- Just [(4,6)]
-- >>> rawMatch [re|\w{2}|] "a a ab abc ba" 6 []
-- Just [(7,9)]
-- >>> rawMatch [re|(\w)(\w)|] "a a ab abc ba" 0 []
-- Just [(4,6),(4,5),(5,6)]
rawMatch :: Regex -> BS.ByteString -> Int -> [PCREExecOption] -> Maybe [(Int, Int)]
rawMatch r@(Regex pcreFp _) s offset opts = unsafePerformIO $ do
  withForeignPtr pcreFp $ \pcrePtr -> do
    let nCapt = PCRE.captureCount r
        ovecSize = (nCapt + 1) * 3
        ovecBytes = ovecSize * size_of_cint
    allocaBytes ovecBytes $ \ovec -> do
      let (strFp, off, len) = BS.toForeignPtr s
      withForeignPtr strFp $ \strPtr -> do
        results <- c_pcre_exec pcrePtr nullPtr (strPtr `plusPtr` off) (fromIntegral len) (fromIntegral offset)
                               (combineExecOptions opts) ovec (fromIntegral ovecSize)
        if results < 0 then return Nothing
        else
          let loop n o acc =
                if n == results then return $ Just $ reverse acc
                else do
                  i <- peekElemOff ovec $! o
                  j <- peekElemOff ovec (o + 1)
                  loop (n + 1) (o + 2) ((fromIntegral i, fromIntegral j) : acc)
          in loop 0 0 []

-- | Searches the string for all matches of a given regex.
--
-- >>> scan [re|\s*entry (\d+) (\w+)\s*&?|] " entry 1 hello  &entry 2 hi"
-- [(" entry 1 hello  &",["1","hello"]),("entry 2 hi",["2","hi"])]
--
-- It is lazy! If you only need the first match, just apply 'head' (or
-- 'headMay' from the 'safe' package) -- no extra work will be performed!
--
-- >>> head $ scan [re|\s*entry (\d+) (\w+)\s*&?|] " entry 1 hello  &entry 2 hi"
-- (" entry 1 hello  &",["1","hello"])
scan :: (Stringable a) => Regex -> a -> [(a, [a])]
scan r s = scanO r [] s

-- | Exactly like 'scan', but passes runtime options to PCRE.
scanO :: (Stringable a) => Regex -> [PCREExecOption] -> a -> [(a, [a])]
scanO r opts s = map behead $ map fromByteString <$> unfoldr nextMatch 0
  where str = toByteString s
        nextMatch offset =
          case rawMatch r str offset opts of
            Nothing -> Nothing
            Just [] -> Nothing
            Just ms -> Just (map (substr str) ms, maximum $ map snd ms)

class RegexReplacement a where
  performReplacement :: BS.ByteString -> [BS.ByteString] -> a -> BS.ByteString

instance Stringable a => RegexReplacement a where
  performReplacement _ _ to = toByteString to

instance Stringable a => RegexReplacement (a -> [a] -> a) where
  performReplacement from groups replacer = toByteString $ replacer (fromByteString from) (map fromByteString groups)

instance Stringable a => RegexReplacement (a -> a) where
  performReplacement from _ replacer = toByteString $ replacer (fromByteString from)

instance Stringable a => RegexReplacement ([a] -> a) where
  performReplacement _ groups replacer = toByteString $ replacer (map fromByteString groups)

rawSub :: RegexReplacement r => Regex -> r -> BS.ByteString -> Int -> [PCREExecOption] -> Maybe (BS.ByteString, Int)
rawSub r t s offset opts =
  case rawMatch r s offset opts of
    Just ((begin, end):groups) ->
      Just (BS.concat [ substr s (0, begin)
                      , performReplacement (substr s (begin, end)) (map (substr s) groups) t
                      , substr s (end, BS.length s)], end)
    _ -> Nothing

-- | Replaces the first occurence of a given regex.
--
-- >>> sub [re|thing|] "world" "Hello, thing thing" :: String
-- "Hello, world thing"
--
-- >>> sub [re|a|] "b" "c" :: String
-- "c"
--
-- You can use functions!
-- A function of Stringable gets the full match.
-- A function of [Stringable] gets the groups.
-- A function of Stringable -> [Stringable] gets both.
--
-- >>> sub [re|%(\d+)(\w+)|] (\(d:w:_) -> "{" ++ d ++ " of " ++ w ++ "}" :: String) "Hello, %20thing" :: String
-- "Hello, {20 of thing}"
sub :: (Stringable a, RegexReplacement r) => Regex -> r -> a -> a
sub r t s = subO r [] t s

-- | Exactly like 'sub', but passes runtime options to PCRE.
subO :: (Stringable a, RegexReplacement r) => Regex -> [PCREExecOption] -> r -> a -> a
subO r opts t s = fromMaybe s $ fromByteString <$> fst <$> rawSub r t (toByteString s) 0 opts

-- | Replaces all occurences of a given regex.
--
-- See 'sub' for more documentation.
--
-- >>> gsub [re|thing|] "world" "Hello, thing thing" :: String
-- "Hello, world world"
gsub :: (Stringable a, RegexReplacement r) => Regex -> r -> a -> a
gsub r t s = gsubO r [] t s

-- | Exactly like 'gsub', but passes runtime options to PCRE.
gsubO :: (Stringable a, RegexReplacement r) => Regex -> [PCREExecOption] -> r -> a -> a
gsubO r opts t s = fromByteString $ loop 0 str
  where str = toByteString s
        loop offset acc =
          case rawSub r t acc offset opts of
            Just (result, newOffset) -> loop newOffset result
            _ -> acc

instance Lift PCREOption where
  -- well, the constructor isn't exported, but at least it implements Read/Show :D
  lift o = let o' = show o in [| read o' :: PCREOption |]

quoteExpRegex :: [PCREOption] -> String -> ExpQ
quoteExpRegex opts txt = [| PCRE.compile (toByteString (txt :: String)) opts |]
  where !_ = PCRE.compile (toByteString txt) opts -- check at compile time

-- | Returns a QuasiQuoter like 're', but with given PCRE options.
mkRegexQQ :: [PCREOption] -> QuasiQuoter
mkRegexQQ opts = QuasiQuoter
  { quoteExp  = quoteExpRegex opts
  , quotePat  = undefined
  , quoteType = undefined
  , quoteDec  = undefined }

-- | A QuasiQuoter for regular expressions that does a compile time check.
re :: QuasiQuoter
re = mkRegexQQ [utf8]
