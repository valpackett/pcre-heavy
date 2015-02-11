{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances, BangPatterns #-}
{-# LANGUAGE UndecidableInstances, ForeignFunctionInterface #-}

module Text.Regex.PCRE.Heavy (
  (=~)
, scan
, re
, Regex
, PCREOption
, rawMatch
) where

import           Language.Haskell.TH hiding (match)
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax
import qualified Text.Regex.PCRE.Light as PCRE
import           Text.Regex.PCRE.Light (Regex, PCREOption)
import           Text.Regex.PCRE.Light.Base
import           Control.Applicative ((<$>))
import           Data.Maybe (isJust)
import           Data.Stringable
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Internal as BS
import           System.IO.Unsafe (unsafePerformIO)
import           Foreign
import           Foreign.Ptr
import           Foreign.C.Types
import           Foreign.C.String
import           Foreign.Storable
import           Foreign.Marshal.Alloc

class RegexResult a where
  fromResult :: Maybe [BS.ByteString] -> a

instance RegexResult (Maybe [BS.ByteString]) where
  fromResult = id

instance Stringable a => RegexResult (Maybe [a]) where
  fromResult x = map fromByteString <$> x

instance RegexResult Bool where
  fromResult = isJust

reMatch :: (Stringable a, RegexResult b) => Regex -> a -> b
reMatch r s = fromResult $ PCRE.match r (toByteString s) []

-- | Matches a string with a regex.
--
-- You can cast the result to Bool or Maybe [Stringable]
-- (where Stringable is anything supported by Data.Stringable).
-- Maybe [Stringable] only represents the first match and its groups.
-- Use scan to find all matches.
--
-- Note: if casts to bool automatically.
--
-- >>> :set -XQuasiQuotes
-- >>> "https://unrelenting.technology" =~ [re|^http.*|] :: Bool
-- True
-- >>> "https://unrelenting.technology" =~ [re|^https?://([^\.]+)\..*|] :: Maybe [String]
-- Just ["https://unrelenting.technology","unrelenting"]
-- >>> if "https://unrelenting.technology" =~ [re|^http.*|] then "YEP" else "NOPE"
-- "YEP"
(=~) :: (Stringable a, RegexResult b) => a -> Regex -> b
(=~) = flip reMatch

-- | Does raw PCRE matching (you probably shouldn't use this directly).
-- 
-- >>> rawMatch [re|\w{2}|] "a a ab abc ba" 0 []
-- Just [(4,6)]
-- >>> rawMatch [re|\w{2}|] "a a ab abc ba" 6 []
-- Just [(7,9)]
-- >>> rawMatch [re|(\w)(\w)|] "a a ab abc ba" 0 []
-- Just [(4,6),(4,5),(5,6)]
rawMatch :: (Stringable a) => Regex -> a -> Int -> [PCREExecOption] -> Maybe [(Int, Int)]
rawMatch r@(Regex pcreFp _) s offset opts = unsafePerformIO $ do
  withForeignPtr pcreFp $ \pcrePtr -> do
    let nCapt = PCRE.captureCount r
        ovecSize = (nCapt + 1) * 3
        ovecBytes = ovecSize * size_of_cint
    allocaBytes ovecBytes $ \ovec -> do
      let (strFp, off, len) = BS.toForeignPtr $ toByteString s
      withForeignPtr strFp $ \strPtr -> do
        r <- c_pcre_exec pcrePtr nullPtr (strPtr `plusPtr` off) (fromIntegral len) (fromIntegral offset)
                         (combineExecOptions opts) ovec (fromIntegral ovecSize)
        if r < 0 then return Nothing
        else
          let loop n o acc =
                if n == r then return $ Just $ reverse acc
                else do
                  i <- peekElemOff ovec $! o
                  j <- peekElemOff ovec (o + 1)
                  loop (n + 1) (o + 2) ((fromIntegral i, fromIntegral j) : acc)
          in loop 0 0 []

-- | Searches the string for all matches of a given regex.
--
-- >>> scan [re|\s*entry (\d+) (\w+)\s*&?|] " entry 1 hello  &entry 2 hi"
-- [[" entry 1 hello  &","1","hello"],["entry 2 hi","2","hi"]]
scan :: (Stringable a) => Regex -> a -> [[a]]
scan r s = map fromByteString <$> loop 0 []
  where str = toByteString s
        loop offset acc =
          case rawMatch r str offset [] of
            Nothing -> reverse acc
            Just [] -> reverse acc
            Just ms -> loop (maximum $ map snd ms) (getMatches ms : acc)
        getMatches ms = map (\(f, t) -> BS.take (t - f) $ BS.drop f str) ms

instance Lift PCREOption where
  lift o = [| o |]

quoteExpRegex :: [PCREOption] -> String -> ExpQ
quoteExpRegex opts txt = [| PCRE.compile (BS.pack txt) opts |]
  where !_ = PCRE.compile (BS.pack txt) opts -- check at compile time

mkRegexQQ :: [PCREOption] -> QuasiQuoter
mkRegexQQ opts = QuasiQuoter
  { quoteExp  = quoteExpRegex opts
  , quotePat  = undefined
  , quoteType = undefined
  , quoteDec  = undefined }

-- | A QuasiQuoter for regular expressions that does a compile time check.
re :: QuasiQuoter
re = mkRegexQQ []
