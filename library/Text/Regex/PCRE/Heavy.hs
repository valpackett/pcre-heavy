{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-unused-binds #-}
{-# LANGUAGE NoImplicitPrelude, UndecidableInstances, FlexibleInstances, FlexibleContexts, BangPatterns #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes, UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface, CPP #-}
#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances #-}
#endif

-- | A usable regular expressions library on top of pcre-light.
module Text.Regex.PCRE.Heavy (
  -- * Matching
  (=~)
, (≈)
, scan
, scanO
, scanRanges
, scanRangesO
  -- * Replacement
, sub
, subO
, gsub
, gsubO
  -- * Splitting
, split
, splitO
  -- * QuasiQuoter
, re
, mkRegexQQ
  -- * Building regexes
, escape
  -- * Types and stuff from pcre-light
, Regex
, PCREOption
, PCRE.compileM
  -- * Advanced raw stuff
, rawMatch
, rawSub
) where

import           Prelude.Compat
import           Language.Haskell.TH hiding (match)
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax
import qualified Text.Regex.PCRE.Light as PCRE
import           Text.Regex.PCRE.Light.Base
import           Data.Maybe (isJust, fromMaybe)
import           Data.List (unfoldr, mapAccumL)
import qualified Data.List.NonEmpty as NE
import           Data.String.Conversions
import           Data.String.Conversions.Monomorphic
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Internal as BS
import           System.IO.Unsafe (unsafePerformIO)
import           Foreign (withForeignPtr, allocaBytes, nullPtr, plusPtr, peekElemOff)

substr ∷ SBS → (Int, Int) → SBS
substr s (f, t) = BS.take (t - f) . BS.drop f $ s

behead ∷ NE.NonEmpty a → (a, [a])
behead l = (NE.head l, NE.tail l)

reMatch ∷ (ConvertibleStrings SBS a, ConvertibleStrings a SBS) ⇒ Regex → a → Bool
reMatch r s = isJust $ PCRE.match r (cs s) []

-- | Checks whether a string matches a regex.
--
-- >>> :set -XQuasiQuotes
-- >>> :set -XFlexibleContexts
-- >>> "https://unrelenting.technology" =~ [re|^http.*|]
-- True
(=~), (≈) ∷ (ConvertibleStrings SBS a, ConvertibleStrings a SBS) ⇒ a → Regex → Bool
(=~) = flip reMatch

-- | Same as =~.
(≈) = (=~)

-- | Does raw PCRE matching (you probably shouldn't use this directly).
--
-- >>> :set -XOverloadedStrings
-- >>> rawMatch [re|\w{2}|] "a a ab abc ba" 0 []
-- Just [(4,6)]
-- >>> rawMatch [re|\w{2}|] "a a ab abc ba" 6 []
-- Just [(7,9)]
-- >>> rawMatch [re|(\w)(\w)|] "a a ab abc ba" 0 []
-- Just [(4,6),(4,5),(5,6)]
rawMatch ∷ Regex → SBS → Int → [PCREExecOption] → Maybe [(Int, Int)]
rawMatch r@(Regex pcreFp _) s offset opts = unsafePerformIO $ do
  withForeignPtr pcreFp $ \pcrePtr → do
    let nCapt = PCRE.captureCount r
        ovecSize = (nCapt + 1) * 3
        ovecBytes = ovecSize * size_of_cint
    allocaBytes ovecBytes $ \ovec → do
      let (strFp, off, len) = BS.toForeignPtr s
      withForeignPtr strFp $ \strPtr → do
        results ← c_pcre_exec pcrePtr nullPtr (strPtr `plusPtr` off) (fromIntegral len) (fromIntegral offset)
                               (combineExecOptions opts) ovec (fromIntegral ovecSize)
        if results < 0 then return Nothing
        else
          let loop n o acc =
                if n == results then return $ Just $ reverse acc
                else do
                  i ← peekElemOff ovec $! o
                  j ← peekElemOff ovec (o + 1)
                  loop (n + 1) (o + 2) ((fromIntegral i, fromIntegral j) : acc)
          in loop 0 0 []

nextMatch ∷ Regex → [PCREExecOption] → SBS → Int → Maybe (NE.NonEmpty (Int, Int), Int)
nextMatch r opts str offset =
  rawMatch r str offset opts >>= NE.nonEmpty >>= \ms → return (ms, maximum $ fmap snd ms)

-- | Searches the string for all matches of a given regex.
--
-- >>> scan [re|\s*entry (\d+) (\w+)\s*&?|] (" entry 1 hello  &entry 2 hi" :: String)
-- [(" entry 1 hello  &",["1","hello"]),("entry 2 hi",["2","hi"])]
--
-- It is lazy! If you only need the first match, just apply 'head' (or
-- 'headMay' from the "safe" library) -- no extra work will be performed!
--
-- >>> head $ scan [re|\s*entry (\d+) (\w+)\s*&?|] (" entry 1 hello  &entry 2 hi" :: String)
-- (" entry 1 hello  &",["1","hello"])
scan ∷ (ConvertibleStrings SBS a, ConvertibleStrings a SBS) ⇒ Regex → a → [(a, [a])]
scan r s = scanO r [] s

-- | Exactly like 'scan', but passes runtime options to PCRE.
scanO ∷ (ConvertibleStrings SBS a, ConvertibleStrings a SBS) ⇒ Regex → [PCREExecOption] → a → [(a, [a])]
scanO r opts s = map behead $ fmap (cs . substr str) <$> unfoldr (nextMatch r opts str) 0
  where str = toSBS s

-- | Searches the string for all matches of a given regex, like 'scan', but
-- returns positions inside of the string.
--
-- >>> scanRanges [re|\s*entry (\d+) (\w+)\s*&?|] (" entry 1 hello  &entry 2 hi" :: String)
-- [((0,17),[(7,8),(9,14)]),((17,27),[(23,24),(25,27)])]
--
-- And just like 'scan', it's lazy.
scanRanges ∷ (ConvertibleStrings SBS a, ConvertibleStrings a SBS) ⇒ Regex → a → [((Int, Int), [(Int, Int)])]
scanRanges r s = scanRangesO r [] s

-- | Exactly like 'scanRanges', but passes runtime options to PCRE.
scanRangesO ∷ (ConvertibleStrings SBS a, ConvertibleStrings a SBS) ⇒ Regex → [PCREExecOption] → a → [((Int, Int), [(Int, Int)])]
scanRangesO r opts s = map behead $ unfoldr (nextMatch r opts str) 0
  where str = toSBS s

class RegexReplacement a where
  performReplacement ∷ SBS → [SBS] → a → SBS

instance {-# OVERLAPPABLE #-} ConvertibleStrings a SBS ⇒ RegexReplacement a where
  performReplacement _ _ to = cs to

instance (ConvertibleStrings SBS a, ConvertibleStrings a SBS) ⇒ RegexReplacement (a → [a] → a) where
  performReplacement from groups replacer = cs $ replacer (cs from) (map cs groups)

instance (ConvertibleStrings SBS a, ConvertibleStrings a SBS) ⇒ RegexReplacement (a → a) where
  performReplacement from _ replacer = cs $ replacer (cs from)

instance (ConvertibleStrings SBS a, ConvertibleStrings a SBS) ⇒ RegexReplacement ([a] → a) where
  performReplacement _ groups replacer = cs $ replacer (map cs groups)

rawSub ∷ RegexReplacement r ⇒ Regex → r → SBS → Int → [PCREExecOption] → Maybe (SBS, Int)
rawSub r t s offset opts =
  case rawMatch r s offset opts of
    Just ((begin, end):groups) →
      let replacement = performReplacement (substr s (begin, end)) (map (substr s) groups) t in
      Just (BS.concat [ substr s (0, begin)
                      , replacement
                      , substr s (end, BS.length s)], begin + BS.length replacement)
    _ → Nothing

-- | Replaces the first occurence of a given regex.
--
-- >>> sub [re|thing|] "world" "Hello, thing thing" :: String
-- "Hello, world thing"
--
-- >>> sub [re|a|] "b" "c" :: String
-- "c"
--
-- >>> sub [re|bad|] "xxxbad" "this is bad, right?" :: String
-- "this is xxxbad, right?"
--
-- You can use functions!
-- A function of ConvertibleStrings SBS gets the full match.
-- A function of [ConvertibleStrings SBS] gets the groups.
-- A function of ConvertibleStrings SBS → [ConvertibleStrings SBS] gets both.
--
-- >>> sub [re|%(\d+)(\w+)|] (\(d:w:_) -> "{" ++ d ++ " of " ++ w ++ "}" :: String) "Hello, %20thing" :: String
-- "Hello, {20 of thing}"
sub ∷ (ConvertibleStrings SBS a, ConvertibleStrings a SBS, RegexReplacement r) ⇒ Regex → r → a → a
sub r t s = subO r [] t s

-- | Exactly like 'sub', but passes runtime options to PCRE.
subO ∷ (ConvertibleStrings SBS a, ConvertibleStrings a SBS, RegexReplacement r) ⇒ Regex → [PCREExecOption] → r → a → a
subO r opts t s = fromMaybe s $ cs <$> fst <$> rawSub r t (cs s) 0 opts

-- | Replaces all occurences of a given regex.
--
-- See 'sub' for more documentation.
--
-- >>> gsub [re|thing|] "world" "Hello, thing thing" :: String
-- "Hello, world world"
--
-- >>> gsub [re||] "" "Hello, world" :: String
-- "Hello, world"
--
-- https://github.com/myfreeweb/pcre-heavy/issues/2
-- >>> gsub [re|good|] "bad" "goodgoodgood" :: String
-- "badbadbad"
--
-- >>> gsub [re|bad|] "xxxbad" "this is bad, right? bad" :: String
-- "this is xxxbad, right? xxxbad"
--
-- >>> gsub [re|a|] "" "aaa" :: String
-- ""
gsub ∷ (ConvertibleStrings SBS a, ConvertibleStrings a SBS, RegexReplacement r) ⇒ Regex → r → a → a
gsub r t s = gsubO r [] t s

-- | Exactly like 'gsub', but passes runtime options to PCRE.
gsubO ∷ (ConvertibleStrings SBS a, ConvertibleStrings a SBS, RegexReplacement r) ⇒ Regex → [PCREExecOption] → r → a → a
gsubO r opts t s = cs $ loop 0 str
  where str = toSBS s
        loop offset acc
          | offset >= l = acc
          | otherwise = case rawSub r t acc offset opts of
            Just (result, newOffset) →
              if newOffset == offset && l == BS.length result
              then acc
              else loop newOffset result
            _ → acc
          where l = BS.length acc

-- | Splits the string using the given regex.
--
-- Is lazy.
--
-- >>> split [re|%(begin|next|end)%|] ("%begin%hello%next%world%end%" :: String)
-- ["","hello","world",""]
--
-- >>> split [re|%(begin|next|end)%|] ("" :: String)
-- [""]
split ∷ (ConvertibleStrings SBS a, ConvertibleStrings a SBS) ⇒ Regex → a → [a]
split r s = splitO r [] s

-- | Exactly like 'split', but passes runtime options to PCRE.
splitO ∷ (ConvertibleStrings SBS a, ConvertibleStrings a SBS) ⇒ Regex → [PCREExecOption] → a → [a]
splitO r opts s = map cs $ map' (substr str) partRanges
  where map' f = foldr ((:) . f) [f (lastL, BS.length str)] -- avoiding the snoc operation
        (lastL, partRanges) = mapAccumL invRange 0 ranges
        invRange acc (xl, xr) = (xr, (acc, xl))
        ranges = map fst $ scanRangesO r opts str
        str = toSBS s

instance Lift PCREOption where
  -- well, the constructor isn't exported, but at least it implements Read/Show :D
  lift o = let o' = show o in [| read o' ∷ PCREOption |]

quoteExpRegex ∷ [PCREOption] → String → ExpQ
quoteExpRegex opts txt = [| PCRE.compile (cs (txt ∷ String)) opts |]
  where !_ = PCRE.compile (cs txt) opts -- check at compile time

-- | Returns a QuasiQuoter like 're', but with given PCRE options.
mkRegexQQ ∷ [PCREOption] → QuasiQuoter
mkRegexQQ opts = QuasiQuoter
  { quoteExp  = quoteExpRegex opts
  , quotePat  = undefined
  , quoteType = undefined
  , quoteDec  = undefined }

-- | A QuasiQuoter for regular expressions that does a compile time check.
re ∷ QuasiQuoter
re = mkRegexQQ [utf8]

-- Metacharacters used in PCRE syntax.  Taken from pcrepattern(3) man
-- page.
pcreMetachars ∷ SBS
pcreMetachars = "\\^$.[|()?*+{"

-- Start and end quote markers in PCRE syntax.
startQuoteMarker, endQuoteMarker ∷ SBS
startQuoteMarker = "\\Q"
endQuoteMarker = "\\E"

-- | Escapes the regex metacharacters in a string.  In other words,
-- given a string, produces a regex that matches just that string (or
-- case variations of that string, if case-insenstive matching is
-- enabled).
--
-- >>> "foo*bar" =~ PCRE.compile (convertString $ escape "foo*bar") []
-- True
escape ∷ (ConvertibleStrings a SBS, ConvertibleStrings SBS a) => a → a
escape = convertString . escapeSBS . convertString
  where escapeSBS s
            -- Handle the special case where \Q...\E doesn't work.
            | endQuoteMarker `BS.isInfixOf` s = BS.concatMap step s
            -- Handle the typical case.
            | otherwise = BS.concat [startQuoteMarker, s, endQuoteMarker]
        step c
            | c `BS.elem` pcreMetachars = BS.pack ['\\', c]
            | otherwise = BS.singleton c
