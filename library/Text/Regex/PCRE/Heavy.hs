{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances, BangPatterns #-}

module Text.Regex.PCRE.Heavy (
  (=~)
, re
, Regex
, PCREOption
) where

import           Language.Haskell.TH hiding (match)
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax
import qualified Text.Regex.PCRE.Light as PCRE
import           Text.Regex.PCRE.Light (Regex, PCREOption)
import           Control.Applicative ((<$>))
import           Data.Maybe (isJust)
import           Data.Stringable
import qualified Data.ByteString.Char8 as BS

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
--
-- if casts to bool automatically.
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
