# pcre-heavy [![Hackage](https://img.shields.io/hackage/v/pcre-heavy.svg?style=flat)](https://hackage.haskell.org/package/pcre-heavy) [![ISC License](https://img.shields.io/badge/license-ISC-red.svg?style=flat)](https://tldrlegal.com/license/-isc-license)

**Finally!** A Haskell regular expressions library that does not suck.

- based on [pcre-light], none of that regex-compat-pluggable-backend stuff
- takes [Stringables] everywhere, use ANY STRING TYPE (String, ByteString, LByteString, Text, LText, FilePath)
- a QuasiQuoter for regexps that does compile time checking (BTW, [vim2hs] has correct syntax highlighting for that!)

[pcre-light]: https://hackage.haskell.org/package/pcre-light
[Stringables]: https://hackage.haskell.org/package/stringable
[vim2hs]: https://github.com/dag/vim2hs#quasi-quoting

## Usage

```haskell
{-# LANGUAGE QuasiQuotes #-}
import           Text.Regex.PCRE.Heavy

-- Checking:
let isUrl = "https://unrelenting.technology" =~ [re|^http.*|] :: Bool

-- In an `if`, you don't even need the annotation:
putStrLn $ if "https://unrelenting.technology" =~ [re|^http.*|] then "YEP" else "NOPE"

-- Extracting matches: (Note: You can use any string type, not just String)
let domain = "https://unrelenting.technology" =~ [re|^https?://([^\.]+)\..*|] :: Maybe [String]
-- returns
Just ["https://unrelenting.technology","unrelenting"]

-- Multiple matches (also called search, scan):
let entries = scan [re|\s*entry (\d+) (\w+)\s*&?|] " entry 1 hello  &entry 2 hi" :: [[String]]
-- returns
[
  [" entry 1 hello  &", "1", "hello"]
, ["entry 2 hi",        "2", "hi"]
]
```

## License

Copyright 2015 Greg V <greg@unrelenting.technology>  
Available under the ISC license, see the `COPYING` file
