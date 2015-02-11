# pcre-heavy [![Hackage](https://img.shields.io/hackage/v/pcre-heavy.svg?style=flat)](https://hackage.haskell.org/package/pcre-heavy) [![ISC License](https://img.shields.io/badge/license-ISC-red.svg?style=flat)](https://tldrlegal.com/license/-isc-license)

*Finally!* A Haskell regular expressions library that does not suck.

- based on [pcre-light], none of that regex-compat-pluggable-backend stuff
- takes and returns [Stringables] everywhere, use ANY STRING TYPE (String, ByteString, LByteString, Text, LText, FilePath) -- but you need a bit more type annotations than usual
- a QuasiQuoter for regexps that does compile time checking (BTW, [vim2hs] has correct syntax highlighting for that!)
- **SEARCHES FOR MULTIPLE MATCHES! DOES REPLACEMENT!**

[pcre-light]: https://hackage.haskell.org/package/pcre-light
[Stringables]: https://hackage.haskell.org/package/stringable
[vim2hs]: https://github.com/dag/vim2hs#quasi-quoting

## Usage

```haskell
{-# LANGUAGE QuasiQuotes #-}
import           Text.Regex.PCRE.Heavy
```

### Matching

```haskell
>>> "https://unrelenting.technology" =~ [re|^http.*|] :: Bool
True
```

In an `if`, you don't even need the annotation:

```haskell
if "https://unrelenting.technology" =~ [re|^http.*|] then "YEP" else "NOPE"
```

Extracting matches (You can use any string type, not just String)

```haskell
>>> "https://unrelenting.technology" =~ [re|^https?://([^\.]+)\..*|] :: Maybe [String]
Just ["https://unrelenting.technology","unrelenting"]
```

`scan` returns all matches (search)

```haskell
>>> scan [re|\s*entry (\d+) (\w+)\s*&?|] " entry 1 hello  &entry 2 hi" :: [[String]]
[
  [" entry 1 hello  &", "1", "hello"]
, ["entry 2 hi",        "2", "hi"]
]
```

### Replacement

`sub` replaces the first match, `gsub` replaces all matches.

```haskell
-- You can use a Stringable type as the replacement...
>>> gsub [re|\d+|] "!!!NUMBER!!!" "Copyright (c) 2015 The 000 Group"
"Copyright (c) !!!NUMBER!!! The !!!NUMBER!!! Group"

-- or a (Stringable a => [a] -> a) function -- that will get the groups...
>>> gsub [re|%(\d+)(\w+)|] (\(d:w:_) -> "{" ++ d ++ " of " ++ w ++ "}" :: String) "Hello, %20thing"
"Hello, {20 of thing}"

-- or a (Stringable a => a -> a) function -- that will get the full match...
>>> gsub [re|-\w+|] (\x -> "+" ++ (reverse $ drop 1 x) :: String) "hello -world"
"hello +dlrow"

-- or a (Stringable a => a -> [a] -> a) function.
-- That will get both the full match and the groups.
-- I have no idea why you would want to use that, but that's there :-)
```

### Options

You can pass `pcre-light` options like this:

```haskell
>>> let myRe = mkRegexQQ [multiline, utf8, ungreedy]
>>> scanO [myRe|\s*entry (\d+) (\w+)\s*&?|] [exec_no_utf8_check] " entry 1 hello  &entry 2 hi" :: [[String]]
>>> gsubO [myRe|\d+|] [exec_notempty] "!!!NUMBER!!!" "Copyright (c) 2015 The 000 Group"
```

## License

Copyright 2015 Greg V <greg@unrelenting.technology>  
Available under the ISC license, see the `COPYING` file
