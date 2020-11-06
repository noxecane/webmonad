module Prelude.X where

import Data.Char (isUpper, toLower, toUpper)
import Data.List (intercalate)
import Data.List.Split (keepDelimsL, split, whenElt)

capitalise :: String -> String
capitalise [] = []
capitalise (c : cs) = toUpper c : cs

uncapitalise :: String -> String
uncapitalise [] = []
uncapitalise (x : xs) = toLower x : xs

unprefix :: String -> String -> String
unprefix p = drop (length p)

dashify :: String -> String
dashify = intercalate "_" . map toLowerStr . splitWhen isUpper . uncapitalise
  where
    toLowerStr = map toLower
    splitWhen = split . keepDelimsL . whenElt
