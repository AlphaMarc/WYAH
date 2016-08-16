module Main where

import           ParserTest
import           Test.QuickCheck

main :: IO ()
main = quickCheck prop_ParseInt
