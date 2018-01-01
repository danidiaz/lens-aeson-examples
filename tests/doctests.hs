module Main where

import Test.DocTest

main :: IO ()
main = doctest [ "src/Data/Aeson/Lens/Examples.hs" ]

