-- |
module Main where

import Test.Framework (defaultMain)
import UI.HydraPrint.NCurses (testSuite)

main :: IO ()
main = defaultMain [testSuite]
