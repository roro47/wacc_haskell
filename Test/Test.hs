module Main where

import Test.TestValid as Valid
import Test.TestInvalidSyntax as InvalidSyntax
import Test.TestSyntaxUnit as SynUnit

main :: IO ()
main = do
  SynUnit.test
  Valid.test
  InvalidSyntax.test
