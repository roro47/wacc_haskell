module Main where

import Test.TestValid as Valid
import Test.TestInvalidSyntax as InvalidSyntax
import Test.TestSyntaxUnit as SynUnit
--import Test.TestSemanticUnit as SemUnit

main :: IO ()
main = do
  SynUnit.test
  --SemUnit.test
  Valid.test
  InvalidSyntax.test
