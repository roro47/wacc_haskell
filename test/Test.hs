module Main where

import TestValid as Valid
import TestInvalidSyntax as InvalidSyntax
import TestInvalidSemantic as InvalidSemantic
import TestSyntaxUnit as SynUnit
import TestSemanticUnit as SemUnit

main :: IO ()
main = do
  SynUnit.test
  SemUnit.test
  Valid.test
  InvalidSyntax.test
  InvalidSemantic.test
