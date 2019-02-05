module Main where

import Test.TestValid as Valid
import Test.TestInvalidSyntax as InvalidSyntax

main = do
  Valid.test
  InvalidSyntax.test
