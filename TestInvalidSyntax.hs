module TestInvalidSyntax where

import Text.ParserCombinators.Parsec
import Test.Hspec

import TestUtil as Util
import Parser

testSyntaxErr file =
  do
    program <- readFile file
    case parse parseProgramF "" program of
      Left _ -> return ()
      Right _ -> expectationFailure err
  where err = "successfully parse a syntactically incorrect wacc program"


main = hspec $ do
       describe "test invalid (syntax) :" $ do
         waccs <- runIO (getWacc "invalid/syntaxErr")
         mapM_ (\wacc -> it ("file: " ++ wacc) (testSyntaxErr wacc)) waccs
