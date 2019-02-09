module Test.TestInvalidSyntax where

import Text.ParserCombinators.Parsec
import Test.Hspec

import Test.TestUtil as Util
import FrontEnd.Parser

testSyntaxErr file =
  do
    program <- readFile file
    case parse parseProgramF "" program of
      Left e -> print e >> return ()
      Right _ -> expectationFailure err
  where err = "successfully parse a syntactically incorrect wacc program"


test = hspec $ do
       describe "test invalid (syntax) :" $ do
         waccs <- runIO (getWacc "Test/invalid/syntaxErr")
         mapM_ (\wacc -> it ("file: " ++ wacc) (testSyntaxErr wacc)) waccs
