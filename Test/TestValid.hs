module Test.TestValid where

import Text.ParserCombinators.Parsec
import Test.Hspec

import Test.TestUtil
import FrontEnd.Parser
  
testValidParse file = do
                 program <- readFile file
                 case parse parseProgramF "" program of
                   Left e -> print e >> expectationFailure ("parse error: at line " ++ line e ++ " with file " ++ file) 
                   Right _ -> return ()

test = hspec $ do
       describe "test valid : check that all wacc program " $ do
         waccs <- runIO (getWacc "Test/valid")
         mapM_ (\wacc -> it ("file: " ++ wacc) (testValidParse wacc)) waccs
