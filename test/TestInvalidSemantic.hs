module TestInvalidSemantic where

import Test.Hspec
import Text.ParserCombinators.Parsec
import Control.Monad.State
import Data.HashMap as HashMap hiding (map)

import FrontEnd.Parser
import FrontEnd.SemanticAnalyzer
import TestUtil

testSemanticErr file =
  do
    program <- readFile file
    case parse parseProgramF "" program of
      Left syntaxErr -> expectationFailure err1
      Right p -> case evalStateT (analyzeProgramF p) (([], HashMap.empty), Main) of
                   Left semanticErr -> 
                     appendFile "output.txt" (file ++ ":\n" ++ semanticErr ++ "\n") >> return ()
                   Right p' -> expectationFailure err2
 where err1 =  "the wacc program should be syntactically correct"
       err2 = "pass semantic check, but should be semantically incorrect"

test = hspec $ do
       describe "test invalid (semantic): " $ do
         waccs <- runIO (getWacc "test/invalid/semanticErr")
         mapM_ (\wacc -> it ("file:" ++ wacc) (testSemanticErr wacc)) waccs
