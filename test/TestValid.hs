module TestValid where

import Control.Monad.State
import Data.HashMap as HashMap hiding (map)
import Text.ParserCombinators.Parsec
import Test.Hspec

import TestUtil
import FrontEnd.Parser
import FrontEnd.SemanticAnalyzer  

testValid file =
  do
    program <- readFile file
    case parse parseProgramF "" program of
      Left syntaxErr -> expectationFailure err1
      Right p -> case evalStateT (analyzeProgramF p) (([], HashMap.empty), Main) of
                   Left semanticErr -> expectationFailure (err2  ++ " and error is : \n"++ semanticErr ++ "\n")
                   Right p' -> return ()
 where err1 =  "the wacc program should be syntactically correct"
       err2 = "the wacc program should be semantically correct"


test = hspec $ do
       describe "test valid : check that all wacc program " $ do
         waccs <- runIO (getWacc "test/valid")
         mapM_ (\wacc -> it ("file: " ++ wacc) (testValid wacc)) waccs
