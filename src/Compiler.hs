module Main where

import System.Environment
import FrontEnd.Parser
import FrontEnd.SemanticAnalyzer

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> fail "No specified file to compile"
    1 -> compile $ head args
    otherwise -> fail "Multiple file compilation is not supported"
  return ()

compile file = do
  ast <- parseFile file
  analyzeAST ast
  return ()
