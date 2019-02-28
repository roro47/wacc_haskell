module Main where

import System.Environment
import System.FilePath
import FrontEnd.Parser
import FrontEnd.SemanticAnalyzer
import BackEnd.CodeGen

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> fail "No specified file to compile"
    1 -> compile $ head args
    otherwise -> fail "Multiple file compilation is not supported"
  return ()

compile path = do
  let (_, file) = splitFileName path
      assemFile = replaceExtensions file ".s"
  ast <- parseFile path
  ast' <- analyzeAST ast
  code <- codeGen ast'
  writeFile assemFile code
