module TestUtil where

import qualified System.Directory as SysDir
import Control.Monad
import Data.List hiding (insert)
import Text.ParserCombinators.Parsec
import Text.Parsec.Pos
import Text.Parsec.Prim
import FrontEnd.SemanticAnalyzer
import Control.Applicative
import Control.Monad.State
import Control.Monad.Except
import Data.HashMap as HashMap hiding (map, filter)
import FrontEnd.AST

removeDot = filter (\s -> s /= "." && s /= "..")

getDirectoryContents d = SysDir.getDirectoryContents d >>= \contents ->
                         return $ removeDot contents

getWacc dir = getDirectoryContents dir >>= \contents ->
              getWacc' contents dir >>= \files ->
              return $ filter (\f -> isSuffixOf ".wacc" f) files
  where getWacc' [] parent = return []
        getWacc' (wacc:waccs) parent =
          SysDir.doesDirectoryExist path >>= \isDir ->
          if isDir
          then getWacc path >>= \files1 ->
               getWacc' waccs parent >>= \files2 ->
               return $ files1 ++ files2
          else getWacc' waccs parent >>= \files ->
               return $ (path:files)
          where path = parent ++ "/" ++ wacc


line = \e -> show $ sourceLine $ errorPos e
col = \e -> show $ sourceLine $ errorPos e
name = \e -> show $ sourceName $ errorPos e

-- parse a string using a parser
parseUsing :: Show a => String -> Parser(a) -> IO (Maybe String)
parseUsing str par =
   do text  <- toIO str
      case parse par "" text of
        Left e  -> return Nothing
        Right r -> return (Just (show r))

toIO :: a -> IO a
toIO = return

-- fake a analyzer with stack
analyzedummyGeneral :: [(String, Type)] -> a ->(a -> Analyzer (a))-> Analyzer (a)
analyzedummyGeneral stk p a =
  do
    pushScope
    mapM (\(id, funcT) ->
        addSymbol (Ann (Ident id) (dummy_pos, None)) funcT dummy_pos) builtInFunc
    mapM (\(id, funcT) ->
        addSymbol (Ann (Ident id) (dummy_pos, None)) funcT dummy_pos) stk
    a' <- a p
    popScope
    return a'

analyzedummy p a = analyzedummyGeneral [] p a
analyzeplus stk p a = analyzedummyGeneral stk p a

dummy_pos = newPos "test" 0 0

-- analyze a string using a suitable parser and a suitable analyzer
analyzeGeneral :: Show a => String -> Parser(a) -> (a -> Analyzer(a)) -> (a ->(a -> Analyzer (a))-> Analyzer (a)) -> IO (Maybe String)
analyzeGeneral str par ana stack = do
      text  <- toIO str
      case parse par "" text of
        Left e  -> return Nothing
        Right r -> (
            case evalStateT (stack r ana) ([], Main) of
              Left e' ->  return Nothing
              Right r' -> return (Just (show r'))
              )

analyzeUsing :: Show a => String -> Parser(a) -> (a -> Analyzer(a)) -> IO (Maybe String)
analyzeUsing str par ana = analyzeGeneral str par ana analyzedummy

-- addSymbol symbol declareT pos
analyzeUsingPlus :: Show a => String -> Parser(a) -> (a -> Analyzer(a))-> [(String, Type)] -> IO (Maybe String)
analyzeUsingPlus str par ana stk = analyzeGeneral str par ana (analyzeplus stk)
