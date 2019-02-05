module Test.TestUtil where

import qualified System.Directory as SysDir
import Control.Monad
import Data.List
import Text.ParserCombinators.Parsec


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

