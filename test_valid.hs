module TestValid where


import System.Directory
import Data.List
import Parser


filter' = filter (\s -> s /= "." && s /= "..")

main = getCurrentDirectory >>= \curr_d ->
       getDirectoryContents curr_d >>= \contents ->
       if not $ elem "valid" contents
       then putStrLn "directory with valid wacc programs not found"
       else getDirectoryContents (curr_d  ++ "/valid") >>= \vs ->
            visitValid (filter' vs) (curr_d ++ "/valid")

visitValid [] parent = putStrLn ""
visitValid (v:vs) parent
  = doesDirectoryExist path >>= \isD ->
    if isD
    then getDirectoryContents path >>= \vs1 ->
         visitValid (filter' vs1) path >>= \_ ->
         visitValid vs parent
    else testFile path >>= \_ ->
         visitValid vs parent
  where path = parent ++ "/" ++ v


testFile file
  | isSuffixOf ".wacc" file = parseFile file >>= \ast ->
                              putStrLn $ file ++ ":\n\n" ++ show ast ++ "\n"
  | otherwise = putStrLn $ ""
