-- 1. Utility for writing assembly to files
-- 2. write a script to allow emulation
-- 3. assume the assembly is in [String]

module BackEnd.Todo where

-- import FrontEnd.AST
-- import FrontEnd.SemanticAnalyzer
import System.Environment

indent :: String -> String
indent s = "\t\t" ++ s

nextline :: String -> String
nextline s = s ++ "\n"

writeCode :: String -> [String] -> IO()
writeCode filename assembly = do
  writeFile outputname (concat assembly)
    where outputname = (take (length filename - 5) filename) ++ ".s"

sampleExit = map nextline $ [".text", "", ".global main", "main:"] ++
              (map indent ["PUSH {lr}", "LDR r4, =-1", "MOV r0, r4", "BL exit",
              "LDR r0, =0", "POP {pc}", ".ltorg", ""])
samplePrint = map nextline $ [".data", "", "msg_0:"] ++ (map indent [".word 13",
              ".ascii \"Hello World!\\n\""]) ++ ["msg_1:"] ++ (map indent [".word 5",
              ".ascii \"%.*s\\0\""]) ++ (["", ".text", "", ".global main", "main:"])
              ++ (map indent ["PUSH {lr}", "LDR r4, =msg_0", "MOV r0, r4", "BL p_print_string"
              , "LDR r0, =0", "POP {pc}", ".ltorg"]) ++ ["p_print_string:"] ++
              (map indent ["PUSH {lr}", "LDR r1, [r0]", "ADD r2, r0, #4", "LDR r0, =msg_1"
              , "ADD r0, r0, #4", "BL printf", "MOV r0, #0", "BL fflush", "POP {pc}"])

main = do
  args <- getArgs
  case args of
    [file] -> do
      writeCode file samplePrint --change this to actual output
    _ -> fail ("File does not exist\n")
