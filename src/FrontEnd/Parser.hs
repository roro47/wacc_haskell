module FrontEnd.Parser where

import System.IO
import System.Exit
import Data.List as List
import Control.Monad
import Control.Monad.Except
import qualified Control.Applicative as App
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Pos

import FrontEnd.Lexer
import FrontEnd.AST


parseProgramF :: Parser (ProgramF ())
parseProgramF = do
  whiteSpace
  pos <- getPosition
  reserved "begin"
  fs <- many (try parseFuncF)
  stat <- parseStatListF
  reserved "end"
  eof
  return $ Ann (Program fs stat) (pos, None)

parseFuncF :: Parser (FuncF ())
parseFuncF = do
  whiteSpace
  pos <- getPosition
  t <- parseType
  ident <- parseIdentF
  ps <- parens (commaSep parseParamF)
  reserved "is"
  stat <- parseStatListF
  checkReturnExit stat
  reserved "end"
  return $ Ann (Func t ident ps stat) (pos, None)
    where checkReturnExit (Ann (StatList stats) (pos, None))
            = case lastStat of
                Return _ -> return ()
                Exit _ -> return ()
                If _ stat1 stat2 ->
                      checkReturnExit stat1 >>
                      checkReturnExit stat2
                While _ stat -> checkReturnExit stat
                Subroutine stat -> checkReturnExit stat
                otherwise -> fail "Expected return or junk after return"
           where Ann lastStat _ = last stats


parseParamF :: Parser (ParamF ())
parseParamF = do
  whiteSpace
  pos <- getPosition
  t <- parseType
  ident <- parseIdentF
  return $ Ann (Param t ident) (pos, None)

parseType :: Parser Type
parseType =
  foldl (<|>) App.empty $ map (\p -> try p) [parseArrayType, parseBaseType, parsePairType]

parseArrayType :: Parser Type
parseArrayType = do
  t <- (parseBaseType <|> parsePairType)
  rs <- many (try $ reserved "[]")
  return $ foldl (\acc x -> TArray acc) t rs

parseBaseType :: Parser Type
parseBaseType = do
  ((reserved "int" >> return TInt)
   <|> (reserved "bool" >> return TBool)
   <|> (reserved "char" >> return TChar)
   <|> (reserved "string" >> return TStr))

parsePairType :: Parser Type
parsePairType = do
  string "pair"
  pair <- (parens $ parsePairElemTypeF >>= \t1 ->
                   comma >>
                   parsePairElemTypeF >>= \t2 ->
                   return $ TPair t1 t2)
  return $ pair
  where parsePairElemTypeF :: Parser Type
        parsePairElemTypeF = try parseArrayType
                         <|> try parseBaseType
                         <|> (string "pair" >>
                              return TAny)

parseStatListF :: Parser (StatListF ())
parseStatListF = do
  whiteSpace
  pos <- getPosition
  stat <- parseStatF
  (try (semi >>
     parseStatListF >>= \(Ann (StatList rest) _) ->
     return $ Ann (StatList (stat:rest)) (pos, None))
     <|> (return $ Ann (StatList [stat]) (pos, None)))


parseStatF :: Parser (StatF ())
parseStatF = whiteSpace >>
             getPosition >>= \pos ->
           ( try parseDeclareStat
         <|> try parseAssignStat
         <|> parseFuncAppStat
         <|> parseReturnStat
         <|> parseExitStat
         <|> parseIfStat
         <|> parseWhileStat
         <|> parseSubroutineStat ) >>= \stat ->
             return $ Ann stat (pos, None)

parseFuncAppStat :: Parser (Stat ())
parseFuncAppStat = do
  pos <- getPosition
  let { ann = (pos, None) }
  (f, expr) <- foldl (<|>) App.empty $ map (\f -> try (parseFunc f)) builtInFunc
  return $ FuncStat (Ann (FuncApp (Ann (Ident f) ann) expr) ann)
    where parseFunc (f, (TFunc _ _ returnT)) =
           reserved f >>
           if returnT /= Void
           then fail "execute non void function in stat"
           else case f of
                  "skip" -> return (f, [])
                  "read" -> parseAssignLHSF >>= \e -> return (f, [e])
                  otherwise -> parseExprF >>= \e -> return (f, [e])
          parseFunc _ = fail "builtIn symbol type is not function"

parseAssignLHSF :: Parser (ExprF ())
parseAssignLHSF = do
  whiteSpace
  pos <- getPosition
  lhs <- foldl (<|>) App.empty $ map try lhsList
  return $ Ann lhs (pos, None)
    where lhsList = [parsePairElem, parseArrayElem,
                    parseIdentF >>= \id -> return $ IdentExpr id]

parsePairElem :: Parser (Expr ())
parsePairElem = do
  pos <- getPosition
  let { ann = (pos, None) } in do {
        parsePairElem' "fst" ann
    <|> parsePairElem' "snd" ann}
    where
      parsePairElem' str ann =
        reserved str >>
        parseExprF >>= \expr ->
        return $  FuncExpr (Ann (FuncApp (Ann (Ident str) ann) [expr]) ann)

parseArrayElem :: Parser (Expr ())
parseArrayElem = do
  i <- parseIdentF
  exprs <- many1 (try parseIndex)
  return $ ArrayElem i exprs
           where parseIndex :: Parser (ExprF ())
                 parseIndex = do
                   expr <- brackets parseExprF
                   return expr

parseIdentF :: Parser (IdentF ())
parseIdentF = do
  pos <- getPosition
  i <- ident
  return $ Ann (Ident i) (pos, None)

parseDeclareStat :: Parser (Stat ())
parseDeclareStat = do
  t <- parseType
  ident <- parseIdentF
  reservedOp "="
  rhs <- parseAssignRHSF
  return $ Declare t ident rhs

parseAssignStat :: Parser (Stat ())
parseAssignStat = do
  lhs <- parseAssignLHSF
  reservedOp "="
  rhs <- parseAssignRHSF
  return $ Assign lhs rhs

parseReturnStat :: Parser (Stat ())
parseReturnStat = do
  reserved "return"
  expr <- parseExprF
  return $ Return expr

parseExitStat :: Parser (Stat ())
parseExitStat = do
  reserved "exit"
  expr <- parseExprF
  return $ Exit expr

parseIfStat :: Parser (Stat ())
parseIfStat = do
  reserved "if"
  expr <- parseExprF
  reserved "then"
  stat1 <- parseStatListF
  reserved "else"
  stat2 <- parseStatListF
  reserved "fi"
  return $ If expr stat1 stat2

parseWhileStat :: Parser (Stat ())
parseWhileStat = do
  reserved "while"
  expr <- parseExprF
  reserved "do"
  stat <- parseStatListF
  reserved "done"
  return $ While expr stat

parseSubroutineStat:: Parser (Stat ())
parseSubroutineStat = do
  reserved "begin"
  stat <- parseStatListF
  reserved "end"
  return $ Subroutine stat

parseAssignRHSF :: Parser (ExprF ())
parseAssignRHSF = do
  pos <- getPosition
  (foldl (<|>) App.empty rhsList >>= \expr ->
    return $ Ann expr (pos, None))
  <|> parseExprF
  where rhsList = [parseArrayLiter, parseNewPair,
                   parsePairElem,
                   parseCallF >>= \expr -> return $ FuncExpr expr]


parseArrayLiter :: Parser (Expr ())
parseArrayLiter = do { es <- brackets (commaSep parseExprF); return $ ArrayLiter es}

parseNewPair :: Parser (Expr ())
parseNewPair = do
  pos <- getPosition
  let { ann = (pos, None); id = Ann (Ident "newpair") ann }
  reserved "newpair"
  newpair <- parens (parseExprF >>= \expr1 ->
    comma >>
    parseExprF >>= \expr2 ->
    return $ FuncExpr (Ann (FuncApp id [expr1,expr2]) ann))
  return $ newpair

parseCallF :: Parser (FuncAppF ())
parseCallF = do
  pos <- getPosition
  reserved "call"
  ident <- parseIdentF
  args <- parens (commaSep parseExprF)
  return $ Ann (FuncApp ident args) (pos, None)

parseExprF :: Parser (ExprF ())
parseExprF = do
  whiteSpace
  expr <- buildExpressionParser table term
  check expr
  return $ expr
     where check (Ann (FuncExpr (Ann (FuncApp id exprs) _)) _)
            = case op of
                "#pos"     -> mapM_ checkIsStringLiter exprs >>
                             mapM_ (checkOverFlow (2^31 -1)) exprs
                "#neg"     -> mapM_ (checkOverFlow (2^31)) exprs
                otherwise -> mapM_ check exprs

            where (Ann (Ident op) _) = id

           check (Ann (BracketExpr e) _) = check e
           check (Ann (ArrayElem _ es) _)
             = mapM_ check es
           check e@(Ann (IntLiter i) _) = checkOverFlow (2^31 -1) e
           check _ = return ()

           checkIsStringLiter (Ann (StringLiter _) _) =
             fail "string concatination not supported"
           checkIsStringLiter _ = return ()

           checkOverFlow limit (Ann e _) =
             case e of
               IntLiter i -> if i > limit then fail "Int Overflow" else return ()
               otherwise -> return ()

genFuncAppF :: String -> SourcePos -> [ExprF ()] -> (FuncAppF ())
genFuncAppF fName pos expr =
  Ann (FuncApp (Ann (Ident fName) (pos, None)) expr) (pos, None)

table = [ [unary symbol "+" (genFuncAppF "#pos"),
           unary symbol "-" (genFuncAppF "#neg"),
           unary symbol"!" (genFuncAppF "!"),
           unary reserved "len" (genFuncAppF "len"),
           unary reserved "ord" (genFuncAppF "ord"),
           unary reserved "chr" (genFuncAppF "chr")],
          [binary "*" (genFuncAppF "*") AssocLeft,
           binary "/" (genFuncAppF "/") AssocLeft,
           binary "%" (genFuncAppF "%") AssocLeft],
          [binary "+" (genFuncAppF "+") AssocLeft,
           binary "-" (genFuncAppF "-") AssocLeft],
          [binary ">=" (genFuncAppF ">=") AssocLeft,
           binary ">" (genFuncAppF ">") AssocLeft,
           binary "<=" (genFuncAppF "<=") AssocLeft,
           binary "<" (genFuncAppF "<") AssocLeft],
          [binary "==" (genFuncAppF "==") AssocLeft,
           binary "!=" (genFuncAppF "!=") AssocLeft],
          [binary "&&" (genFuncAppF "&&") AssocLeft],
          [binary "||" (genFuncAppF "||") AssocLeft]
        ]

toFuncExpr funcApp@(Ann _ ann) = Ann (FuncExpr funcApp) ann

unary operation n f =
  Prefix . chainl1 (try $ whiteSpace >>
                          getPosition >>= \pos ->
                          operation n >>
                          (return $ (\e -> toFuncExpr $ f pos [e]))) $ return (.)

binary n f assoc =
  Infix (try $ whiteSpace >>
               getPosition >>= \pos ->
               symbol n >>= \_ ->
               return $ (\e1 e2 -> toFuncExpr $ f pos [e1, e2])) assoc

term :: Parser (ExprF ())
term = whiteSpace >>
       getPosition >>= \pos ->
      (try parseIntLiter
       <|> try parseBoolLiter
       <|> try parseCharLiter
       <|> try parseStringLiter
       <|> try parsePairLiter
       <|> try parseArrayElem
       <|> try (parseIdentF >>= \id -> return $ IdentExpr id)
       <|> try parseBracketExpr) >>= \expr ->
       return $ Ann expr (pos, None)

parseBracketExpr :: Parser (Expr ())
parseBracketExpr = do { exprF <- parens parseExprF; return $ BracketExpr exprF}

parseIntLiter :: Parser (Expr ())
parseIntLiter = do { x <- integer ; return $ IntLiter x}

parseBoolLiter :: Parser (Expr ())
parseBoolLiter = do
  (do {reserved "true";return $ BoolLiter True}
    <|> (do {reserved "false"; return $ BoolLiter False}))

parseCharLiter :: Parser (Expr ())
parseCharLiter = (try catchWrongEscape >> fail "wrong escape")
             <|> (do { c <- charLiteral; return $ CharLiter c })
     where catchWrongEscape = do { char '\''; char '\"'; char '\''; return '\"'}

parseStringLiter :: Parser (Expr ())
parseStringLiter = do { s <- stringLiteral; return $ StringLiter s }

parsePairLiter :: Parser (Expr ())
parsePairLiter = do { reserved "null" ; return $ Null}

parseFile :: String -> IO (ProgramF ())
parseFile file = do
  program <- readFile file
  case parse parseProgramF "" program of
    Left e -> putStrLn "#syntax_error#" >>
              exitWith (ExitFailure 100)
    Right r -> return r
    where line = \e -> show $ sourceLine $ errorPos e
          col = \e -> show $ sourceLine $ errorPos e
          name = \e -> show $ sourceName $ errorPos e
