module FrontEnd.Parser where

import System.IO
import Control.Monad
import Control.Monad.Except
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
             t <- parseTypeF
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
              t <- parseTypeF
              ident <- parseIdentF
              return $ Ann (Param t ident) (pos, None)

parseTypeF :: Parser (TypeF ())
parseTypeF = try parseArrayTypeF
         <|> try parseBaseTypeF
         <|> parsePairTypeF

parseArrayTypeF :: Parser (TypeF ())
parseArrayTypeF = do
                  whiteSpace
                  pos <- getPosition
                  t <- (parseBaseTypeF <|> parsePairTypeF)
                  rs <- many (try $ reserved "[]")
                  return $ foldl (\acc x -> Ann (TArray acc) (pos, None)) t rs

parseBaseTypeF :: Parser (TypeF ())
parseBaseTypeF = do
                 whiteSpace
                 pos <- getPosition
                 ((reserved "int" >> (return $ Ann TInt (pos, None)))
                  <|> (reserved "bool" >> (return $ Ann TBool (pos, None)))
                  <|> (reserved "char" >> (return $ Ann TChar (pos, None)))
                  <|> (reserved "string" >> (return $ Ann TStr (pos, None))))

parsePairTypeF :: Parser (TypeF ())
parsePairTypeF = do
                 whiteSpace
                 pos <- getPosition
                 string "pair"
                 reservedOp "("
                 t1 <- parsePairElemTypeF
                 comma
                 t2 <- parsePairElemTypeF
                 reservedOp ")"
                 return $ Ann (TPair t1 t2) (pos, None)
        where parsePairElemTypeF :: Parser (TypeF ())
              parsePairElemTypeF = try parseArrayTypeF
                               <|> try parseBaseTypeF
                               <|> (getPosition >>= \pos ->
                                    string "pair" >>
                                    (return $ (Ann Any (pos, None))))

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
           ( parseSkipStatF
         <|> try parseDeclareStatF
         <|> try parseAssignStatF
         <|> parseReadStatF
         <|> parseFreeStatF
         <|> parseReturnStatF
         <|> parseExitStatF
         <|> parsePrintStatF
         <|> parsePrintlnStatF
         <|> parseIfStatF
         <|> parseWhileStatF
         <|> parseSubroutineStatF )

parseDeclareStatF :: Parser (StatF ())
parseDeclareStatF = do
                    pos <- getPosition
                    t <- parseTypeF
                    ident <- parseIdentF
                    reservedOp "="
                    rhs <- parseAssignRHSF
                    return $ Ann (Declare t ident rhs) (pos, None)

parseAssignStatF :: Parser (StatF ())
parseAssignStatF = do
                   pos <- getPosition
                   lhs <- parseAssignLHSF
                   reservedOp "="
                   rhs <- parseAssignRHSF
                   return $ Ann (Assign lhs rhs) (pos, None)

parseReadStatF :: Parser (StatF ())
parseReadStatF = do
                 pos <- getPosition
                 reserved "read"
                 lhs <- parseAssignLHSF
                 return $ Ann (Read lhs) (pos, None)

parseSkipStatF :: Parser (StatF ())
parseSkipStatF = do
                 whiteSpace
                 pos <- getPosition
                 reserved "skip"
                 return $ Ann Skip (pos, None)

parseFreeStatF :: Parser (StatF ())
parseFreeStatF = do
                 whiteSpace
                 pos <- getPosition
                 reserved "free"
                 expr <- parseExprF
                 return $ Ann (Free expr) (pos, None)

parseReturnStatF :: Parser (StatF ())
parseReturnStatF = do
                   whiteSpace
                   pos <- getPosition
                   reserved "return"
                   expr <- parseExprF
                   return $ Ann (Return expr) (pos, None)

parseExitStatF :: Parser (StatF ())
parseExitStatF = do
                 whiteSpace
                 pos <- getPosition
                 reserved "exit"
                 expr <- parseExprF
                 return $ Ann (Exit expr) (pos, None)

parsePrintStatF :: Parser (StatF ())
parsePrintStatF = do
                  whiteSpace
                  pos <- getPosition
                  reserved "print"
                  expr <- parseExprF
                  return $ Ann (Print expr) (pos, None)

parsePrintlnStatF :: Parser (StatF ())
parsePrintlnStatF = do
                    whiteSpace
                    pos <- getPosition
                    reserved "println"
                    expr <- parseExprF
                    return $ Ann (Println expr) (pos, None)

parseIfStatF :: Parser (StatF ())
parseIfStatF = do
               whiteSpace
               pos <- getPosition
               reserved "if"
               expr <- parseExprF
               reserved "then"
               stat1 <- parseStatListF
               reserved "else"
               stat2 <- parseStatListF
               reserved "fi"
               return $ Ann (If expr stat1 stat2) (pos, None)

parseWhileStatF :: Parser (StatF ())
parseWhileStatF = do
                  whiteSpace
                  pos <- getPosition
                  reserved "while"
                  expr <- parseExprF
                  reserved "do"
                  stat <- parseStatListF
                  reserved "done"
                  return $ Ann (While expr stat) (pos, None)

parseSubroutineStatF :: Parser (StatF ())
parseSubroutineStatF = do
                       whiteSpace
                       pos <- getPosition
                       reserved "begin"
                       stat <- parseStatListF
                       reserved "end"
                       return $ Ann (Subroutine stat) (pos, None)


parseAssignLHSF :: Parser (AssignLHSF ())
parseAssignLHSF = do
                  pos <- getPosition
                  (do {elem <- try parsePairElemF;
                    return $ Ann (PairElemLHS elem) (pos, None)}
                      <|> do {elem <- try parseArrayElemF;
                        return $ Ann (ArrayElemLHS elem) (pos, None)}
                      <|> do {ident <- try parseIdentF;
                        return $ Ann (IdentLHS ident) (pos, None)})

parseAssignRHSF :: Parser (AssignRHSF ())
parseAssignRHSF = (do {pos <- getPosition;
                  expr <- parseExprF;
                  return $ Ann (ExprRHS expr) (pos, None)}
             <|> parseNewPairRHSF
             <|> do {pos <- getPosition;
                     elem <- parsePairElemF;
                     return $ Ann (PairElemRHS elem) (pos, None)}
             <|> parseCallRHSF
             <|> parseArrayLiterRHSF)

parseArrayLiterRHSF :: Parser (AssignRHSF ())
parseArrayLiterRHSF = do
                      whiteSpace
                      pos <- getPosition
                      reservedOp "["
                      es <- commaSep parseExprF
                      reservedOp "]"
                      return $ Ann (ArrayLiter es) (pos, None)

parseNewPairRHSF :: Parser (AssignRHSF ())
parseNewPairRHSF = do
                  pos <- getPosition
                  reserved "newpair"
                  reservedOp "("
                  expr1 <- parseExprF
                  reservedOp ","
                  expr2 <- parseExprF
                  reservedOp ")"
                  return $ Ann (NewPair expr1 expr2) (pos, None)

parseCallRHSF :: Parser (AssignRHSF ())
parseCallRHSF = do
                pos <- getPosition
                reserved "call"
                ident <- parseIdentF
                args <- parens (commaSep parseExprF)
                return $ Ann (Call ident args) (pos, None)

parsePairElemF :: Parser (PairElemF ())
parsePairElemF = do
                 pos <- getPosition
                 ( do {reserved "fst";
                   expr <- parseExprF;
                   return $ Ann (PairElemFst expr) (pos, None)}
                    <|> do {reserved "snd";
                    expr <- parseExprF;
                    return $ Ann (PairElemSnd expr) (pos, None)})


parseExprF :: Parser (ExprF ())
parseExprF = do
             whiteSpace
             expr <- buildExpressionParser table term
             checkOverFlow expr
             checkPrefix expr
             return $ expr
     where checkPrefix (Ann (UExpr uop ef@(Ann e _)) _)
            = case e of
                LiterExpr liter ->
                  if (uop == Pos || uop == Neg) && not (isIntLiter liter)
                  then fail "syntatic error"
                  else  return ()
                otherwise -> checkPrefix ef
           checkPrefix (Ann (BExpr _ e1 e2) _)
            = checkPrefix e1 >> checkPrefix e2
           checkPrefix (Ann (BracketExpr e) _) = checkPrefix e
           checkPrefix (Ann (ArrayExpr (Ann (ArrayElem _ es) _)) _)
             = mapM_ checkPrefix es
           checkPrefix _ = return ()

           isIntLiter (Ann (IntLiter _) _) = True
           isIntLiter liter = False

           checkOverFlow intexpr = if invalid intexpr then fail("Int Overflow")
            else return ()
              where
                invalid (Ann (UExpr Neg(Ann (LiterExpr(Ann (IntLiter i)_))_))_) = i > 2^31
                invalid (Ann (LiterExpr(Ann (IntLiter i)_))_) = i > 2^31 -1
                invalid _ = False


table = [ [unary symbol "+" (UExpr Pos),
           unary symbol "-" (UExpr Neg),
           unary symbol"!" (UExpr Not),
           unary reserved "len" (UExpr Len),
           unary reserved "ord" (UExpr Ord),
           unary reserved "chr" (UExpr Chr)],
          [binary "*" (BExpr Mul) AssocLeft,
           binary "/" (BExpr Div) AssocLeft,
           binary "%" (BExpr Mod) AssocLeft],
          [binary "+" (BExpr Plus) AssocLeft,
           binary "-" (BExpr Minus) AssocLeft],
          [binary ">=" (BExpr GEq) AssocLeft, --Don't mess up order between >= and >
           binary ">" (BExpr G) AssocLeft,
           binary "<=" (BExpr LEq) AssocLeft,
           binary "<" (BExpr L) AssocLeft],
          [binary "==" (BExpr Eq) AssocLeft,
           binary "!=" (BExpr NEq) AssocLeft],
          [binary "&&" (BExpr And) AssocLeft],
          [binary "||" (BExpr Or) AssocLeft]
        ]


unary operation n f =
  Prefix . chainl1 (try (whiteSpace >>
                    getPosition >>= \pos ->
                    operation n >>
                    (return $ \e -> Ann (f e) (pos, None)))) $ return (.)

binary n f assoc =
  Infix (try (whiteSpace >>
         getPosition >>= \pos ->
         symbol n >>
         (return $ (\e1 e2 -> Ann (f e1 e2) (pos, None))))) assoc

term :: Parser (ExprF ())
term =  try parseLiterExprF
   <|>  try parseArrayExprF
   <|>  try parseIdentExprF
   <|>  try parseBracketExprF


parseArrayExprF :: Parser (ExprF ())
parseArrayExprF = do
                  pos <- getPosition
                  a <- parseArrayElemF
                  return $ Ann (ArrayExpr a) (pos, None)

parseBracketExprF :: Parser (ExprF ())
parseBracketExprF = do
                    pos <- getPosition
                    reservedOp "("
                    exprF <- parseExprF
                    reservedOp ")"
                    return $ Ann (BracketExpr exprF) (pos, None)

parseLiterExprF :: Parser (ExprF ())
parseLiterExprF = do
                  pos <- getPosition
                  l <- parseLiterF
                  return $ Ann (LiterExpr l) (pos, None)

parseIdentExprF :: Parser (ExprF ())
parseIdentExprF = do
                  whiteSpace
                  pos <- getPosition
                  i <- parseIdentF
                  return $ Ann (IdentExpr i) (pos, None)


parseLiterF :: Parser (LiterF ())
parseLiterF = try parseIntLiterF
         <|> try parseBoolLiterF
         <|> try parseCharLiterF
         <|> try parseStringLiterF
         <|> try parsePairLiterF

parseIntLiterF :: Parser (LiterF ())
parseIntLiterF = do
                 pos <- getPosition
                 x <- integer
                 return $ Ann (IntLiter x) (pos, None)


parseBoolLiterF :: Parser (LiterF ())
parseBoolLiterF = do
                  pos <- getPosition
                  (do {reserved "true";return $ Ann (BoolLiter True) (pos, None)}
                      <|> (do {reserved "false"; return $ Ann (BoolLiter False) (pos, None)}))

parseCharLiterF :: Parser (LiterF ())
parseCharLiterF = do
                  pos <- getPosition
                  ((try catchWrongEscape >> fail "wrong escape")
                    <|> (do {c <- charLiteral; return $ Ann (CharLiter c) (pos, None)}))
     where catchWrongEscape =
             do
               char '\''
               char '\"'
               char '\''
               return '\"'


parseStringLiterF :: Parser (LiterF ())
parseStringLiterF = do
                    pos <- getPosition
                    s <- stringLiteral
                    return $ Ann (StringLiter s) (pos, None)

parsePairLiterF :: Parser (LiterF ())
parsePairLiterF = do
                  pos <- getPosition
                  reserved "null"
                  return $ Ann Null (pos, None)

parseIdentF :: Parser (IdentF ())
parseIdentF = do
              pos <- getPosition
              i <- ident
              return $ Ann (Ident i) (pos, None)

parseArrayElemF :: Parser (ArrayElemF ())
parseArrayElemF = do
                  pos <- getPosition
                  i <- parseIdentF
                  exprs <- many1 (try parseIndex)
                  return $ Ann (ArrayElem i exprs) (pos, None)
           where parseIndex :: Parser (ExprF ())
                 parseIndex = do
                              expr <- brackets parseExprF
                              return expr

parseFile :: String -> IO (ProgramF ())
parseFile file =
  do
    program <- readFile file
    case parse parseProgramF "" program of
      Left e -> print e >>
                fail ("parse error: " ++ "at line " ++ line e ++ " and col " ++ col e ++ " with file " ++ file)
      Right r -> return r
      where line = \e -> show $ sourceLine $ errorPos e
            col = \e -> show $ sourceLine $ errorPos e
            name = \e -> show $ sourceName $ errorPos e
