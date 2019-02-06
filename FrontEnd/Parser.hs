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
parseFuncF = whiteSpace >>= \_ ->
             getPosition >>= \pos ->
             parseTypeF >>= \t ->
             parseIdentF >>= \ident ->
             parens (commaSep parseParamF) >>= \ps ->
             reserved "is" >>= \_ ->
             parseStatListF >>= \stat ->
             checkReturnExit stat >>= \_ ->
             reserved "end" >>= \_ ->
             return $ Ann (Func t ident ps stat) (pos, None)
    where checkReturnExit (Ann (StatList stats) (pos, None))
            = case lastStat of
                Return _ -> return ()
                Exit _ -> return ()
                If _ stat1 stat2 ->
                      checkReturnExit stat1 >>= \_ ->
                      checkReturnExit stat2
                While _ stat -> checkReturnExit stat
                Subroutine stat -> checkReturnExit stat
                otherwise -> fail "Expected return"
           where Ann lastStat _ = last stats


parseParamF :: Parser (ParamF ())
parseParamF = whiteSpace >>= \_ ->
              getPosition >>= \pos ->
              parseTypeF >>= \t ->
              parseIdentF >>= \ident ->
              return $ Ann (Param t ident) (pos, None)

parseTypeF :: Parser (TypeF ())
parseTypeF = try parseArrayTypeF
         <|> try parseBaseTypeF
         <|> parsePairTypeF

parseArrayTypeF :: Parser (TypeF ())
parseArrayTypeF = whiteSpace >>= \_ ->
             getPosition >>= \pos ->
            (parseBaseTypeF
         <|> parsePairTypeF) >>= \t ->
             many (try $ reserved "[]") >>= \rs ->
             return $ foldl (\acc x -> Ann (TArray acc) (pos, None)) t rs

parseBaseTypeF :: Parser (TypeF ())
parseBaseTypeF = whiteSpace >>= \_ ->
                 getPosition >>= \pos ->
                (reserved "int" >>= \_ -> return $ Ann TInt (pos, None))
            <|> (reserved "bool" >>= \_ -> return $ Ann TBool (pos, None))
            <|> (reserved "char" >>= \_ -> return $ Ann TChar (pos, None))
            <|> (reserved "string" >>= \_ -> return $ Ann TStr (pos, None))

parsePairTypeF :: Parser (TypeF ())
parsePairTypeF = whiteSpace >>= \_ ->
                 getPosition >>= \pos ->
                 string "pair" >>= \_ ->
                 reservedOp "(" >>= \_ ->
                 parsePairElemTypeF >>= \t1 ->
                 comma >>= \_ ->
                 parsePairElemTypeF >>= \t2 ->
                 reservedOp ")" >>= \_ ->
                 return $ Ann (TPair t1 t2) (pos, None)
        where parsePairElemTypeF :: Parser (TypeF ())
              parsePairElemTypeF = try parseArrayTypeF
                               <|> try parseBaseTypeF
                               <|> (getPosition >>= \pos ->
                                    string "pair" >>= \_ ->
                                    return $ (Ann Any (pos, None)))

parseStatListF :: Parser (StatListF ())
parseStatListF = whiteSpace >>= \_ ->
                 getPosition >>= \pos ->
                 parseStatF >>= \stat ->
                   try (semi >>= \_ ->
                    parseStatListF >>= \(Ann (StatList rest) _) ->
                    return $ Ann (StatList (stat:rest)) (pos, None))
                <|> (return $ Ann (StatList [stat]) (pos, None))


parseStatF :: Parser (StatF ())
parseStatF = whiteSpace >>= \_ ->
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
parseAssignLHSF = getPosition >>= \pos ->
                  (try parsePairElemF >>= \elem ->
                    return $ Ann (PairElemLHS elem) (pos, None))
              <|> (try parseArrayElemF >>= \elem ->
                    return $ Ann (ArrayElemLHS elem) (pos, None))
              <|> (try parseIdentF >>= \ident ->
                    return $ Ann (IdentLHS ident) (pos, None))

parseAssignRHSF :: Parser (AssignRHSF ())
parseAssignRHSF = (getPosition >>= \pos ->
                    parseExprF >>= \expr ->
                   return $ Ann (ExprRHS expr) (pos, None))
             <|> parseNewPairRHSF
             <|> (getPosition >>= \pos ->
               parsePairElemF >>= \elem ->
               return $ Ann (PairElemRHS elem) (pos, None))
             <|> parseCallRHSF
             <|> parseArrayLiterRHSF

parseArrayLiterRHSF :: Parser (AssignRHSF ())
parseArrayLiterRHSF = whiteSpace >>= \_ ->
                      getPosition >>= \pos ->
                      reservedOp "[" >>= \_ ->
                      commaSep parseExprF >>= \es ->
                      reservedOp "]" >>= \_ ->
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
parsePairElemF = getPosition >>= \pos ->
             ( reserved "fst" >>= \_ ->
               parseExprF >>= \expr ->
               return $ Ann (PairElemFst expr) (pos, None))
         <|> ( reserved "snd" >>= \_ ->
               parseExprF >>= \expr->
               return $ Ann (PairElemSnd expr) (pos, None))


parseExprF :: Parser (ExprF ())
parseExprF = whiteSpace >>= \_ ->
             buildExpressionParser table term >>= \expr ->
             checkPrefix expr >>= \_ ->
             return $ expr
     where checkPrefix (Ann (UExpr uop ef@(Ann e _)) _)
            = case e of
                LiterExpr liter ->
                  if (uop == Pos || uop == Neg) && not (isIntLiter liter)
                  then fail "syntatic error"
                  else return ()
                otherwise -> checkPrefix ef
           checkPrefix (Ann (BExpr _ e1 e2) _)
            = checkPrefix e1 >>= \_ ->
              checkPrefix e2
           checkPrefix (Ann (BracketExpr e) _) = checkPrefix e
           checkPrefix (Ann (ArrayExpr (Ann (ArrayElem _ es) _)) _)
             = mapM_ checkPrefix es
           checkPrefix _ = return ()


           isIntLiter (Ann (IntLiter _) _) = True
           isIntLiter liter = False



table = [ [unary "+" (UExpr Pos),
           unary "-" (UExpr Neg),
           unary "!" (UExpr Not),
           unary "len" (UExpr Len),
           unary "ord" (UExpr Ord),
           unary "chr" (UExpr Chr)],
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


unary n f =
  Prefix . chainl1 (try (whiteSpace >>= \_ ->
                    getPosition >>= \pos ->
                    symbol n >>= \_ ->
                    return $ \e -> Ann (f e) (pos, None))) $ return (.)

binary n f assoc =
  Infix (try (whiteSpace >>= \_ ->
         getPosition >>= \pos ->
         symbol n >>= \_ ->
         return $ (\e1 e2 -> Ann (f e1 e2) (pos, None)))) assoc

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
parseLiterExprF = getPosition >>= \pos ->
                  parseLiterF >>= \l ->
                  return $ Ann (LiterExpr l) (pos, None)

parseIdentExprF :: Parser (ExprF ())
parseIdentExprF = whiteSpace >>= \_ ->
                  getPosition >>= (\pos ->
                  parseIdentF >>= (\i ->
                  return $ Ann (IdentExpr i) (pos, None)))


parseLiterF :: Parser (LiterF ())
parseLiterF = try parseIntLiterF
         <|> try parseBoolLiterF
         <|> try parseCharLiterF
         <|> try parseStringLiterF
         <|> try parsePairLiterF

parseIntLiterF :: Parser (LiterF ())
parseIntLiterF = getPosition >>= (\pos ->
                integer >>= (\x ->
                (if (invalidInt x)
                  then fail "integer overflow"
                  else return $ Ann (IntLiter x) (pos, None))))

                  where
                    invalidInt :: Integer -> Bool
                    invalidInt i = i > (2 ^ 31) - 1 || i < - 2 ^ 31


parseBoolLiterF :: Parser (LiterF ())
parseBoolLiterF = getPosition >>= (\pos ->
              (reserved "true" >>= (\_ ->
                return $ Ann (BoolLiter True) (pos, None)))
          <|> (reserved "false" >>= (\_ ->
                return $ Ann (BoolLiter False) (pos, None)))
          )

parseCharLiterF :: Parser (LiterF ())
parseCharLiterF = getPosition >>= \pos ->
              (try catchWrongEscape >>= \_ ->
                  fail "wrong escape")
          <|> (charLiteral >>= \c ->
              return $ Ann (CharLiter c) (pos, None))
     where catchWrongEscape =
             do
               char '\''
               char '\"'
               char '\''
               return '\"'


parseStringLiterF :: Parser (LiterF ())
parseStringLiterF = getPosition >>= (\pos ->
               stringLiteral >>= (\s ->
               return $ Ann (StringLiter s) (pos, None)))

parsePairLiterF :: Parser (LiterF ())
parsePairLiterF = getPosition >>= \pos ->
                  reserved "null" >>= \_ ->
                  return $ Ann Null (pos, None)


parseIdentF :: Parser (IdentF ())
parseIdentF = getPosition >>= (\pos ->
               ident >>= (\i ->
               return $ Ann (Ident i) (pos, None)))

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
