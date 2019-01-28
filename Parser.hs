module Parser where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language

import Text.ParserCombinators.Parsec.Pos

import Lexer
import AST

type Label = SourcePos

parseProgramF :: Parser (ProgramF Label)
parseProgramF = do
              whiteSpace
              pos <- getPosition
              reserved "begin"
              fs <- many (try parseFuncF)
              stat <- parseStatF
              reserved "end"
              return $ Ann (Program fs stat) pos

parseFuncF :: Parser (FuncF Label)
parseFuncF = whiteSpace >>= \_ ->
             getPosition >>= \pos ->
             parseTypeF >>= \t ->
             parseIdentF >>= \ident ->
             parens (commaSep parseParamF) >>= \ps ->
             reserved "is" >>= \_ ->
             parseStatF >>= \stat ->
             reserved "end" >>= \_ ->
             return $ Ann (Func t ident ps stat) pos 


parseParamF :: Parser (ParamF Label)
parseParamF = whiteSpace >>= \_ ->
              getPosition >>= \pos ->
              parseTypeF >>= \t ->
              parseIdentF >>= \ident ->
              return $ Ann (Param t ident) pos

parseTypeF :: Parser (TypeF Label)
parseTypeF = try parseArrayTypeF
         <|> try parseBaseTypeF
         <|> parsePairTypeF

parseArrayTypeF :: Parser (TypeF Label)
parseArrayTypeF = whiteSpace >>= \_ ->
             getPosition >>= \pos ->
            (parseBaseTypeF
         <|> parsePairTypeF) >>= \t ->
             many (try $ reserved "[]") >>= \rs ->
             return $ foldl (\acc x -> Ann (TArray acc) pos) t rs 
  
parseBaseTypeF :: Parser (TypeF Label)
parseBaseTypeF = whiteSpace >>= \_ ->
                 getPosition >>= \pos -> 
                (reserved "int" >>= \_ -> return $ Ann TInt pos)
            <|> (reserved "bool" >>= \_ -> return $ Ann TBool pos)
            <|> (reserved "char" >>= \_ -> return $ Ann TChar pos)
            <|> (reserved "string" >>= \_ -> return $ Ann TStr pos)
           
parsePairTypeF :: Parser (TypeF Label)
parsePairTypeF = whiteSpace >>= \_ ->
                 getPosition >>= \pos ->
                 string "pair" >>= \_ ->
                 reservedOp "(" >>= \_ ->
                 parsePairElemTypeF >>= \t1 ->
                 comma >>= \_ ->
                 parsePairElemTypeF >>= \t2 ->
                 reservedOp ")" >>= \_ -> 
                 return $ Ann (TPair t1 t2) pos
        where parsePairElemTypeF :: Parser (TypeF Label)
              parsePairElemTypeF = try parseArrayTypeF
                               <|> try parseBaseTypeF
                               <|> (getPosition >>= \pos ->
                                    string "pair" >>= \_ ->
                                    return $ (Ann Any pos))
                        
parseStatF :: Parser (StatF Label)
parseStatF = whiteSpace >>= \_ ->
             getPosition >>= \pos ->
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
         <|> parseSubroutineStatF) >>= \stat1 ->
             (try semi >>= \_ ->
              parseStatF >>= \stat2 ->
              return $ Ann (Seq stat1 stat2) pos)
         <|> (return stat1)

parseDeclareStatF :: Parser (StatF Label)
parseDeclareStatF = do
                    pos <- getPosition
                    t <- parseTypeF
                    ident <- parseIdentF
                    reservedOp "="
                    rhs <- parseAssignRHSF
                    return $ Ann (Declare t ident rhs) pos

parseAssignStatF :: Parser (StatF Label)
parseAssignStatF = do
                   pos <- getPosition
                   lhs <- parseAssignLHSF
                   reservedOp "="
                   rhs <- parseAssignRHSF
                   return $ Ann (Assign lhs rhs) pos
                                       
parseReadStatF :: Parser (StatF Label)
parseReadStatF = do
                 pos <- getPosition
                 reserved "read"
                 lhs <- parseAssignLHSF
                 return $ Ann (Read lhs) pos

parseSkipStatF :: Parser (StatF Label)
parseSkipStatF = do
                 whiteSpace
                 pos <- getPosition
                 reserved "skip"
                 return $ Ann Skip pos

parseFreeStatF :: Parser (StatF Label)
parseFreeStatF = do
                 whiteSpace
                 pos <- getPosition
                 reserved "free"
                 expr <- parseExprF
                 return $ Ann (Free expr) pos
                 
parseReturnStatF :: Parser (StatF Label)
parseReturnStatF = do
                   whiteSpace
                   pos <- getPosition
                   reserved "return"
                   expr <- parseExprF
                   return $ Ann (Return expr) pos

parseExitStatF :: Parser (StatF Label)
parseExitStatF = do
                 whiteSpace
                 pos <- getPosition
                 reserved "exit"
                 expr <- parseExprF
                 return $ Ann (Exit expr) pos

parsePrintStatF :: Parser (StatF Label)
parsePrintStatF = do
                  whiteSpace
                  pos <- getPosition
                  reserved "print"
                  expr <- parseExprF
                  return $ Ann (Print expr) pos
                  
parsePrintlnStatF :: Parser (StatF Label)
parsePrintlnStatF = do
                    whiteSpace
                    pos <- getPosition
                    reserved "println"
                    expr <- parseExprF
                    return $ Ann (Println expr) pos

parseIfStatF :: Parser (StatF Label)
parseIfStatF = do
               whiteSpace
               pos <- getPosition
               reserved "if"
               expr <- parseExprF
               reserved "then"
               stat1 <- parseStatF
               reserved "else"
               stat2 <- parseStatF
               reserved "fi"
               return $ Ann (If expr stat1 stat2) pos

parseWhileStatF :: Parser (StatF Label)
parseWhileStatF = do
                  whiteSpace
                  pos <- getPosition
                  reserved "while"
                  expr <- parseExprF
                  reserved "do"
                  stat <- parseStatF
                  reserved "done"
                  return $ Ann (While expr stat) pos

parseSubroutineStatF :: Parser (StatF Label)
parseSubroutineStatF = do
                       whiteSpace
                       pos <- getPosition
                       program <- parseProgramF
                       return $ Ann (Subroutine program) pos


parseAssignLHSF :: Parser (AssignLHSF Label)
parseAssignLHSF = getPosition >>= \pos -> 
                  (try parsePairElemF >>= \elem ->
                    return $ Ann (PairElemLHS elem) pos)
              <|> (try parseArrayElemF >>= \elem ->
                    return $ Ann (ArrayElemLHS elem) pos)
              <|> (try parseIdentF >>= \ident ->
                    return $ Ann (IdentLHS ident) pos)
                  
parseAssignRHSF :: Parser (AssignRHSF Label)
parseAssignRHSF = (getPosition >>= \pos ->
                    parseExprF >>= \expr ->
                   return $ Ann (ExprRHS expr) pos)
             <|> parseNewPairRHSF
             <|> (getPosition >>= \pos ->
               parsePairElemF >>= \elem ->
               return $ Ann (PairElemRHS elem) pos)
             <|> parseCallRHSF
             <|> parseArrayLiterRHSF

parseArrayLiterRHSF :: Parser (AssignRHSF Label)
parseArrayLiterRHSF = whiteSpace >>= \_ ->
                      getPosition >>= \pos ->
                      reservedOp "[" >>= \_ ->
                      commaSep parseExprF >>= \es ->
                      reservedOp "]" >>= \_ ->
                      return $ Ann (ArrayLiter es) pos
                 
parseNewPairRHSF :: Parser (AssignRHSF Label)
parseNewPairRHSF = do
                  pos <- getPosition
                  reserved "newpair"
                  reservedOp "("
                  expr1 <- parseExprF
                  reservedOp ","
                  expr2 <- parseExprF
                  reservedOp ")"
                  return $ Ann (NewPair expr1 expr2) pos
                  
parseCallRHSF :: Parser (AssignRHSF Label)
parseCallRHSF = do
                pos <- getPosition
                reserved "call"
                ident <- parseIdentF
                args <- parens (commaSep parseExprF)
                return $ Ann (Call ident args) pos

parsePairElemF :: Parser (PairElemF Label)
parsePairElemF = getPosition >>= \pos ->
             ( reserved "fst" >>= \_ ->
               parseExprF >>= \expr ->
               return $ Ann (PairElemFst expr) pos)
         <|> ( reserved "snd" >>= \_ ->
               parseExprF >>= \expr->
               return $ Ann (PairElemSnd expr) pos)
  
parseExprF :: Parser (ExprF Label)
parseExprF = whiteSpace >>= \_ ->
             buildExpressionParser table term
             
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
                    return $ \e -> Ann (f e) pos)) $ return (.)
  
binary n f assoc =
  Infix (try (whiteSpace >>= \_ ->
         getPosition >>= \pos ->
         symbol n >>= \_ ->
         return $ (\e1 e2 -> Ann (f e1 e2) pos))) assoc

term :: Parser (ExprF Label)
term =  try parseLiterExprF
   <|>  try parseArrayExprF
   <|>  try parseIdentExprF
   <|>  try parseBracketExprF
            

parseArrayExprF :: Parser (ExprF Label)
parseArrayExprF = do
                  pos <- getPosition
                  a <- parseArrayElemF
                  return $ Ann (ArrayExpr a) pos

parseBracketExprF :: Parser (ExprF Label)
parseBracketExprF = do
                    pos <- getPosition
                    reservedOp "("
                    exprF <- parseExprF
                    reservedOp ")"
                    return $ Ann (BracketExpr exprF) pos

parseLiterExprF :: Parser (ExprF Label)
parseLiterExprF = getPosition >>= \pos ->
                  parseLiterF >>= \l ->
                  return $ Ann (LiterExpr l) pos

parseIdentExprF :: Parser (ExprF Label)
parseIdentExprF = whiteSpace >>= \_ ->
                  getPosition >>= (\pos ->
                  parseIdentF >>= (\i ->
                  return $ Ann (IdentExpr i) pos))


parseLiterF :: Parser (LiterF Label)
parseLiterF = try parseIntLiterF
         <|> try parseBoolLiterF
         <|> try parseCharLiterF
         <|> try parseStringLiterF
         <|> try parsePairLiterF

parseIntLiterF :: Parser (LiterF Label)
parseIntLiterF = getPosition >>= (\pos -> 
               integer >>= (\x -> 
               return $ Ann (IntLiter x) pos)
           )

parseBoolLiterF :: Parser (LiterF Label)
parseBoolLiterF = getPosition >>= (\pos ->
              (reserved "true" >>= (\_ -> 
                return $ Ann (BoolLiter True) pos))
          <|> (reserved "false" >>= (\_ -> 
                return $ Ann (BoolLiter False) pos))
          )

parseCharLiterF :: Parser (LiterF Label)
parseCharLiterF = getPosition >>= (\pos ->
              charLiteral >>= (\c -> 
              return $ Ann (CharLiter c) pos))

parseStringLiterF :: Parser (LiterF Label)
parseStringLiterF = getPosition >>= (\pos ->
               stringLiteral >>= (\s -> 
               return $ Ann (StringLiter s) pos))

parsePairLiterF :: Parser (LiterF Label)
parsePairLiterF = getPosition >>= \pos ->
                  reserved "null" >>= \_ ->
                  return $ Ann Null pos


parseIdentF :: Parser (IdentF Label)
parseIdentF = getPosition >>= (\pos -> 
               ident >>= (\i -> 
               return $ Ann (Ident i) pos))

parseArrayElemF :: Parser (ArrayElemF Label)
parseArrayElemF = do
                  pos <- getPosition
                  i <- parseIdentF
                  exprs <- many (try parseIndex)
                  return $ Ann (ArrayElem i exprs) pos
           where parseIndex :: Parser (ExprF Label)
                 parseIndex = do
                              expr <- brackets parseExprF
                              return expr

parseFile :: String -> IO (ProgramF Label)
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

