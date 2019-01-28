module Lexer where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

keyNames :: [String]
keyNames = [ "begin",
             "is",
             "end",
             "skip",
             "read",
             "free",
             "return",
             "exit",
             "print",
             "println",
             "if",
             "then",
             "else",
             "fi",
             "while",
             "do",
             "done",
             "newpair",
             "call",
             "fst",
             "snd",
             "int",
             "bool",
             "char",
             "string",
             "pair",
             "true",
             "false",
             "null"]

keyOps :: [String]
keyOps = ["!",
          "+",
          "-",
          "len",
          "ord",
          "chr",
          "*",
          "/",
          "%",
          "+",
          "-",
          ">",
          ">=",
          "<",
          "<=",
          "==",
          "!=",
          "&&",
          "||",
          "[",
          "]",
          "(",
          ")",
          "[]"]
languageDef = 
  emptyDef { Token.commentLine = "#"
           , Token.identStart = letter <|> char '_'
           , Token.identLetter = alphaNum <|> char '_'
           , Token.reservedNames = keyNames
           }

 --, Token.reservedOpNames = keyOps
lexer = Token.makeTokenParser languageDef

ident = Token.identifier lexer
reserved = Token.reserved lexer
parens = Token.parens lexer
integer = Token.integer lexer
charLiteral = Token.charLiteral lexer
stringLiteral = Token.stringLiteral lexer
whiteSpace = Token.whiteSpace lexer
semi = Token.semi lexer
reservedOp = Token.reservedOp lexer
comma = Token.comma lexer
leftSqB = Token.reservedOp lexer "["
rightSqB = Token.reservedOp lexer "]"
leftRB = Token.reservedOp lexer "("
rightRB = Token.reservedOp lexer ")"

semiSep = Token.semiSep lexer
commaSep = Token.commaSep lexer
symbol = Token.symbol lexer
brackets = Token.brackets lexer
