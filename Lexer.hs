module Lexer where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.Parsec.Combinator as Combinator
import qualified Text.ParserCombinators.Parsec.Token as CToken
import Text.Parsec.Token as Token
import qualified Text.Parsec.Char as CharToken


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
  emptyDef { CToken.commentLine = "#"
           , CToken.identStart = letter <|> char '_'
           , CToken.identLetter = alphaNum <|> char '_'
           , CToken.reservedNames = keyNames
           }

 --, Token.reservedOpNames = keyOps


lexer = CToken.makeTokenParser languageDef

ident = CToken.identifier lexer
reserved = CToken.reserved lexer
parens = CToken.parens lexer
integer = CToken.integer lexer
charLiteral = CToken.charLiteral lexer
stringLiteral = CToken.stringLiteral lexer
whiteSpace = CToken.whiteSpace lexer
semi = CToken.semi lexer
reservedOp = CToken.reservedOp lexer
comma = CToken.comma lexer
leftSqB = CToken.reservedOp lexer "["
rightSqB = CToken.reservedOp lexer "]"
leftRB = CToken.reservedOp lexer "("
rightRB = CToken.reservedOp lexer ")"

semiSep = CToken.semiSep lexer
commaSep = CToken.commaSep lexer
symbol = CToken.symbol lexer
brackets = CToken.brackets lexer
