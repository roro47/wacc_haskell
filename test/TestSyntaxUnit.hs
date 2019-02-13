module TestSyntaxUnit where

import Text.ParserCombinators.Parsec
import Test.Hspec
import TestUtil as Util
import FrontEnd.Parser
import FrontEnd.AST

test = do
      putStrLn "*:;;;:*:;;;:*:;;;:*Syntax error test*:;;;:*:;;;:*:;;;:*"
      integerTest
      charTest
      boolTest
      stringTest
      arrayRHSTest
      newpairRHSTest
      pairElemTest
      arrayElemTest
      identTest
      uopTest
      binopTest
      bracketTest
      statTest
      typeTest
      statListTest

integerTest = hspec $ do
       describe "parse Integer:" $ do
         it "☑️ can parse integer" $ do
              parseUsing "123" parseIntLiter `shouldReturn` (Just "IntLiter 123")
         it "☑️ can parse negative integer" $ do
              parseUsing "-123" parseIntLiter `shouldReturn` (Just "IntLiter (-123)")
         it "☑️ can parse positive integer with sign" $ do
              parseUsing "+123" parseIntLiter `shouldReturn` (Just "IntLiter 123")
         it "✖️ cannot parse overflow numbers" $ do
              parseUsing "9999999999999999999999" parseExprF `shouldReturn` Nothing

charTest = hspec $ do
      describe "parse Char:" $ do
         it "☑️ can parse charLiteral" $ do
              parseUsing "'c'" parseCharLiter `shouldReturn` (Just "CharLiter 'c'")
         it "✖️ cannot parse escape chars without escape" $ do
              parseUsing "'\"'" parseCharLiter `shouldReturn` Nothing
         it "☑️ can parse escape chars with escape" $ do
              parseUsing "'\\\"'" parseCharLiter `shouldReturn` (Just "CharLiter '\"'")
         it "☑️ can parse weird control characters" $ do
              parseUsing "'\\SOH'" parseCharLiter `shouldReturn` (Just "CharLiter '\\SOH'")

boolTest = hspec $ do
      describe "parse Bool:" $ do
         it "☑️ can parse True" $ do
              parseUsing "true" parseBoolLiter `shouldReturn` (Just "BoolLiter True")
         it "☑️ can parse escape chars with escape" $ do
              parseUsing "false" parseBoolLiter `shouldReturn` (Just "BoolLiter False")
         it "✖️ cannot parse uppercase bool" $ do
              parseUsing "False" parseBoolLiter `shouldReturn` Nothing

stringTest = hspec $ do
      describe "parse String:" $ do
         it "☑️ can parse normal string" $ do
              parseUsing "\"Hello, World!\"" parseStringLiter
              `shouldReturn` (Just "StringLiter \"Hello, World!\"")
         it "✖️ cannot parse string with incorrect escape" $ do
              parseUsing "Hello, \"World!" parseStringLiter `shouldReturn` Nothing

arrayRHSTest = hspec $ do
      describe "parse Array:" $ do
         it "☑️ can parse empty array" $ do
              parseUsing "[]" parseArrayLiter `shouldReturn` (Just "ArrayLiter []")
         it "✖️ cannot directly parse multidimension array" $ do
              parseUsing "[[],[]]" parseArrayLiter `shouldReturn` Nothing
         it "✖️ cannot directly parse pair array" $ do
              parseUsing "[(1, 2)]" parseArrayLiter `shouldReturn` Nothing
         it "☑️ can parse many kinds of arrays..." $ do
              parseUsing "[1, 2]" parseArrayLiter `shouldReturn` (
                Just "ArrayLiter [IntLiter 1,IntLiter 2]")
              parseUsing "['a', 'b']" parseArrayLiter `shouldReturn` (
                Just "ArrayLiter [CharLiter 'a',CharLiter 'b']")
              parseUsing "[\"hello\"]" parseArrayLiter `shouldReturn` (
                Just "ArrayLiter [StringLiter \"hello\"]")
              parseUsing "[a]" parseArrayLiter `shouldReturn` (
                Just "ArrayLiter [IdentExpr \"a\"]")
              parseUsing "[true, true]" parseArrayLiter `shouldReturn` (
                Just "ArrayLiter [BoolLiter True,BoolLiter True]")

newpairRHSTest = hspec $ do
      describe "parse pair:" $ do
          it "☑️ can parse int pair" $ do
              parseUsing "newpair (1, 2)" parseNewPair `shouldReturn` (
                Just  "FuncExpr FuncApp \"newpair\" [IntLiter 1,IntLiter 2]")
          it "☑️ can parse pair with different types" $ do
              parseUsing "newpair (1, 'a')" parseNewPair `shouldReturn` (
                Just  "FuncExpr FuncApp \"newpair\" [IntLiter 1,CharLiter 'a']")
          it "☑️ can parse pair with null" $ do
              parseUsing "newpair (null, null)" parseNewPair `shouldReturn` (
                Just  "FuncExpr FuncApp \"newpair\" [Null,Null]")

pairElemTest = hspec $ do
      describe "parse pair elements:" $ do
          it "☑️ can parse fst" $ do
              parseUsing "fst a" parsePairElem `shouldReturn` (
                Just  "FuncExpr FuncApp \"fst\" [IdentExpr \"a\"]")
          it "☑️ can parse snd" $ do
              parseUsing "snd a" parsePairElem `shouldReturn` (
                Just  "FuncExpr FuncApp \"snd\" [IdentExpr \"a\"]")

arrayElemTest = hspec $ do
      describe "parse array elements:" $ do
          it "☑️ can parse int index" $ do
              parseUsing "a[1]" parseArrayElem `shouldReturn` (
                Just  "ArrayElem \"a\" [IntLiter 1]")
          it "☑️ can parse identifier index" $ do
              parseUsing "a[b]" parseArrayElem `shouldReturn` (
                Just  "ArrayElem \"a\" [IdentExpr \"b\"]")

identTest = hspec $ do
      describe "parse identifiers:" $ do
          it "☑️ can parse normal identifier" $ do
              parseUsing "ident233H_" parseIdentF `shouldReturn` (
                Just "\"ident233H_\"")
          it "✖️ cannot parse identifier starting with number" $ do
              parseUsing "2ident" parseIdentF `shouldReturn` Nothing
          it "☑️ can parse identifier starting Capital letter" $ do
              parseUsing "Ident" parseIdentF `shouldReturn` (Just "\"Ident\"")
          it "☑️ can parse identifier starting underscore" $ do
              parseUsing "_Ident" parseIdentF `shouldReturn` (Just "\"_Ident\"")
          it "✖️ cannot parse identifier with weird characters" $ do
              parseUsing "insI%@!d##S" parseIdentF `shouldReturn` (Just "\"insI\"")

uopTest = hspec $ do
      describe "parse unary operators:" $ do
          it "☑️ can parse pos, neg, chr, ord" $ do
              parseUsing "+1" parseExprF `shouldReturn` (
                Just "FuncExpr FuncApp \"#pos\" [IntLiter 1]")
              parseUsing "-a" parseExprF `shouldReturn` (
                Just "FuncExpr FuncApp \"#neg\" [IdentExpr \"a\"]")
              parseUsing "!a" parseExprF `shouldReturn` (
                Just "FuncExpr FuncApp \"!\" [IdentExpr \"a\"]")
              parseUsing "chr 12" parseExprF `shouldReturn` (
                Just "FuncExpr FuncApp \"chr\" [IntLiter 12]")
              parseUsing "ord 'a'" parseExprF `shouldReturn` (
                Just "FuncExpr FuncApp \"ord\" [CharLiter 'a']")
          it "✖️ cannot parse pos and neg on strings and chars" $ do
              parseUsing "+\"hello\"" parseExprF `shouldReturn` Nothing

binopTest = hspec $ do
      describe "parse binary operators:" $ do
          it "☑️ Mul, Div, Mod, Plus, Minus, G, GEq, L, LEq, Eq, NEq, And, Or" $ do
              parseUsing "1*1" parseExprF `shouldReturn` (
                Just "FuncExpr FuncApp \"*\" [IntLiter 1,IntLiter 1]")
              parseUsing "2/a" parseExprF `shouldReturn` (
                Just "FuncExpr FuncApp \"/\" [IntLiter 2,IdentExpr \"a\"]")
              parseUsing "3%4" parseExprF `shouldReturn` (
                Just "FuncExpr FuncApp \"%\" [IntLiter 3,IntLiter 4]")
              parseUsing "2+2" parseExprF `shouldReturn` (
                Just "FuncExpr FuncApp \"+\" [IntLiter 2,IntLiter 2]")
              parseUsing "c-b" parseExprF `shouldReturn` (
                Just "FuncExpr FuncApp \"-\" [IdentExpr \"c\",IdentExpr \"b\"]")
              parseUsing "a > b" parseExprF `shouldReturn` (
                Just "FuncExpr FuncApp \">\" [IdentExpr \"a\",IdentExpr \"b\"]")
              parseUsing "a >= b" parseExprF `shouldReturn` (
                Just "FuncExpr FuncApp \">=\" [IdentExpr \"a\",IdentExpr \"b\"]")
              parseUsing "a < b" parseExprF `shouldReturn` (
                Just "FuncExpr FuncApp \"<\" [IdentExpr \"a\",IdentExpr \"b\"]")
              parseUsing "a <= b" parseExprF `shouldReturn` (
                Just "FuncExpr FuncApp \"<=\" [IdentExpr \"a\",IdentExpr \"b\"]")
              parseUsing "a == b" parseExprF `shouldReturn` (
                Just "FuncExpr FuncApp \"==\" [IdentExpr \"a\",IdentExpr \"b\"]")
              parseUsing "a != b" parseExprF `shouldReturn` (
                Just "FuncExpr FuncApp \"!=\" [IdentExpr \"a\",IdentExpr \"b\"]")
              parseUsing "a && b" parseExprF `shouldReturn` (
                Just "FuncExpr FuncApp \"&&\" [IdentExpr \"a\",IdentExpr \"b\"]")
              parseUsing "a || b" parseExprF `shouldReturn` (
                Just "FuncExpr FuncApp \"||\" [IdentExpr \"a\",IdentExpr \"b\"]")

bracketTest = hspec $ do
      describe "parse brackets:" $ do
          it "✖️ cannot parse empty bracket" $ do
              parseUsing "()" parseBracketExpr `shouldReturn` Nothing
          it "☑️ can parse some normal bracket" $ do
              parseUsing
               "(a)" parseBracketExpr `shouldReturn` (
                Just "BracketExpr IdentExpr \"a\"")
          it "☑️ can parse bracket in bracket" $ do
              parseUsing "((a))" parseBracketExpr `shouldReturn` (
                Just "BracketExpr BracketExpr IdentExpr \"a\"")
          it "✖️ cannot parse broken brackets" $ do
              parseUsing "(a" parseBracketExpr `shouldReturn` Nothing
              parseUsing "a)" parseBracketExpr `shouldReturn` Nothing
              parseUsing ")" parseBracketExpr `shouldReturn` Nothing
              parseUsing "(" parseBracketExpr `shouldReturn` Nothing

statTest = hspec $ do
      describe "parse stats:" $ do
          it "☑️ can parse skip" $ do
              parseUsing "skip" parseStatF `shouldReturn` (Just "FuncStat FuncApp \"skip\" []")
          it "☑️ can parse assign" $ do
              parseUsing "int a = b" parseStatF `shouldReturn` (
                Just "Declare TInt \"a\" IdentExpr \"b\"")
          it "☑️ can parse read" $ do
              parseUsing "read a" parseStatF `shouldReturn` (
                Just "FuncStat FuncApp \"read\" [IdentExpr \"a\"]")
          it "☑️ can parse read" $ do
              parseUsing "free a" parseStatF `shouldReturn` (
                Just "FuncStat FuncApp \"free\" [IdentExpr \"a\"]")
          it "☑️ can parse return" $ do
              parseUsing "return a" parseStatF `shouldReturn` (
                Just "Return IdentExpr \"a\"")
          it "☑️ can parse exit" $ do
              parseUsing "exit a" parseStatF `shouldReturn` (
                Just "Exit IdentExpr \"a\"")
          it "☑️ can parse print" $ do
              parseUsing "print a" parseStatF `shouldReturn` (
                Just "FuncStat FuncApp \"print\" [IdentExpr \"a\"]")
          it "☑️ can parse println" $ do
              parseUsing "println a" parseStatF `shouldReturn` (
                Just "FuncStat FuncApp \"println\" [IdentExpr \"a\"]")
          it "☑️ can parse if" $ do
              parseUsing "if true then skip else skip fi" parseStatF `shouldReturn` (
                Just "If BoolLiter True StatList [FuncStat FuncApp \"skip\" []] StatList [FuncStat FuncApp \"skip\" []]")
          it "☑️ can parse while" $ do
              parseUsing "while true do skip done" parseStatF `shouldReturn` (
                Just "While BoolLiter True StatList [FuncStat FuncApp \"skip\" []]")
          it "☑️ can parse Subroutine" $ do
              parseUsing "begin skip end" parseStatF `shouldReturn` (
                Just "Subroutine StatList [FuncStat FuncApp \"skip\" []]")

typeTest = hspec $ do
      describe "parse types:" $ do
          it "☑️ can parse int, bool, char, string, array, pair, any" $ do
              parseUsing "int" parseType `shouldReturn` (Just "TInt")
              parseUsing "bool" parseType `shouldReturn` (Just "TBool")
              parseUsing "char" parseType `shouldReturn` (Just "TChar")
              parseUsing "string" parseType `shouldReturn` (Just "TStr")
              parseUsing "int []" parseType `shouldReturn` (Just "TArray [TInt]")
              parseUsing "pair(int, int)" parseType `shouldReturn` (Just "TPair (TInt, TInt)")
              parseUsing "pair(pair, pair)" parseType `shouldReturn` (Just "TPair (Any, Any)")

statListTest =hspec $ do
      describe "parse statList:" $ do
          it "☑️ can parse list of stats" $ do
            parseUsing "skip;skip;skip" parseStatListF `shouldReturn`
              (Just $ "StatList [FuncStat FuncApp \"skip\" [],FuncStat FuncApp \"skip\" [],"
                    ++ "FuncStat FuncApp \"skip\" []]")
