module Test.TestSyntaxUnit where

import Text.ParserCombinators.Parsec
import Test.Hspec
import Test.TestUtil as Util
import FrontEnd.Parser
import FrontEnd.AST

test = do
      putStrLn "*†*:;;;:*†*:;;;:*†*Syntax error test*†*:;;;:*†*:;;;:*†*"
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
              parseUsing "123" parseIntLiterF `shouldReturn` (Just "IntLiter 123")
         it "☑️ can parse negative integer" $ do
              parseUsing "-123" parseIntLiterF `shouldReturn` (Just "IntLiter (-123)")
         it "☑️ can parse positive integer with sign" $ do
              parseUsing "+123" parseIntLiterF `shouldReturn` (Just "IntLiter 123")
         it "✖️ cannot parse overflow numbers" $ do
              parseUsing "9999999999999999" parseIntLiterF `shouldReturn` Nothing

charTest = hspec $ do
      describe "parse Char:" $ do
         it "☑️ can parse charLiteral" $ do
              parseUsing "'c'" parseCharLiterF `shouldReturn` (Just "CharLiter 'c'")
         it "✖️ cannot parse escape chars without escape" $ do
              parseUsing "'\"'" parseCharLiterF `shouldReturn` Nothing
         it "☑️ can parse escape chars with escape" $ do
              parseUsing "'\\\"'" parseCharLiterF `shouldReturn` (Just "CharLiter '\"'")
         it "☑️ can parse weird control characters" $ do
              parseUsing "'\\SOH'" parseCharLiterF `shouldReturn` (Just "CharLiter '\\SOH'")

boolTest = hspec $ do
      describe "parse Bool:" $ do
         it "☑️ can parse True" $ do
              parseUsing "true" parseBoolLiterF `shouldReturn` (Just "BoolLiter True")
         it "☑️ can parse escape chars with escape" $ do
              parseUsing "false" parseBoolLiterF `shouldReturn` (Just "BoolLiter False")
         it "✖️ cannot parse uppercase bool" $ do
              parseUsing "False" parseBoolLiterF `shouldReturn` Nothing

stringTest = hspec $ do
      describe "parse String:" $ do
         it "☑️ can parse normal string" $ do
              parseUsing "\"Hello, World!\"" parseStringLiterF
              `shouldReturn` (Just "StringLiter \"Hello, World!\"")
         it "✖️ cannot parse string with incorrect escape" $ do
              parseUsing "Hello, \"World!" parseStringLiterF `shouldReturn` Nothing

arrayRHSTest = hspec $ do
      describe "parse Array:" $ do
         it "☑️ can parse empty array" $ do
              parseUsing "[]" parseArrayLiterRHSF `shouldReturn` (Just "ArrayLiter []")
         it "✖️ cannot directly parse multidimension array" $ do
              parseUsing "[[],[]]" parseArrayLiterRHSF `shouldReturn` Nothing
         it "✖️ cannot directly parse pair array" $ do
              parseUsing "[(1, 2)]" parseArrayLiterRHSF `shouldReturn` Nothing
         it "☑️ can parse many kinds of arrays..." $ do
              parseUsing "[1, 2]" parseArrayLiterRHSF `shouldReturn` (
                Just "ArrayLiter [LiterExpr IntLiter 1,LiterExpr IntLiter 2]")
              parseUsing "['a', 'b']" parseArrayLiterRHSF `shouldReturn` (
                Just "ArrayLiter [LiterExpr CharLiter 'a',LiterExpr CharLiter 'b']")
              parseUsing "[\"hello\"]" parseArrayLiterRHSF `shouldReturn` (
                Just "ArrayLiter [LiterExpr StringLiter \"hello\"]")
              parseUsing "[a]" parseArrayLiterRHSF `shouldReturn` (
                Just "ArrayLiter [IdentExpr \"a\"]")
              parseUsing "[true, true]" parseArrayLiterRHSF `shouldReturn` (
                Just "ArrayLiter [LiterExpr BoolLiter True,LiterExpr BoolLiter True]")

newpairRHSTest = hspec $ do
      describe "parse pair:" $ do
          it "☑️ can parse int pair" $ do
              parseUsing "newpair (1, 2)" parseNewPairRHSF `shouldReturn` (
                Just  "NewPair LiterExpr IntLiter 1 LiterExpr IntLiter 2")
          it "☑️ can parse pair with different types" $ do
              parseUsing "newpair (1, 'a')" parseNewPairRHSF `shouldReturn` (
                Just  "NewPair LiterExpr IntLiter 1 LiterExpr CharLiter 'a'")
          it "☑️ can parse pair with null" $ do
              parseUsing "newpair (null, null)" parseNewPairRHSF `shouldReturn` (
                Just  "NewPair LiterExpr Null LiterExpr Null")

pairElemTest = hspec $ do
      describe "parse pair elements:" $ do
          it "☑️ can parse fst" $ do
              parseUsing "fst a" parsePairElemF `shouldReturn` (
                Just  "PairElemFst IdentExpr \"a\"")
          it "☑️ can parse snd" $ do
              parseUsing "snd a" parsePairElemF `shouldReturn` (
                Just  "PairElemSnd IdentExpr \"a\"")

arrayElemTest = hspec $ do
      describe "parse array elements:" $ do
          it "☑️ can parse int index" $ do
              parseUsing "a[1]" parseArrayElemF `shouldReturn` (
                Just  "ArrayElem \"a\" [LiterExpr IntLiter 1]")
          it "☑️ can parse identifier index" $ do
              parseUsing "a[b]" parseArrayElemF `shouldReturn` (
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
                Just "UExpr Pos LiterExpr IntLiter 1")
              parseUsing "-a" parseExprF `shouldReturn` (
                Just "UExpr Neg IdentExpr \"a\"")
              parseUsing "!a" parseExprF `shouldReturn` (
                Just "UExpr Not IdentExpr \"a\"")
              parseUsing "chr 12" parseExprF `shouldReturn` (
                Just "UExpr Chr LiterExpr IntLiter 12")
              parseUsing "ord 'a'" parseExprF `shouldReturn` (
                Just "UExpr Ord LiterExpr CharLiter 'a'")
          it "✖️ cannot parse pos and neg on strings and chars" $ do
              parseUsing "+\"hello\"" parseExprF `shouldReturn` Nothing
              parseUsing "-'a'" parseExprF `shouldReturn` Nothing

binopTest = hspec $ do
      describe "parse binary operators:" $ do
          it "☑️ Mul, Div, Mod, Plus, Minus, G, GEq, L, LEq, Eq, NEq, And, Or" $ do
              parseUsing "1*1" parseExprF `shouldReturn` (
                Just "BExpr Mul LiterExpr IntLiter 1 LiterExpr IntLiter 1")
              parseUsing "2/a" parseExprF `shouldReturn` (
                Just "BExpr Div LiterExpr IntLiter 2 IdentExpr \"a\"")
              parseUsing "3%4" parseExprF `shouldReturn` (
                Just "BExpr Mod LiterExpr IntLiter 3 LiterExpr IntLiter 4")
              parseUsing "2+2" parseExprF `shouldReturn` (
                Just "BExpr Plus LiterExpr IntLiter 2 LiterExpr IntLiter 2")
              parseUsing "c-b" parseExprF `shouldReturn` (
                Just "BExpr Minus IdentExpr \"c\" IdentExpr \"b\"")
              parseUsing "a > b" parseExprF `shouldReturn` (
                Just "BExpr G IdentExpr \"a\" IdentExpr \"b\"")
              parseUsing "a >= b" parseExprF `shouldReturn` (
                Just "BExpr GEq IdentExpr \"a\" IdentExpr \"b\"")
              parseUsing "a < b" parseExprF `shouldReturn` (
                Just "BExpr L IdentExpr \"a\" IdentExpr \"b\"")
              parseUsing "a <= b" parseExprF `shouldReturn` (
                Just "BExpr LEq IdentExpr \"a\" IdentExpr \"b\"")
              parseUsing "a == b" parseExprF `shouldReturn` (
                Just "BExpr Eq IdentExpr \"a\" IdentExpr \"b\"")
              parseUsing "a != b" parseExprF `shouldReturn` (
                Just "BExpr NEq IdentExpr \"a\" IdentExpr \"b\"")
              parseUsing "a && b" parseExprF `shouldReturn` (
                Just "BExpr And IdentExpr \"a\" IdentExpr \"b\"")
              parseUsing "a || b" parseExprF `shouldReturn` (
                Just "BExpr Or IdentExpr \"a\" IdentExpr \"b\"")

bracketTest = hspec $ do
      describe "parse brackets:" $ do
          it "✖️ cannot parse empty bracket" $ do
              parseUsing "()" parseBracketExprF `shouldReturn` Nothing
          it "☑️ can parse some normal bracket" $ do
              parseUsing "(a)" parseBracketExprF `shouldReturn` (
                Just "BracketExpr IdentExpr \"a\"")
          it "☑️ can parse bracket in bracket" $ do
              parseUsing "((a))" parseBracketExprF `shouldReturn` (
                Just "BracketExpr BracketExpr IdentExpr \"a\"")
          it "✖️ cannot parse broken brackets" $ do
              parseUsing "(a" parseBracketExprF `shouldReturn` Nothing
              parseUsing "a)" parseBracketExprF `shouldReturn` Nothing
              parseUsing ")" parseBracketExprF `shouldReturn` Nothing
              parseUsing "(" parseBracketExprF `shouldReturn` Nothing

statTest = hspec $ do
      describe "parse stats:" $ do
          it "☑️ can parse skip" $ do
              parseUsing "skip" parseStatF `shouldReturn` (Just "Skip")
          it "☑️ can parse assign" $ do
              parseUsing "int a = b" parseStatF `shouldReturn` (
                Just "Declare TInt \"a\" ExprRHS IdentExpr \"b\"")
          it "☑️ can parse read" $ do
              parseUsing "read a" parseStatF `shouldReturn` (
                Just "Read IdentLHS \"a\"")
          it "☑️ can parse read" $ do
              parseUsing "free a" parseStatF `shouldReturn` (
                Just "Free IdentExpr \"a\"")
          it "☑️ can parse return" $ do
              parseUsing "return a" parseStatF `shouldReturn` (
                Just "Return IdentExpr \"a\"")
          it "☑️ can parse exit" $ do
              parseUsing "exit a" parseStatF `shouldReturn` (
                Just "Exit IdentExpr \"a\"")
          it "☑️ can parse print" $ do
              parseUsing "print a" parseStatF `shouldReturn` (
                Just "Print IdentExpr \"a\"")
          it "☑️ can parse println" $ do
              parseUsing "println a" parseStatF `shouldReturn` (
                Just "Println IdentExpr \"a\"")
          it "☑️ can parse if" $ do
              parseUsing "if true then skip else skip fi" parseStatF `shouldReturn` (
                Just "If LiterExpr BoolLiter True StatList [Skip] StatList [Skip]")
          it "☑️ can parse while" $ do
              parseUsing "while true do skip done" parseStatF `shouldReturn` (
                Just "While LiterExpr BoolLiter True StatList [Skip]")
          it "☑️ can parse Subroutine" $ do
              parseUsing "begin skip end" parseStatF `shouldReturn` (
                Just "Subroutine StatList [Skip]")

typeTest = hspec $ do
      describe "parse types:" $ do
          it "☑️ can parse int, bool, char, string, array, pair, any" $ do
              parseUsing "int" parseTypeF `shouldReturn` (Just "TInt")
              parseUsing "bool" parseTypeF `shouldReturn` (Just "TBool")
              parseUsing "char" parseTypeF `shouldReturn` (Just "TChar")
              parseUsing "string" parseTypeF `shouldReturn` (Just "TStr")
              parseUsing "int []" parseTypeF `shouldReturn` (Just "TArray [TInt]")
              parseUsing "pair(int, int)" parseTypeF `shouldReturn` (Just "TPair (TInt, TInt)")
              parseUsing "pair(pair, pair)" parseTypeF `shouldReturn` (Just "TPair (Any, Any)")

statListTest =hspec $ do
      describe "parse statList:" $ do
          it "☑️ can parse list of stats" $ do
            parseUsing "skip;skip;skip" parseStatListF `shouldReturn` (Just "StatList [Skip,Skip,Skip]")
