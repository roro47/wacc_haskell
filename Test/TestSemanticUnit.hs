module Test.TestSemanticUnit where

import Text.ParserCombinators.Parsec
import Test.Hspec
import Test.TestUtil as Util
import FrontEnd.Parser
import FrontEnd.AST
import FrontEnd.SemanticAnalyzer

test = do
       putStrLn "*†*:;;;:*†*:;;;:*†*Semantic error test*†*:;;;:*†*:;;;:*†*"
       analyzeExprTest
       analyzeLiteralTest
       analyzeArrayTest
       analyzeStatTest
       analyzePairTest
       analyzeFuncTest


analyzeExprTest = hspec $ do
      describe "analyze expressions:" $ do
          it "☑️ accepts all binary operations (except || and &&) on integers" $ do
              analyzeUsing "2 + 2" parseExprF analyzeExprF `shouldReturn` (
                Just "BExpr Plus LiterExpr IntLiter 2 LiterExpr IntLiter 2")
              analyzeUsing "3 - 2" parseExprF analyzeExprF `shouldReturn` (
                Just "BExpr Minus LiterExpr IntLiter 3 LiterExpr IntLiter 2")
              analyzeUsing "3 * 2" parseExprF analyzeExprF `shouldReturn` (
                Just "BExpr Mul LiterExpr IntLiter 3 LiterExpr IntLiter 2")
              analyzeUsing "3 / 2" parseExprF analyzeExprF `shouldReturn` (
                Just "BExpr Div LiterExpr IntLiter 3 LiterExpr IntLiter 2")
              analyzeUsing "3 == 2" parseExprF analyzeExprF `shouldReturn` (
                Just "BExpr Eq LiterExpr IntLiter 3 LiterExpr IntLiter 2")
              analyzeUsing "3 > 2" parseExprF analyzeExprF `shouldReturn` (
                Just "BExpr G LiterExpr IntLiter 3 LiterExpr IntLiter 2")
              analyzeUsing "3 >= 2" parseExprF analyzeExprF `shouldReturn` (
                Just "BExpr GEq LiterExpr IntLiter 3 LiterExpr IntLiter 2")
              analyzeUsing "3 < 2" parseExprF analyzeExprF `shouldReturn` (
                Just "BExpr L LiterExpr IntLiter 3 LiterExpr IntLiter 2")
              analyzeUsing "3 <= 2" parseExprF analyzeExprF `shouldReturn` (
                Just "BExpr LEq LiterExpr IntLiter 3 LiterExpr IntLiter 2")
              analyzeUsing "3 != 2" parseExprF analyzeExprF `shouldReturn` (
                Just "BExpr NEq LiterExpr IntLiter 3 LiterExpr IntLiter 2")

          it "✖️ rejects && and || between integers" $ do
              parseUsing "2 && 3" parseIdentF `shouldReturn` Nothing
              parseUsing "2 || 23333" parseIdentF `shouldReturn` Nothing

          it "☑️ accepts some unary operations (Pos, Neg, Chr) before integers" $ do
              analyzeUsing "+ 2" parseExprF analyzeExprF `shouldReturn` (
                Just "UExpr Pos LiterExpr IntLiter 2")
              analyzeUsing "-3" parseExprF analyzeExprF `shouldReturn` (
                Just "UExpr Neg LiterExpr IntLiter 3")
              analyzeUsing "chr 23" parseExprF analyzeExprF `shouldReturn` (
                Just "UExpr Chr LiterExpr IntLiter 23")

          it "✖️ rejects all the other unary operations on integers" $ do
              analyzeUsing "len 3" parseExprF analyzeExprF `shouldReturn` Nothing
              analyzeUsing "ord 3" parseExprF analyzeExprF `shouldReturn` Nothing
              analyzeUsing "! 3" parseExprF analyzeExprF `shouldReturn` Nothing

          it "☑️ can apply unary operations ord on char" $ do
              analyzeUsing "ord 'a'" parseExprF analyzeExprF `shouldReturn` (
       	         Just "UExpr Ord LiterExpr CharLiter 'a'")

          it "☑️ can apply unary operations chr on integer" $ do
              analyzeUsing "chr 10" parseExprF analyzeExprF `shouldReturn` (
                 Just "UExpr Chr LiterExpr IntLiter 10")


analyzeLiteralTest = hspec $ do
     describe "analyze Literals:" $ do
        it "☑️ can recognize char, int, bool, string" $ do
             analyzeUsing "123" parseIntLiterF analyzeLiterF `shouldReturn` (
               Just "IntLiter 123")
             analyzeUsing "'a'" parseCharLiterF analyzeLiterF `shouldReturn` (
               Just "CharLiter 'a'")
             analyzeUsing "true" parseBoolLiterF analyzeLiterF `shouldReturn` (
               Just "BoolLiter True")
             analyzeUsing "\"hello world!\"" parseStringLiterF analyzeLiterF `shouldReturn` (
               Just "StringLiter \"hello world!\"")

        it "✖️ rejects invaldliterals" $ do
             analyzeUsing "ab" parseLiterF analyzeLiterF `shouldReturn` Nothing
             analyzeUsing "abab" parseLiterF analyzeLiterF `shouldReturn` Nothing
             -- analyzeUsing "1a2b" parseLiterF analyzeLiterF `shouldReturn` Nothing



analyzeArrayTest = hspec $ do
     describe "analyze arrays:" $ do
       it "☑️ can recognize empty array" $ do
              analyzeUsing "[]" parseArrayLiterRHSF analyzeAssignRHSF `shouldReturn`(
                Just "ArrayLiter []")

       it "☑️ can recognize array of char, int, bool" $ do
              analyzeUsing "[1,2,3]" parseArrayLiterRHSF analyzeAssignRHSF `shouldReturn`(
                Just "ArrayLiter [LiterExpr IntLiter 1,LiterExpr IntLiter 2,LiterExpr IntLiter 3]")
              analyzeUsing "['a', 'b']" parseArrayLiterRHSF analyzeAssignRHSF `shouldReturn`(
                Just "ArrayLiter [LiterExpr CharLiter 'a',LiterExpr CharLiter 'b']")
              analyzeUsing "[true, true, false]" parseArrayLiterRHSF analyzeAssignRHSF `shouldReturn`(
                Just "ArrayLiter [LiterExpr BoolLiter True,LiterExpr BoolLiter True,LiterExpr BoolLiter False]")
       it "☑️ can recognize array of null" $ do
              analyzeUsing "[null, null]" parseArrayLiterRHSF analyzeAssignRHSF `shouldReturn`(
                Just "ArrayLiter [LiterExpr Null,LiterExpr Null]")

       -- it "☑️ can apply ==, != on arrays" $ do------------------------
       --        analyzeUsing "int[] a = [1];int[] b = [3]; bool t = a == b" parseStatListF analyzeStatListF `shouldReturn`(
       --          Just "StatList [Declare TArray [TInt] \"a\" ArrayLiter [LiterExpr IntLiter 1],Declare TArray [TInt] \"b\" ArrayLiter [LiterExpr IntLiter 3]]")

       it "✖️ reject all other binOps on arrays" $ do
              analyzeUsing "int[] a = [1];int[] b = [3]; a >= b" parseStatListF analyzeStatListF `shouldReturn` (
                Just "StatList [Declare TArray [TInt] \"a\" ArrayLiter [LiterExpr IntLiter 1],Declare TArray [TInt] \"b\" ArrayLiter [LiterExpr IntLiter 3]]")

       it "✖️ reject direct array creation of pairs or in the rhs itself" $ do
              analyzeUsing "pair(int, int)[] a = [newpair(1, 3), newpair(2, 4)]" parseStatF analyzeStatF `shouldReturn` Nothing
              analyzeUsing "[newpair(1, 3), newpair(2, 4)]" parseArrayLiterRHSF analyzeAssignRHSF `shouldReturn` Nothing

       it "✖️ rejects direct multi-dimensional array" $ do
              analyzeUsing "[[1,2], [3,4]]" parseArrayLiterRHSF analyzeAssignRHSF `shouldReturn` Nothing

       it "✖️ rejects mixed-type arrays creation or in the rhs itself" $ do
              analyzeUsing "int[] a = [1,'a']" parseStatF analyzeStatF `shouldReturn` Nothing


--stat, statList and assignments
analyzeStatTest = hspec $ do
     describe "analyse assignment:" $ do
        it "☑️ can assign string, bool, int, array, char and pair to lhs" $ do
            analyzeUsing "string s = \"A String\"" parseStatF analyzeStatF `shouldReturn`(
              Just "Declare TStr \"s\" ExprRHS LiterExpr StringLiter \"A String\"")
            analyzeUsing "int a = 1" parseStatF analyzeStatF `shouldReturn`(
              Just "Declare TInt \"a\" ExprRHS LiterExpr IntLiter 1")
            analyzeUsing "int[] a = [1,2,3]" parseStatF analyzeStatF `shouldReturn`(
              Just "Declare TArray [TInt] \"a\" ArrayLiter [LiterExpr IntLiter 1,LiterExpr IntLiter 2,LiterExpr IntLiter 3]")
            analyzeUsing "pair(int, int) p = newpair(10, 3)" parseStatF analyzeStatF `shouldReturn`(
              Just "Declare TPair (TInt, TInt) \"p\" NewPair LiterExpr IntLiter 10 LiterExpr IntLiter 3")
            analyzeUsing "char c = 'c'" parseStatF analyzeStatF `shouldReturn`(
                Just "Declare TChar \"c\" ExprRHS LiterExpr CharLiter 'c'")


        it "☑️ can assign null to lhs" $ do
            analyzeUsing "int n = null" parseStatF analyzeStatF `shouldReturn`(
              Just "Declare TInt \"n\" ExprRHS LiterExpr Null")

        it "☑️ can analyze valid statList" $ do
            analyzeUsing "if true then skip else skip fi" parseStatListF analyzeStatListF `shouldReturn` (
              Just "StatList [If LiterExpr BoolLiter True StatList [Skip] StatList [Skip]]")
            analyzeUsing "int c = 1;int b = c" parseStatListF analyzeStatListF `shouldReturn`(
              Just "StatList [Declare TInt \"c\" ExprRHS LiterExpr IntLiter 1,Declare TInt \"b\" ExprRHS IdentExpr \"c\"]")

        it "☑️ can analyze indirect creation of multidimensional array  in a statlist" $ do
            analyzeUsing "int[] a = [1,2,3];int[] b = [3,4];int[][] c = [a,b]" parseStatListF analyzeStatListF  `shouldReturn`
             (Just "StatList [Declare TArray [TInt] \"a\" ArrayLiter [LiterExpr IntLiter 1,LiterExpr IntLiter 2,LiterExpr IntLiter 3],Declare TArray [TInt] \"b\" ArrayLiter [LiterExpr IntLiter 3,LiterExpr IntLiter 4],Declare TArray [TArray [TInt]] \"c\" ArrayLiter [IdentExpr \"a\",IdentExpr \"b\"]]")

        it "☑️ can analyze indirect creation of array of pairs in a statList" $ do
            analyzeUsing "pair(int, int) p1 = newpair(10, 3); pair(int, int) p2 = newpair(1, 5); pair(int, int)[] pl = [p1, p2]"
             parseStatListF analyzeStatListF  `shouldReturn`
             (Just "StatList [Declare TPair (TInt, TInt) \"p1\" NewPair LiterExpr IntLiter 10 LiterExpr IntLiter 3,Declare TPair (TInt, TInt) \"p2\" NewPair LiterExpr IntLiter 1 LiterExpr IntLiter 5,Declare TArray [TPair (TInt, TInt)] \"pl\" ArrayLiter [IdentExpr \"p1\",IdentExpr \"p2\"]]")

        it "✖️ rejects invalid identifier name" $ do
            analyzeUsing "int 1a = 10" parseStatF analyzeStatF `shouldReturn` Nothing

        it "✖️ rejects when type of rhs doesn't match the rhs" $ do
            analyzeUsing "int a = true" parseStatF analyzeStatF `shouldReturn` Nothing
            analyzeUsing "char a = t123" parseStatF analyzeStatF `shouldReturn` Nothing

        it "✖️ rejects when identifier used before declaration" $ do
            analyzeUsing "int c = b;int b = 1" parseStatF analyzeStatF `shouldReturn` Nothing

analyzePairTest = hspec $ do
  describe "analyse pairs:" $ do
    it "☑️ can analyze pairs of mixed type" $ do
      analyzeUsing "pair(int, char) p = newpair(1, 'a')" parseStatF analyzeStatF `shouldReturn`(
        Just "Declare TPair (TInt, TChar) \"p\" NewPair LiterExpr IntLiter 1 LiterExpr CharLiter 'a'")

    it "☑️ can analyze pairs of same type" $ do
      analyzeUsing "pair(int, int) p = newpair(1, 1)" parseStatF analyzeStatF `shouldReturn`(
        Just "Declare TPair (TInt, TInt) \"p\" NewPair LiterExpr IntLiter 1 LiterExpr IntLiter 1")

    -- it "☑️ can apply binOp ==, != on pairs" $ do-------------------
    --   analyzeUsing "pair(int, int) p = newpair(10, 3); p == p " parseStatListF analyzeStatListF `shouldReturn`(
    --     Just "Declare TPair (TInt, TInt) \"p\" NewPair LiterExpr IntLiter 10 LiterExpr IntLiter 3")

    it "✖️ rejects other binOp on pairs" $ do--error stat not analyzed
      analyzeUsing "pair(int, int) p = newpair(10, 3); p + p " parseStatListF analyzeStatListF `shouldReturn` (
        Just "StatList [Declare TPair (TInt, TInt) \"p\" NewPair LiterExpr IntLiter 10 LiterExpr IntLiter 3]")

analyzeFuncTest = hspec $ do
  describe "analyse functions: " $ do
    it "☑️ can analyse valid fuctions" $ do
      analyzeUsing "int inc(int x) is return x + 1 end" parseFuncF analyzeFuncF `shouldReturn`
        (Just "Func TInt \"inc\" [Param TInt \"x\"] StatList [Return BExpr Plus IdentExpr \"x\" LiterExpr IntLiter 1]")

--TODO
--pair
--operators
-- analyzeFuncF
--arrayelem
-- analyzeAssignLHSF
-- analyzePairElemF
-- analyzeAssignRHSF
