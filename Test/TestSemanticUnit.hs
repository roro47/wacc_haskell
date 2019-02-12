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
                Just "FuncExpr FuncApp \"+\" [IntLiter 2,IntLiter 2]")
              analyzeUsing "3 - 2" parseExprF analyzeExprF `shouldReturn` (
                Just "FuncExpr FuncApp \"-\" [IntLiter 3,IntLiter 2]")
              analyzeUsing "3 * 2" parseExprF analyzeExprF `shouldReturn` (
                Just "FuncExpr FuncApp \"*\" [IntLiter 3,IntLiter 2]")
              analyzeUsing "3 / 2" parseExprF analyzeExprF `shouldReturn` (
                Just "FuncExpr FuncApp \"/\" [IntLiter 3,IntLiter 2]")

              let addSymbol = [("==",TFunc [TAny] [TInt,TInt] TBool)]
              analyzeUsingPlus "3 == 2" parseExprF analyzeExprF  addSymbol `shouldReturn` (
                Just "FuncExpr FuncApp \"==\" [IntLiter 3,IntLiter 2]")

              let addSymbol = [(">",TFunc [TAny] [TInt,TInt] TBool)]
              analyzeUsingPlus "3 > 2" parseExprF analyzeExprF addSymbol `shouldReturn` (
                Just "FuncExpr FuncApp \">\" [IntLiter 3,IntLiter 2]")

              let addSymbol = [(">=",TFunc [TAny] [TInt,TInt] TBool)]
              analyzeUsingPlus "3 >= 2" parseExprF analyzeExprF addSymbol `shouldReturn` (
                Just "FuncExpr FuncApp \">=\" [IntLiter 3,IntLiter 2]")

              let addSymbol = [("<",TFunc [TAny] [TInt,TInt] TBool)]
              analyzeUsingPlus "3 < 2" parseExprF analyzeExprF addSymbol `shouldReturn` (
                Just "FuncExpr FuncApp \"<\" [IntLiter 3,IntLiter 2]")

              let addSymbol = [("<=",TFunc [TAny] [TInt,TInt] TBool)]
              analyzeUsingPlus "3 <= 2" parseExprF analyzeExprF addSymbol `shouldReturn` (
                Just "FuncExpr FuncApp \"<=\" [IntLiter 3,IntLiter 2]")

              let addSymbol = [("!=",TFunc [TAny] [TInt,TInt] TBool)]
              analyzeUsingPlus "3 != 2" parseExprF analyzeExprF addSymbol `shouldReturn` (
                Just "FuncExpr FuncApp \"!=\" [IntLiter 3,IntLiter 2]")

          it "✖️ rejects && and || between integers" $ do
              parseUsing "2 && 3" parseIdentF `shouldReturn` Nothing
              parseUsing "2 || 23333" parseIdentF `shouldReturn` Nothing

          it "☑️ accepts some unary operations (Pos, Neg, Chr) before integers" $ do
              analyzeUsing "+ 2" parseExprF analyzeExprF `shouldReturn` (
                Just "FuncExpr FuncApp \"pos\" [IntLiter 2]")
              analyzeUsing "-3" parseExprF analyzeExprF `shouldReturn` (
                Just "FuncExpr FuncApp \"neg\" [IntLiter 3]")
              analyzeUsing "chr 23" parseExprF analyzeExprF `shouldReturn` (
                Just "FuncExpr FuncApp \"chr\" [IntLiter 23]")

          it "☑️ accepts unary operations || and && before bool" $ do
              analyzeUsing "true || false" parseExprF analyzeExprF `shouldReturn` (
                Just "FuncExpr FuncApp \"||\" [BoolLiter True,BoolLiter False]")
              analyzeUsing "true && false" parseExprF analyzeExprF `shouldReturn` (
                Just "FuncExpr FuncApp \"&&\" [BoolLiter True,BoolLiter False]")


          it "✖️ rejects all the other unary operations on integers" $ do
              analyzeUsing "len 3" parseExprF analyzeExprF `shouldReturn` Nothing
              analyzeUsing "ord 3" parseExprF analyzeExprF `shouldReturn` Nothing
              analyzeUsing "! 3" parseExprF analyzeExprF `shouldReturn` Nothing

          it "☑️ can apply unary operations ord on char" $ do
              analyzeUsing "ord 'a'" parseExprF analyzeExprF `shouldReturn` (
       	         Just "FuncExpr FuncApp \"ord\" [CharLiter 'a']")

          it "☑️ can apply unary operations chr on integer" $ do
              analyzeUsing "chr 10" parseExprF analyzeExprF `shouldReturn` (
                 Just "FuncExpr FuncApp \"chr\" [IntLiter 10]")


analyzeLiteralTest = hspec $ do
     describe "analyze Literals:" $ do
        it "☑️ can recognize char, int, bool, string" $ do
             analyzeUsing "123" term analyzeExprF `shouldReturn` (
               Just "IntLiter 123")
             analyzeUsing "'a'" term analyzeExprF `shouldReturn` (
               Just "CharLiter 'a'")
             analyzeUsing "true" term analyzeExprF `shouldReturn` (
               Just "BoolLiter True")
             analyzeUsing "\"hello world!\"" term analyzeExprF `shouldReturn` (
               Just "StringLiter \"hello world!\"")
             analyzeUsing "int a = 1; int b = a" parseStatListF analyzeStatListF `shouldReturn` (
               Just "StatList [Declare TInt \"a\" IntLiter 1,Declare TInt \"b\" IdentExpr \"a\"]")

        it "✖️ rejects invald literals" $ do
             analyzeUsing "True" term analyzeExprF `shouldReturn` Nothing
             analyzeUsing "ab" term analyzeExprF `shouldReturn` Nothing
             analyzeUsing "abab" term analyzeExprF `shouldReturn` Nothing




analyzeArrayTest = hspec $ do
     describe "analyze arrays:" $ do
       it "☑️ can recognize empty array" $ do
              analyzeUsing "[]" parseAssignRHSF analyzeExprF `shouldReturn`(
                Just "ArrayLiter []")

       it "☑️ can recognize array of char, int, bool" $ do
              analyzeUsing "[1,2,3]" parseAssignRHSF analyzeExprF `shouldReturn`(
                Just "ArrayLiter [IntLiter 1,IntLiter 2,IntLiter 3]")
              analyzeUsing "['a', 'b']" parseAssignRHSF analyzeExprF `shouldReturn`(
                Just "ArrayLiter [CharLiter 'a',CharLiter 'b']")
              analyzeUsing "[true, true, false]" parseAssignRHSF analyzeExprF `shouldReturn`(
                Just "ArrayLiter [BoolLiter True,BoolLiter True,BoolLiter False]")
       it "☑️ can recognize array of null" $ do
              analyzeUsing "[null, null]" parseAssignRHSF analyzeExprF `shouldReturn`(
                Just "ArrayLiter [Null,Null]")

       it "☑️ can apply binOp ==, != on arrays" $ do
              let addSymbol = [("==",TFunc [TAny]  [TArray TInt, TArray TInt] TBool)]
              analyzeUsingPlus "int[] a = [1] ; bool b = a == a" parseStatListF analyzeStatListF addSymbol `shouldReturn`(
                Just "StatList [Declare TArray [TInt] \"a\" ArrayLiter [IntLiter 1],Declare TBool \"b\" FuncExpr FuncApp \"==\" [IdentExpr \"a\",IdentExpr \"a\"]]")

       it "☑️ can apply uOp len on arrays" $ do
              analyzeUsing "int[] a = [1] ; int b = len a" parseStatListF analyzeStatListF `shouldReturn`(
                Just "StatList [Declare TArray [TInt] \"a\" ArrayLiter [IntLiter 1],Declare TInt \"b\" FuncExpr FuncApp \"len\" [IdentExpr \"a\"]]")

       it "☑️ can access elements in array" $ do
              analyzeUsing "int[] a = [1,2,3]; int b = a[0]" parseStatListF analyzeStatListF `shouldReturn` (
                Just "StatList [Declare TArray [TInt] \"a\" ArrayLiter [LiterExpr IntLiter 1,LiterExpr IntLiter 2,LiterExpr IntLiter 3],Declare TInt \"b\" ExprRHS ArrayExpr ArrayElem \"a\" [LiterExpr IntLiter 0]]")

       it "☑️ can modify array" $ do
              analyzeUsing "int[] a = [1,2,3]; a[0] = 0" parseStatListF analyzeStatListF `shouldReturn` (
                 Just "StatList [Declare TArray [TInt] \"a\" ArrayLiter [LiterExpr IntLiter 1,LiterExpr IntLiter 2,LiterExpr IntLiter 3],Assign ArrayElemLHS ArrayElem \"a\" [LiterExpr IntLiter 0] ExprRHS LiterExpr IntLiter 0]")
              analyzeUsing "int[] a = [1,2,3]; a = [2,3]" parseStatListF analyzeStatListF `shouldReturn` (
                Just "StatList [Declare TArray [TInt] \"a\" ArrayLiter [IntLiter 1,IntLiter 2,IntLiter 3],Assign IdentExpr \"a\" ArrayLiter [IntLiter 2,IntLiter 3]]")

       it "✖️ reject all other binOps on arrays" $ do
              analyzeUsing "int[] a = [1];int[] b = [3]; bool c = a >= b" parseStatListF analyzeStatListF `shouldReturn` Nothing

       it "✖️ reject direct array creation of pairs or in the rhs itself" $ do
              analyzeUsing "pair(int, int)[] a = [newpair(1, 3), newpair(2, 4)]" parseStatF analyzeStatF `shouldReturn` Nothing
              analyzeUsing "[newpair(1, 3), newpair(2, 4)]" parseAssignRHSF analyzeExprF `shouldReturn` Nothing

       it "✖️ rejects direct multi-dimensional array" $ do
              analyzeUsing "[[1,2], [3,4]]" parseAssignRHSF analyzeExprF `shouldReturn` Nothing

       it "✖️ rejects mixed-type arrays creation or in the rhs itself" $ do
              analyzeUsing "int[] a = [1,'a']" parseStatF analyzeStatF `shouldReturn` Nothing


--stat, statList and assignments
analyzeStatTest = hspec $ do
     describe "analyse assignment:" $ do
        it "☑️ can assign string, bool, int, array, char and pair to lhs" $ do
            analyzeUsing "string s = \"A String\"" parseStatF analyzeStatF `shouldReturn`(
              Just "Declare TStr \"s\" StringLiter \"A String\"")
            analyzeUsing "int a = 1" parseStatF analyzeStatF `shouldReturn`(
              Just "Declare TInt \"a\" IntLiter 1")
            analyzeUsing "int[] a = [1,2,3]" parseStatF analyzeStatF `shouldReturn`(
              Just "Declare TArray [TInt] \"a\" ArrayLiter [IntLiter 1,IntLiter 2,IntLiter 3]")
            analyzeUsing "pair(int, int) p = newpair(10, 3)" parseStatF analyzeStatF `shouldReturn`(
              Just "Declare TPair (TInt, TInt) \"p\" FuncExpr FuncApp \"newpair\" [IntLiter 10,IntLiter 3]")
            analyzeUsing "char c = 'c'" parseStatF analyzeStatF `shouldReturn`(
              Just "Declare TChar \"c\" CharLiter 'c'")

        it "☑️ can re-assign values to lhs" $ do
            analyzeUsing "int a = 1; a = 2" parseStatListF analyzeStatListF `shouldReturn`(
              Just "StatList [Declare TInt \"a\" IntLiter 1,Assign IdentExpr \"a\" IntLiter 2]")
            analyzeUsing "bool b = true; b = false" parseStatListF analyzeStatListF `shouldReturn`(
              Just "StatList [Declare TBool \"b\" BoolLiter True,Assign IdentExpr \"b\" BoolLiter False]")
            analyzeUsing "string s = \"hello\"; s = \"hello again\"" parseStatListF analyzeStatListF `shouldReturn`(
              Just "StatList [Declare TStr \"s\" StringLiter \"hello\",Assign IdentExpr \"s\" StringLiter \"hello again\"]")
            analyzeUsing "char c = 'a'; c = 'b'" parseStatListF analyzeStatListF `shouldReturn`(
              Just "StatList [Declare TChar \"c\" CharLiter 'a',Assign IdentExpr \"c\" CharLiter 'b']")

        it "☑️ can analyze valid statList" $ do
            analyzeUsing "if true then skip else skip fi" parseStatListF analyzeStatListF `shouldReturn` (
              Just "StatList [If BoolLiter True StatList [FuncStat FuncApp \"skip\" []] StatList [FuncStat FuncApp \"skip\" []]]")
            analyzeUsing "int c = 1;int b = c" parseStatListF analyzeStatListF `shouldReturn`(
              Just "StatList [Declare TInt \"c\" IntLiter 1,Declare TInt \"b\" IdentExpr \"c\"]")

        it "☑️ can analyze indirect creation of multidimensional array in a statlist" $ do
            analyzeUsing "int[] a = [1,2,3];int[] b = [3,4];int[][] c = [a,b]" parseStatListF analyzeStatListF  `shouldReturn`
             (Just "StatList [Declare TArray [TInt] \"a\" ArrayLiter [IntLiter 1,IntLiter 2,IntLiter 3],Declare TArray [TInt] \"b\" ArrayLiter [IntLiter 3,IntLiter 4],Declare TArray [TArray [TInt]] \"c\" ArrayLiter [IdentExpr \"a\",IdentExpr \"b\"]]")

        it "☑️ can analyze indirect creation of array of pairs in a statList" $ do
            analyzeUsing "pair(int, int) p1 = newpair(10, 3); pair(int, int) p2 = newpair(1, 5); pair(int, int)[] pl = [p1, p2]"
             parseStatListF analyzeStatListF  `shouldReturn`
             (Just "StatList [Declare TPair (TInt, TInt) \"p1\" FuncExpr FuncApp \"newpair\" [IntLiter 10,IntLiter 3],Declare TPair (TInt, TInt) \"p2\" FuncExpr FuncApp \"newpair\" [IntLiter 1,IntLiter 5],Declare TArray [TPair (TInt, TInt)] \"pl\" ArrayLiter [IdentExpr \"p1\",IdentExpr \"p2\"]]")

        it "✖️ rejects invald identifiers" $ do
            analyzeUsing "int 1a = 1" parseStatF analyzeStatF `shouldReturn` Nothing
            analyzeUsing "int null = 1" parseStatF analyzeStatF `shouldReturn` Nothing

        it "✖️ cannot re-assign values to lhs when type doesn't match" $ do
            analyzeUsing "int 1a = 1" parseStatF analyzeStatF `shouldReturn` Nothing
            analyzeUsing "int null = 1" parseStatF analyzeStatF `shouldReturn` Nothing

        it "✖️ rejects when type of rhs doesn't match the rhs" $ do
            analyzeUsing "int a = 1; a = true" parseStatListF analyzeStatListF `shouldReturn` Nothing
            analyzeUsing "char a = 'c'; a = 1" parseStatListF analyzeStatListF `shouldReturn` Nothing

        it "✖️ cannot assign null to lhs" $ do
            analyzeUsing "int n = null" parseStatF analyzeStatF `shouldReturn` Nothing

        it "✖️ rejects when identifier used before declaration" $ do
            analyzeUsing "int c = b;int b = 1" parseStatF analyzeStatF `shouldReturn` Nothing

analyzePairTest = hspec $ do
  describe "analyse pairs:" $ do
    it "☑️ can analyze pairs of mixed type" $ do
      analyzeUsingPlus "pair(int, char) p = newpair(1, 'a')" parseStatF analyzeStatF builtInFunc `shouldReturn`(
        Just "Declare TPair (TInt, TChar) \"p\" FuncExpr FuncApp \"newpair\" [IntLiter 1,CharLiter 'a']")

    it "☑️ can analyze pairs of same type" $ do
      analyzeUsing "pair(int, int) p = newpair(1, 1)" parseStatF analyzeStatF `shouldReturn`(
        Just "Declare TPair (TInt, TInt) \"p\" FuncExpr FuncApp \"newpair\" [IntLiter 1,IntLiter 1]")

    it "☑️ can access pair elements" $ do
      let addSymbol = [("fst",TFunc [TAny]  [TPair TInt TAny] TInt)]
      analyzeUsingPlus "pair(int, int) p = newpair(1, 1); int p1 = fst p" parseStatListF analyzeStatListF addSymbol `shouldReturn`(
        Just "StatList [Declare TPair (TInt, TInt) \"p\" FuncExpr FuncApp \"newpair\" [IntLiter 1,IntLiter 1],Declare TInt \"p1\" FuncExpr FuncApp \"fst\" [IdentExpr \"p\"]]")

    it "☑️ can apply binOp ==, != on pairs" $ do
       let addSymbol = [("==",TFunc [TAny]  [TPair TInt TInt, TPair TInt TInt] TBool)]
       analyzeUsingPlus "pair(int, int) p = newpair(10, 3); bool pp = p == p " parseStatListF analyzeStatListF addSymbol `shouldReturn`(
         Just "StatList [Declare TPair (TInt, TInt) \"p\" FuncExpr FuncApp \"newpair\" [IntLiter 10,IntLiter 3],Declare TBool \"pp\" FuncExpr FuncApp \"==\" [IdentExpr \"p\",IdentExpr \"p\"]]")

       let addSymbol = [("!=",TFunc [TAny]  [TPair TInt TInt, TPair TInt TInt] TBool)]
       analyzeUsingPlus "pair(int, int) p = newpair(10, 3); bool pp = p != p " parseStatListF analyzeStatListF addSymbol `shouldReturn`(
         Just "StatList [Declare TPair (TInt, TInt) \"p\" FuncExpr FuncApp \"newpair\" [IntLiter 10,IntLiter 3],Declare TBool \"pp\" FuncExpr FuncApp \"!=\" [IdentExpr \"p\",IdentExpr \"p\"]]")

    it "✖️ rejects other binOp on pairs" $ do
      analyzeUsing "pair(int, int) p = newpair(10, 3); pair(int, int) pp = p + p " parseStatListF analyzeStatListF `shouldReturn` Nothing
      analyzeUsing "pair(int, int) p = newpair(10, 3); pair(int, int) pp = p > p " parseStatListF analyzeStatListF `shouldReturn` Nothing

analyzeFuncTest = hspec $ do
  describe "analyse functions: " $ do
    it "☑️ can analyse valid fuctions" $ do
      analyzeUsing "int inc(int x) is return x + 1 end" parseFuncF analyzeFuncF `shouldReturn`
        (Just "Func TInt \"inc\" [Param TInt \"x\"] StatList [Return FuncExpr FuncApp \"+\" [IdentExpr \"x\",IntLiter 1]]")
