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
          it "✖️ ... and rejects all the other unary operations" $ do
              analyzeUsing "len 3" parseExprF analyzeExprF `shouldReturn` Nothing
              analyzeUsing "ord 3" parseExprF analyzeExprF `shouldReturn` Nothing
              analyzeUsing "! 3" parseExprF analyzeExprF `shouldReturn` Nothing
