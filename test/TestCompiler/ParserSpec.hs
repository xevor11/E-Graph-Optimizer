module ParserSpec where

import Test.Hspec
import Compiler.Parser

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "parseExpr" $ do
  it "parses a simple expression" $ do
    let input = "2 + 3"
    parseExpr input `shouldBe` (BinOpExpr AddOp (IntExpr 2) (IntExpr 3))

  it "parses an expression with operator precedence" $ do
    let input = "2 + 3 * 4"
    parseExpr input `shouldBe` (BinOpExpr AddOp (IntExpr 2) (BinOpExpr MulOp (IntExpr 3) (IntExpr 4)))

  it "parses a function call" $ do
    let input = "foo(2, 3)"
    parseExpr input `shouldBe` (FnCallExpr "foo" [(IntExpr 2), (IntExpr 3)])
