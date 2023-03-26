module TestCompiler.OptimizerSpec where

import Test.Hspec
import Compiler.Optimizer
import Egg

spec :: Spec
spec = describe "Optimizer" $ do
  describe "optimizeExpr" $ do
    it "should simplify addition of zero" $ do
      let inputExpr = makeExpr "+" [makeExpr "num" [EVal 5], makeExpr "num" [EVal 0]]
      let expectedExpr = makeExpr "num" [EVal 5]
      optimizeExpr inputExpr `shouldBe` expectedExpr

    it "should simplify multiplication by one" $ do
      let inputExpr = makeExpr "*" [makeExpr "num" [EVal 5], makeExpr "num" [EVal 1]]
      let expectedExpr = makeExpr "num" [EVal 5]
      optimizeExpr inputExpr `shouldBe` expectedExpr

    it "should simplify division by one" $ do
      let inputExpr = makeExpr "/" [makeExpr "num" [EVal 5], makeExpr "num" [EVal 1]]
      let expectedExpr = makeExpr "num" [EVal 5]
      optimizeExpr inputExpr `shouldBe` expectedExpr

    it "should simplify multiplication by zero" $ do
      let inputExpr = makeExpr "*" [makeExpr "num" [EVal 5], makeExpr "num" [EVal 0]]
      let expectedExpr = makeExpr "num" [EVal 0]
      optimizeExpr inputExpr `shouldBe` expectedExpr

    it "should simplify negation of zero" $ do
      let inputExpr = makeExpr "-" [makeExpr "num" [EVal 0]]
      let expectedExpr = makeExpr "num" [EVal 0]
      optimizeExpr inputExpr `shouldBe` expectedExpr

    it "should simplify negation of negation" $ do
      let inputExpr = makeExpr "-" [makeExpr "-" [makeExpr "num" [EVal 5]]]
      let expectedExpr = makeExpr "num" [EVal 5]
      optimizeExpr inputExpr `shouldBe` expectedExpr

    it "should simplify addition of negative numbers" $ do
      let inputExpr = makeExpr "+" [makeExpr "num" [EVal 5], makeExpr "-" [makeExpr "num" [EVal 3]]]
      let expectedExpr = makeExpr "num" [EVal 2]
      optimizeExpr inputExpr `shouldBe` expectedExpr

makeExpr :: String -> [ENode] -> Expr
makeExpr name args = Expr (Symbol name) args
