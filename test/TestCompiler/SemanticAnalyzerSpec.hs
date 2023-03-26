module SemanticAnalyzerSpec where

import Test.Hspec
import Compiler.SemanticAnalyzer
import Compiler.SymbolTable

spec :: Spec
spec = do
  describe "semantic analysis" $ do
    it "should reject redeclaration of variables" $ do
      let prog = [
            VarDeclaration "x",
            VarDeclaration "y",
            VarDeclaration "x"
            ]
      semanticCheck prog `shouldBe` Left (RedeclarationError "x")
      
    it "should reject use of undeclared variables" $ do
      let prog = [
            Assignment "x" (IntLiteral 1)
            ]
      semanticCheck prog `shouldBe` Left (UndeclaredError "x")
      
    it "should allow declaration and assignment of variables" $ do
      let prog = [
            VarDeclaration "x",
            Assignment "x" (IntLiteral 1)
            ]
      semanticCheck prog `shouldBe` Right ()
      
    it "should reject use of uninitialized variables" $ do
      let prog = [
            VarDeclaration "x",
            Assignment "y" (IntLiteral 2)
            ]
      semanticCheck prog `shouldBe` Left (UninitializedError "x")
      
    it "should allow use of initialized variables" $ do
      let prog = [
            VarDeclaration "x",
            Assignment "x" (IntLiteral 1),
            Assignment "y" (Var "x")
            ]
      semanticCheck prog `shouldBe` Right ()
