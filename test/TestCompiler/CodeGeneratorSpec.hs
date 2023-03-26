module TestCompiler.CodeGeneratorSpec where

import Test.Hspec
import Compiler.CodeGenerator

spec :: Spec
spec = describe "CodeGenerator" $ do
  describe "generateCode" $ do
    it "generates correct code for an integer constant" $
      generateCode (IntConst 42) `shouldBe` [PushInt 42]
    
    it "generates correct code for a binary expression" $
      generateCode (BinaryOp Plus (IntConst 1) (IntConst 2)) `shouldBe` [PushInt 1, PushInt 2, Add]

    it "generates correct code for a function call" $
      generateCode (FuncCall "foo" [IntConst 1, IntConst 2]) `shouldBe` [PushInt 1, PushInt 2, Call "foo" 2]
