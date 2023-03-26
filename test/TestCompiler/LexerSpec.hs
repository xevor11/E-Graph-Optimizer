module TestCompiler.LexerSpec where

import Test.Hspec
import Compiler.Lexer

spec :: Spec
spec = describe "lexer" $ do
  it "lexes a simple program" $ do
    let input = "var x = 42;"
    let expectedOutput = [Keyword "var", Identifier "x", Operator "=", Number 42.0, Operator ";"]
    lexer input `shouldBe` Right expectedOutput
    
  it "lexes a program with a string literal" $ do
    let input = "\"Hello, world!\""
    let expectedOutput = [StringLiteral "Hello, world!"]
    lexer input `shouldBe` Right expectedOutput

  it "returns an error on invalid input" $ do
    let input = "$$$"
    lexer input `shouldSatisfy` isLeft
