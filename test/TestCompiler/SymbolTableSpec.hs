module TestCompiler.SymbolTableSpec where

import Test.Hspec
import Compiler.SymbolTable

spec :: Spec
spec = do
  describe "Symbol Table" $ do
    it "should add a symbol to the symbol table" $ do
      let initialSymbolTable = emptySymbolTable
      let symbol = Symbol "x" 0
      let symbolTable = addSymbol initialSymbolTable symbol
      lookupSymbol symbolTable "x" `shouldBe` Just 0

    it "should handle symbol shadowing" $ do
      let initialSymbolTable = emptySymbolTable
      let symbol1 = Symbol "x" 0
      let symbol2 = Symbol "x" 1
      let symbolTable = addSymbol (addSymbol initialSymbolTable symbol1) symbol2
      lookupSymbol symbolTable "x" `shouldBe` Just 1

    it "should return Nothing when symbol not found in symbol table" $ do
      let initialSymbolTable = emptySymbolTable
      lookupSymbol initialSymbolTable "y" `shouldBe` Nothing
