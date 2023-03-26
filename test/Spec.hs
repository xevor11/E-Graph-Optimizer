import Test.Hspec (hspec, describe)

import TestCompiler.LexerSpec
import TestCompiler.ParserSpec
import TestCompiler.SemanticAnalyzerSpec
import TestCompiler.SymbolTableSpec
import TestCompiler.CodeGeneratorSpec
import TestCompiler.OptimizerSpec

main :: IO ()
main = hspec $ do
    describe "Lexer" lexerSpec
    describe "Parser" parserSpec
    describe "SemanticAnalyzer" semanticAnalyzerSpec
    describe "SymbolTable" symbolTableSpec
    describe "CodeGenerator" codeGeneratorSpec
    describe "Optimizer" optimizerSpec
