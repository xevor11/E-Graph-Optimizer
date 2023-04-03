module Main where

import Compiler.Lexer
import Compiler.Parser
import Compiler.SemanticAnalyzer
import Compiler.SymbolTable
import Compiler.CodeGenerator

main :: IO ()
main = do
  -- Parse command line arguments
  -- ...

  -- Read input file
  input <- readFile "input_file.txt"

  -- Lex input
  let tokens = tokenize input

  -- Parse tokens
  let ast = parse tokens

  -- Perform semantic analysis
  let analyzedAst = analyze ast

  -- Generate symbol table
  let symbolTable = generateSymbolTable analyzedAst

  -- Generate code
  let code = generateCode analyzedAst symbolTable

  -- Write output file
  writeFile "output_file.txt" code
