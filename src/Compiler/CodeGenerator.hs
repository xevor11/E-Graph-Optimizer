{-# LANGUAGE OverloadedStrings #-}

module Compiler.CodeGenerator where

import Compiler.AST
import Compiler.SymbolTable

import Control.Monad.Except
import Control.Monad.State
import qualified Data.ByteString.Char8 as B
import Data.Map (Map)
import qualified Data.Map as Map

-- | The code generation error type.
data CodeGenerationError = UndefinedVariableError String
                         | UndefinedFunctionError String
                         | WrongNumberOfArgumentsError String Int Int
                         deriving (Eq, Show)

-- | The type of the code generation context.
data CodeGenerationContext = CodeGenerationContext
  { symbolTable :: SymbolTable
  , labelCounter :: Int
  , generatedCode :: [B.ByteString]
  } deriving (Eq, Show)

-- | The initial code generation context.
initialContext :: CodeGenerationContext
initialContext = CodeGenerationContext emptySymbolTable 0 []

-- | A monad for performing code generation.
type CodeGenerator = ExceptT CodeGenerationError (State CodeGenerationContext)

-- | Generates code for a program and returns either an error or the generated code.
generateCode :: Program -> Either CodeGenerationError B.ByteString
generateCode p = evalState (runExceptT (generateCode' p)) initialContext

-- | Generates code for a program and returns either an error or the generated code.
generateCode' :: Program -> CodeGenerator B.ByteString
generateCode' (Program stmts) = do
  emit ".data"
  emitGlobalVariables
  emit ".text"
  emitFunctionDefinitions
  return $ B.concat $ reverse $ generatedCode initialContext
  where
    (globals, functions) = partitionDeclarations stmts
    functionMap = Map.fromList $ map functionToTuple functions
    emitGlobalVariables = mapM_ emitGlobalVariable globals
    emitFunctionDefinitions = mapM_ (emitFunctionDefinition functionMap) functions

-- | Emits code for a global variable.
emitGlobalVariable :: VariableDeclaration -> CodeGenerator ()
emitGlobalVariable (VariableDeclaration name expr) = do
  emit $ name <> ":"
  emit $ "    .word " <> (show $ evalConstantExpression expr)

-- | Emits code for a function definition.
emitFunctionDefinition :: Map String FunctionDeclaration -> FunctionDeclaration -> CodeGenerator ()
emitFunctionDefinition functionMap (FunctionDeclaration name args returnType body) = do
  emit $ name <> ":"
  modify $ \ctx -> ctx { labelCounter = 0 }
  emitPrologue (length args)
  mapM_ defineArgument args
  _ <- generateCode' body
  emitEpilogue returnType
  where
    defineArgument (VariableDeclaration name _) = do
      offset <- gets $ Map.lookup name . symbolTable
      case offset of
        Just offset -> emit $ "    sw $a" <> show offset <> ", -" <> show (offset*4) <> "($sp)"
        Nothing -> throwError $ UndefinedVariableError name
    emitPrologue numArgs = do
      emit "    sw $ra, -4($sp)"
      emit "    sw $fp, -8($sp)"
      emit "    addiu $fp, $sp, -8"
      emit $ "    addiu $sp, $sp, -" <> (show $ (numArgs+2)*4)
      emit "    sw $a0, 0($fp)"
      emit "    sw $a1, -4($fp)"
      emit "    sw $a2, -8($fp)"
      emit "    sw $a3, -12($fp)"
    emitEpilogue returnType = do
      emit "    lw $ra, -4($fp)"
			emit "    lw $fp, -8($fp)"
      emit "    jr $ra"
		