module Compiler.SemanticAnalyzer where

import Compiler.AST
import Compiler.SymbolTable

import Control.Monad.Except
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map

-- | The semantic analysis error type.
data SemanticError
  = VariableRedefinitionError String
  | UndefinedVariableError String
  | UndefinedFunctionError String
  | ArgumentMismatchError String Int Int
  | ReturnTypeMismatchError String Type Type
  deriving (Eq, Show)

-- | The type of the semantic analysis context.
data SemanticContext = SemanticContext
  { symbolTable :: SymbolTable
  , currentFunction :: Maybe FunctionDeclaration
  } deriving (Eq, Show)

-- | The initial semantic analysis context.
initialContext :: SemanticContext
initialContext = SemanticContext emptySymbolTable Nothing

-- | A monad for performing semantic analysis.
type SemanticAnalyzer = ExceptT SemanticError (State SemanticContext)

-- | Analyzes a program and returns either an error or a new program with types annotated.
analyzeProgram :: Program -> Either SemanticError Program
analyzeProgram p = evalState (runExceptT (analyzeProgram' p)) initialContext

-- | Analyzes a program and returns either an error or a new program with types annotated.
analyzeProgram' :: Program -> SemanticAnalyzer Program
analyzeProgram' (Program stmts) = do
  stmts' <- mapM analyzeStatement stmts
  return (Program stmts')

-- | Analyzes a statement and returns either an error or a new statement with types annotated.
analyzeStatement :: Statement -> SemanticAnalyzer Statement
analyzeStatement (ExpressionStatement expr) = do
  _ <- analyzeExpression expr
  return (ExpressionStatement expr)
analyzeStatement (DeclarationStatement varDecls) = do
  mapM_ analyzeVariableDeclaration varDecls
  return (DeclarationStatement varDecls)
analyzeStatement (ReturnStatement expr) = do
  currentFunc <- getCurrentFunction
  case currentFunc of
    Just f -> do
      returnType <- analyzeExpression expr
      when (returnType /= functionReturnType f) $
        throwError (ReturnTypeMismatchError (functionName f) (functionReturnType f) returnType)
      return (ReturnStatement expr)
    Nothing -> throwError (SemanticError "Return statement not allowed outside of function")
analyzeStatement (IfStatement expr ifStmts elseStmts) = do
  condType <- analyzeExpression expr
  when (condType /= BooleanType) $
    throwError (SemanticError "If condition must be of boolean type")
  ifStmts' <- mapM analyzeStatement ifStmts
  elseStmts' <- mapM analyzeStatement elseStmts
  return (IfStatement expr ifStmts' elseStmts')
analyzeStatement (WhileStatement expr stmts) = do
  condType <- analyzeExpression expr
  when (condType /= BooleanType) $
    throwError (SemanticError "While condition must be of boolean type")
  stmts' <- mapM analyzeStatement stmts
  return (WhileStatement expr stmts')
analyzeStatement (ForStatement init expr update stmts) = do
  analyzeStatement init
  condType <- analyzeExpression expr
  when (condType /= BooleanType) $
    throwError (SemanticError "For condition must be of boolean type")
  analyzeStatement update
  stmts' <- mapM analyzeStatement stmts
  return (ForStatement init expr update stmts')

-- | Analyzes a variable declaration and adds it to the symbol table.
analyzeVariableDeclaration :: VariableDeclaration -> SemanticAnalyzer ()
analyzeVariableDeclaration (VariableDeclaration name expr) = do
  currentSymTab <- getSymbolTable
-- | Analyzes a variable declaration and adds it to the symbol table.
analyzeVariableDeclaration :: VariableDeclaration -> SemanticAnalyzer ()
analyzeVariableDeclaration (VariableDeclaration name expr) = do
  currentSymTab <- getSymbolTable
  when (name `Map.member` currentSymTab) $
    throwError (VariableRedefinitionError name)
  exprType <- analyzeExpression expr
  modifySymbolTable (Map.insert name exprType)

-- | Analyzes an expression and returns its type.
analyzeExpression :: Expression -> SemanticAnalyzer Type
analyzeExpression (LiteralExpression lit) = return (getLiteralType lit)
analyzeExpression (VariableExpression name) = do
  currentSymTab <- getSymbolTable
  case Map.lookup name currentSymTab of
    Just t -> return t
    Nothing -> throwError (UndefinedVariableError name)
analyzeExpression (AssignmentExpression name expr) = do
  currentSymTab <- getSymbolTable
  when (name `Map.notMember` currentSymTab) $
    throwError (UndefinedVariableError name)
  exprType <- analyzeExpression expr
  when (exprType /= currentSymTab Map.! name) $
    throwError (SemanticError "Type mismatch in assignment expression")
  return exprType
analyzeExpression (BinaryExpression op left right) = do
  leftType <- analyzeExpression left
  rightType <- analyzeExpression right
  case (op, leftType, rightType) of
    (AddOp, IntType, IntType) -> return IntType
    (SubOp, IntType, IntType) -> return IntType
    (MulOp, IntType, IntType) -> return IntType
    (DivOp, IntType, IntType) -> return IntType
    (ModOp, IntType, IntType) -> return IntType
    (EqOp, t1, t2) | t1 == t2 -> return BooleanType
    (NeOp, t1, t2) | t1 == t2 -> return BooleanType
    (LtOp, IntType, IntType) -> return BooleanType
    (LeOp, IntType, IntType) -> return BooleanType
    (GtOp, IntType, IntType) -> return BooleanType
    (GeOp, IntType, IntType) -> return BooleanType
    _ -> throwError (SemanticError "Invalid binary expression")
analyzeExpression (UnaryExpression op expr) = do
  exprType <- analyzeExpression expr
  case (op, exprType) of
    (NotOp, BooleanType) -> return BooleanType
    (NegOp, IntType) -> return IntType
    _ -> throwError (SemanticError "Invalid unary expression")
analyzeExpression (FunctionCallExpression name args) = do
  currentSymTab <- getSymbolTable
  case Map.lookup name currentSymTab of
    Just (FunctionType argTypes returnType) -> do
      let numArgs = length args
      when (length argTypes /= numArgs) $
        throwError (ArgumentMismatchError name (length argTypes) numArgs)
      argTypes' <- mapM analyzeExpression args
      when (argTypes' /= argTypes) $
        throwError (SemanticError "Argument types do not match function signature")
      return returnType
    _ -> throwError (UndefinedFunctionError name)

