module Compiler.SymbolTable where

import Data.Map (Map)
import qualified Data.Map as Map

import Compiler.AST

-- | The symbol table maps variable names to variable information.
type SymbolTable = Map String VariableInfo

-- | Information about a variable in the symbol table.
data VariableInfo = VariableInfo
  { variableType :: Type
  } deriving (Eq, Show)

-- | An empty symbol table.
emptySymbolTable :: SymbolTable
emptySymbolTable = Map.empty

-- | Looks up a variable in the symbol table.
lookupVariable :: String -> SymbolTable -> Maybe VariableInfo
lookupVariable = Map.lookup

-- | Inserts a variable into the symbol table.
insertVariable :: String -> VariableInfo -> SymbolTable -> SymbolTable
insertVariable = Map.insert

-- | Updates a variable in the symbol table.
updateVariable :: String -> VariableInfo -> SymbolTable -> SymbolTable
updateVariable = Map.insert

-- | Removes a variable from the symbol table.
removeVariable :: String -> SymbolTable -> SymbolTable
removeVariable = Map.delete

-- | Creates a new block in the symbol table.
enterBlock :: SymbolTable -> SymbolTable
enterBlock = id

-- | Exits the current block in the symbol table.
exitBlock :: SymbolTable -> SymbolTable
exitBlock = id
