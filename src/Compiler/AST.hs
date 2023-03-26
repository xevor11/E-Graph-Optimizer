-- AST.hs

module Compiler.AST where

-- The top-level AST node for the program
data Program = Program [Function]

-- A function definition
data Function = Function String [Parameter] Block

-- A function parameter
data Parameter = Parameter String

-- A block of statements
data Block = Block [Statement]

-- A statement
data Statement
  = Assignment String Expression
  | If Expression Block Block
  | While Expression Block
  | Return Expression
  | ExpressionStatement Expression

-- An expression
data Expression
  = LiteralInt Int
  | LiteralFloat Float
  | LiteralString String
  | Identifier String
  | BinOp BinOp Expression Expression
  | UnOp UnOp Expression

-- Binary operators
data BinOp
  = Add
  | Sub
  | Mul
  | Div
  | Mod
  | Eq
  | Neq
  | Lt
  | Leq
  | Gt
  | Geq
  | And
  | Or

-- Unary operators
data UnOp
  = Not
  | Neg
