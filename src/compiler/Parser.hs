module Compiler.Parser (parse) where

import Compiler.AST
import Compiler.Lexer
import Control.Applicative ((<|>))
import Text.Parsec hiding (parse)
import Text.Parsec.Expr

-- | Parses the input source code and returns an AST.
parse :: String -> Either ParseError Program
parse input = runParser program () "" input

-- | Parses a program.
program :: Parsec String () Program
program = whiteSpace *> many (statement <* semi) <* eof

-- | Parses a statement.
statement :: Parsec String () Stmt
statement =
  ifStmt <|> whileStmt <|> returnStmt <|> printStmt <|> assignmentStmt

-- | Parses an if statement.
ifStmt :: Parsec String () Stmt
ifStmt = do
  reserved "if"
  cond <- parens expr
  thenBlock <- braces (many statement)
  elseBlock <- option [] (reserved "else" *> braces (many statement))
  return $ If cond thenBlock elseBlock

-- | Parses a while statement.
whileStmt :: Parsec String () Stmt
whileStmt = do
  reserved "while"
  cond <- parens expr
  block <- braces (many statement)
  return $ While cond block

-- | Parses a return statement.
returnStmt :: Parsec String () Stmt
returnStmt = do
  reserved "return"
  e <- optionMaybe expr
  return $ Return e

-- | Parses a print statement.
printStmt :: Parsec String () Stmt
printStmt = do
  reserved "print"
  e <- expr
  return $ Print e

-- | Parses an assignment statement.
assignmentStmt :: Parsec String () Stmt
assignmentStmt = do
  var <- identifier
  reservedOp "="
  e <- expr
  return $ Assign var e

-- | Parses an expression.
expr :: Parsec String () Expr
expr = buildExpressionParser table factor

-- | Operator table for expression parsing.
table :: [[Operator String () Expr]]
table =
  [ [Prefix (reservedOp "-" >> return Neg)]
  , [Infix (reservedOp "*" >> return Mult) AssocLeft, Infix (reservedOp "/" >> return Div) AssocLeft]
  , [Infix (reservedOp "+" >> return Add) AssocLeft, Infix (reservedOp "-" >> return Sub) AssocLeft]
  ]

-- | Parses a factor.
factor :: Parsec String () Expr
factor =
  parens expr
    <|> try (Var <$> identifier)
    <|> (Lit . IntVal <$> integer)
    <|> (Lit . BoolVal <$> bool)
    <?> "factor"

-- | Parses a boolean value.
bool :: Parsec String () Bool
bool = (reserved "true" >> return True) <|> (reserved "false" >> return False)
