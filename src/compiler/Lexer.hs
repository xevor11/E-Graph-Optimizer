module Compiler.Lexer
    ( Token(..)
    , lexSource
    ) where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.List (stripPrefix)

data Token
    = TokLet
    | TokIn
    | TokIdent String
    | TokIntLit Integer
    | TokOp String
    | TokLParen
    | TokRParen
    | TokAssign
    | TokEOF
    deriving (Show, Eq)

isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_'

lexIdent :: String -> (String, String)
lexIdent s = case span isIdentChar s of
    ("let", rest) -> ("let", rest)
    ("in", rest) -> ("in", rest)
    (ident, rest) -> (ident, rest)

lexNumber :: String -> (String, String)
lexNumber s = span isDigit s

lexOp :: String -> (String, String)
lexOp s = case stripPrefix ">=" s of
    Just rest -> (">=", rest)
    Nothing -> case stripPrefix ">" s of
        Just rest -> (">", rest)
        Nothing -> case stripPrefix "<=" s of
            Just rest -> ("<=", rest)
            Nothing -> case stripPrefix "<" s of
                Just rest -> ("<", rest)
                Nothing -> case stripPrefix "==" s of
                    Just rest -> ("==", rest)
                    Nothing -> case stripPrefix "!=" s of
                        Just rest -> ("!=", rest)
                        Nothing -> ([], s)

lexToken :: String -> (Token, String)
lexToken "" = (TokEOF, "")
lexToken ('(':rest) = (TokLParen, rest)
lexToken (')':rest) = (TokRParen, rest)
lexToken ('=':'=':rest) = (TokOp "==", rest)
lexToken ('!':'=':rest) = (TokOp "!=", rest)
lexToken ('=':rest) = (TokAssign, rest)
lexToken (c:s)
    | isSpace c = lexToken s
    | isAlpha c = let (ident, rest) = lexIdent (c:s) in (case ident of
        "let" -> TokLet
        "in" -> TokIn
        _ -> TokIdent ident, rest)
    | isDigit c = let (num, rest) = lexNumber (c:s) in (TokIntLit (read num), rest)
    | otherwise = let (op, rest) = lexOp (c:s) in (TokOp op, rest)

lexSource' :: String -> [Token]
lexSource' s = case lexToken s of
    (TokEOF, "") -> []
    (tok, rest) -> tok : lexSource' rest

lexSource :: String -> [Token]
lexSource = lexSource' . filter (/= '\r')
