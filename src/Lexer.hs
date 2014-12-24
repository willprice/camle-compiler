module Lexer (constant, parens, reserved, reservedOp, identifier, whitespace) where

import Text.Parsec.Char (letter, alphaNum)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Token as Token

lexer = Token.makeTokenParser camleStyle


camleStyle = emptyDef { Token.commentStart = "{"
                      , Token.commentEnd = "}"
                      , Token.reservedOpNames = reservedOperations
                      , Token.reservedNames = reservedKeywords
                      , Token.nestedComments = False
                      , Token.identStart = letter
                      , Token.identLetter = alphaNum
                      , Token.caseSensitive = False
                      }
    where reservedKeywords = ["while", "do", "if", "then", "else", "read",
                              "write", "writeln", "skip", "true", "false"]
          reservedOperations = ["+", "-", "*", ":=", "!", ";", "<=", "=", "(", ")"]

constant = Token.integer lexer

parens = Token.parens lexer

reserved = Token.reserved lexer

reservedOp = Token.reservedOp lexer

identifier = Token.identifier lexer

whitespace = Token.whiteSpace lexer
