module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Expr
import qualified Text.Parsec.Token as Token

import Lexer
import Syntax

-- First implement assignment & write & writeln
program = do
        whitespace
        statements
        eof

statements = statement >> many (reserved ";" >> statement)

statement = try variableAssignment 
         <|> try write
         <|> try writeln

write = reserved "write" >> parens (expression)

writeln = reserved "writeln"

variableAssignment = identifier >> reserved ":=" >> expression

binary symbol dataConstructor associativity = Expr.Infix (reservedOp symbol >> return (BinOp dataConstructor)) associativity
prescedenceTable = [[binary "*" Times Expr.AssocLeft],
                    [binary "+" Plus Expr.AssocLeft, binary "-" Minus Expr.AssocLeft]]

expression = Expr.buildExpressionParser prescedenceTable factor

factor = try constant
      <|> identifier

