module CamleCompiler.Parser where

import Text.Parsec hiding (parse, string)
import Text.Parsec.String (Parser)

import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Expr as Expr

import CamleCompiler.Lexer
import CamleCompiler.AST

parse :: String -> Either ParseError Program
parse = Parsec.parse program "CamleParser"
parseExpression = Parsec.parse expression "CamleParser expression"
parseBooleanExpression = Parsec.parse booleanExpression "CamleParser boolean expression"

-- First implement assignment & write & writeln
program :: Parser Program
program = do
        whitespace
        program <- statements
        eof
        return $ Program program

statements :: Parser [Statement]
statements = do
    first <- statement
    rest <- many (reserved ";" >> statement)
    return $ first:rest

statement :: Parser Statement
statement = try skip 
          <|> try write
          <|> try writeln
          <|> try conditional
          <|> try whileloop
          <|> try readvar
          <|> try variableAssignment 
          <|> ((parens $ sepBy1 statement (reservedOp ";")) >>= return . Statements)

whileloop = do
        reserved "while"
        bexp <- booleanExpression
        reserved "do"
        stmt <- statement
        return $ While bexp stmt

conditional = do
            reserved "if"
            bexp <- booleanExpression
            reserved "then"
            ifTrue <- statement
            reserved "else"
            ifFalse <- statement
            return $ If bexp ifTrue ifFalse


readvar = reserved "read" >>  parens identifier >>= return . Read . VarName

skip = reserved "skip" >> return Skip

writeln = reserved "writeln" >> (return $ WriteString "\n")

write :: Parser Statement
write = do 
           reserved "write"
           try writeString <|> try writeExpression <|> writeBoolean

writeBoolean = parens booleanExpression >>= return . WriteBoolean

writeString = do
           val <- parens string 
           return $ WriteString val

writeExpression = parens expression >>= \exp -> return $ WriteExpression exp

booleanExpression = do 
                       terms <- sepBy1 booleanTerm (reservedOp "&") 
                       return $ case terms of
                           [term] -> BooleanExpression term
                           _ -> BAnd terms

booleanTerm = try negatedBoolean <|> (boolean >>= return . BTerm)

negatedBoolean = reservedOp "!" >> boolean >>= return . BNegate

boolean =   try true
            <|> try false
            <|> try booleanComparison
            <|> parentheticalBooleanExpression
           where parentheticalBooleanExpression = parens booleanExpression >>= return . BExp

booleanComparison = do
        e1 <- expression
        op <- relationalOperator
        e2 <- expression
        return $ BBinOp op e1 e2

relationalOperator = try relationalEquals <|> relationalLessThanEquals

relationalEquals = do
        reservedOp "="
        return Equal

relationalLessThanEquals = do
        reservedOp "<="
        return LessThanEqual

true = do
        reserved "true" 
        return BTrue

false = do
        reserved "false"
        return BFalse

string = between (reservedOp "'") (reservedOp "'") $ many1 stringChar

stringChar = try escapedQuote
           <|> noneOf "'"
    where escapedQuote = stringEscapeChar >> char '\''

stringEscapeChar = char '\''


variableAssignment = do
        name <- identifier 
        reserved ":=" 
        val <- expression
        return $ Assign (VarName name) val

prescedenceTable = [ [prefix "-" Negate]
                   , [binary "*" (BinOp Times) Expr.AssocLeft]
                   , [binary "+" (BinOp Plus) Expr.AssocLeft, binary "-" (BinOp Minus) Expr.AssocLeft]
                   ]

expression :: Parser Expression
expression = Expr.buildExpressionParser prescedenceTable eterm

eterm = try (term >>= return . ETerm)
      <|> parens expression

term = try (integer >>= \val -> return $ Constant val)
     <|> (identifier >>= \ident -> return $ Var ident)

factor = try constant


{- Parsec Expression helper functions
- defined at: https://hackage.haskell.org/package/parsec-3.1.0/docs/Text-Parsec-Expr.html
-}
binary  name fun assoc = Expr.Infix (do{ reservedOp name; return fun }) assoc
prefix  name fun       = Expr.Prefix (do{ reservedOp name; return fun })
postfix name fun       = Expr.Postfix (do{ reservedOp name; return fun })

parseWhile :: String -> IO (Either ParseError Program)
parseWhile file = readFile file >>= return . parse
