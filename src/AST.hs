module AST where

type Name = String

data Statement = Skip
               | If BooleanExpression Statement Statement
               | While BooleanExpression Statement
               | Read Variable
               | WriteStatement
data Expression = BinOp Operation Term Term
                deriving (Eq, Ord, Show)

data Operation = Plus
               | Minus
               | Times
               deriving (Eq, Ord, Show) 

data WriteStatement = WriteString String
                    | WriteBoolean BooleanExpression
                    | WriteExpression Expression
                    | Writeln

data BooleanTerm = Negate BooleanConstant
                 | BooleanConstant

data BooleanExpression = BooleanExpression [BooleanTerm]

data BooleanConstant = BTrue
                     | BFalse
                     | Equal Expression Expression
                     | LessThanEqual Expression Expression
                     | BooleanExpression

data Term = Term [Factor]

data Factor = Variable
            | Constant
            | Expression
