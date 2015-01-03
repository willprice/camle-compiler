module CamleCompiler.AST where

data Program = Program [Statement]
             deriving (Show, Eq)

data Statement = Skip
               | Assign VarName Expression
               | If BooleanExpression Statement Statement
               | While BooleanExpression Statement
               | Read VarName
               | Write WriteStatement
               | Statements [Statement]
               deriving (Show, Eq)

data WriteStatement = WriteString String
                    | WriteBoolean BooleanExpression
                    | WriteExpression Expression
                    | Writeln
                    deriving (Show, Eq)

data Expression = BinOp BinaryArithmeticOperation Expression Expression
                | Negate Expression
                | Constant Integer
                | Var VarName
                deriving (Show, Eq)

newtype VarName = VarName String
                deriving (Show, Eq)


data BinaryArithmeticOperation = Times
                         | Minus
                         | Plus
                         deriving (Eq, Ord, Show) 

data BooleanExpression = BooleanExpression BooleanTerm
                       | BAnd [BooleanTerm]
                       deriving (Show, Eq)

data BooleanTerm = BNegate Boolean
                 | BTerm Boolean
                 deriving (Show, Eq)

data Boolean = BTrue
             | BFalse
             | BBinOp RelationalBinaryOp Expression Expression
             | BExp BooleanExpression
             deriving (Show, Eq)

data RelationalBinaryOp = Equal
                        | LessThanEqual
                        deriving (Show, Eq)

