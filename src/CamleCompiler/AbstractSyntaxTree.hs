{-# LANGUAGE DeriveDataTypeable #-}
module CamleCompiler.AbstractSyntaxTree where
import Data.Data
import Data.Generics.Uniplate.Data()

data Program = Program [Statement]
             deriving (Show, Eq, Data, Typeable)

data Statement = Skip
               | Assign VarName Expression
               | If BooleanExpression Statement Statement
               | While BooleanExpression Statement
               | Read VarName
               | WriteString String
               | WriteBoolean BooleanExpression
               | WriteExpression Expression
               | Statements [Statement]
               deriving (Show, Eq, Data, Typeable)

data Expression = BinOp BinaryArithmeticOperation Expression Expression
                | Negate Expression
                | ETerm Term
                deriving (Show, Eq, Data, Typeable)

data Term = Constant Integer
          | Var String
          deriving (Show, Eq, Data, Typeable)

data VarName = VarName String deriving (Show, Eq, Ord, Data, Typeable)


data BinaryArithmeticOperation = Times
                         | Minus
                         | Plus
                         deriving (Eq, Ord, Show, Data, Typeable) 

data BooleanExpression = BooleanExpression BooleanTerm
                       | BAnd BooleanTerm BooleanTerm
                       deriving (Show, Eq, Data, Typeable)

data BooleanTerm = BNegate Boolean
                 | BTerm Boolean
                 deriving (Show, Eq, Data, Typeable)

data Boolean = BTrue
             | BFalse
             | BBinOp RelationalBinaryOp Expression Expression
             | BExp BooleanExpression
             deriving (Show, Eq, Data, Typeable)

data RelationalBinaryOp = Equal
                        | LessThanEqual
                        deriving (Show, Eq, Data, Typeable)
