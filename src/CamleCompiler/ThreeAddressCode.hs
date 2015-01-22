{-# LANGUAGE DeriveDataTypeable #-}
module CamleCompiler.ThreeAddressCode where
import Data.Data
import Data.Generics.Uniplate.Data()

type InstructionSeq = [Instruction]

newtype Label = Label String
                deriving (Eq, Show, Data, Typeable)

-- TAC = Three Address Code
data Instruction = Assign Term Exp
                 | Goto Label
                 | If Term Label
                 | Write Value
                 | Nop
                 | ILabel Label
                 deriving (Eq, Show, Data, Typeable)

data Exp = BinOp Op Term Term
         | UnaryOp Op Term
         | ETerm Term
         deriving (Eq, Show, Data, Typeable)

data Op = Plus
        | Minus
        | Times
        | And
        | Negate
        | Equal
        | LessThanEqual
        deriving (Eq, Show, Data, Typeable)

data Variable = Var String
              | TemporaryVar Integer
              deriving (Eq, Show, Data, Typeable)

data Term = TVar Variable
          | TValue Value
          deriving (Eq, Show, Data, Typeable)

data Value = Constant Integer
           | Boolean Bool
           | StringLiteral String
           deriving (Eq, Show, Data, Typeable)


valueToExpression val = ETerm $ TValue val
variableToExpression var = ETerm $ variableToTerm var
variableToTerm var = TVar var
binOpOfVariables op var1 var2 = BinOp op (TVar var1) (TVar var2)
