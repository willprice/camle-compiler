{-# LANGUAGE DeriveDataTypeable #-}
module CamleCompiler.ThreeAddressCode where
import Data.Data
import Data.Generics.Uniplate.Data()

data Variable = Variable { name :: String } deriving (Eq, Ord, Show, Data, Typeable)

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


data RelOp = Equal
           | LessThanEqual
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
        deriving (Eq, Show, Data, Typeable)

data ID = Var String
        | TemporaryVar Integer
        deriving (Eq, Show, Data, Typeable)

data Term = TID ID
          | TValue Value
          deriving (Eq, Show, Data, Typeable)

data Value = Constant Integer
           | Boolean Bool
           | StringLiteral String
           deriving (Eq, Show, Data, Typeable)
