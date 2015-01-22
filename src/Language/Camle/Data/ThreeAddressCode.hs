module Language.Camle.Data.ThreeAddressCode where


type Program = [TAC]

data TAC = TAC { instruction :: Instruction
               , result :: Maybe Argument
               , arg1 :: Maybe Argument
               , arg2 :: Maybe Argument
               } deriving (Show, Eq)

data Instruction = InstrBinary BinaryOp
                 | InstrUnary UnaryOp
                 | InstrNoop
                 | InstrIf
                 | InstrIfNot
                 | InstrAssign
                 | InstrRead
                 | InstrWriteString
                 | InstrWriteInt
                 deriving (Show, Eq)

data Argument = ArgLabel Label 
              | ArgConstant Integer
              | ArgBool Bool
              | ArgVar VarName
              | ArgTempVar Integer
              deriving (Show, Eq)

type VarName = String

newtype Label = Label Integer deriving (Show, Eq)

data BinaryOp = 
              -- Arithmetic
              BOpAdd | BOpSub | BOpMul
              -- Comparison
              | BOpLTE | BOpEq 
              -- Logical
              | BOpAnd
              deriving (Show, Eq)

data UnaryOp = UOpMinus | UOpNegate
             deriving (Show, Eq)
