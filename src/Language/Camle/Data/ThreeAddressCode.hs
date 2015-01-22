module Language.Camle.Data.ThreeAddressCode where


type Program = [TAC]

data TAC = TAC { instruction :: Instruction
               , result :: Maybe Argument
               , arg1 :: Maybe Argument
               , arg2 :: Maybe Argument
               } deriving (Eq)

data Instruction = InstrBinary BinaryOp
                 | InstrUnary UnaryOp
                 | InstrIf
                 | InstrIfNot
                 | InstrAssign
                 | InstrRead
                 | InstrWriteString
                 | InstrWriteInt
                 | InstrNoop
                 | InstrLabel
                 deriving (Eq)

data Argument = ArgLabel Label 
              | ArgConstant Integer
              | ArgString String
              | ArgBool Bool
              | ArgVar VarName
              | ArgTempVar Integer
              deriving (Eq)

type VarName = String

newtype Label = Label Integer deriving (Show, Eq)

data BinaryOp = 
              -- Arithmetic
              BOpAdd | BOpSub | BOpMul
              -- Comparison
              | BOpLTE | BOpEq 
              -- Logical
              | BOpAnd
              deriving (Eq)

data UnaryOp = UOpMinus | UOpNegate
             deriving (Eq)


instance Show Argument where
        show (ArgLabel lbl) = "L" ++ show lbl
        show (ArgConstant n) = show n
        show (ArgString s) = "\"" ++ show s ++ "\""
        show (ArgBool bool) = show bool
        show (ArgVar name) = show name
        show (ArgTempVar n) = "T" ++ show n

instance Show BinaryOp where
        show op = case op of
                      BOpSub -> "-"
                      BOpAdd -> "+"
                      BOpMul -> "*"
                      BOpLTE -> "<="
                      BOpEq -> "="
                      BOpAnd -> "&"

instance Show UnaryOp where
        show op = case op of
                      UOpMinus -> "-"
                      UOpNegate -> "!"

instance Show TAC where
        show tac = case instruction tac of 
                                    InstrAssign -> show res ++ " := " ++ show a1
                                    (InstrBinary op) -> show res ++ " := " ++ show a1 ++ show op ++ show a2
                                    (InstrUnary op) -> show res ++ " := " ++ show op ++ show a1
                                    InstrIf -> "if " ++ show a1 ++ " goto " ++ show a2
                                    InstrIfNot -> "if not " ++ show a1 ++ " goto " ++ show a2
                                    InstrRead -> show res ++ " := " ++ "read"
                                    InstrWriteString -> "write " ++ show a1
                                    InstrWriteInt -> "write " ++ show a1
                                    InstrNoop -> "noop"
                                    InstrLabel -> "L" ++ show a1
                                    where a1 = arg1 tac
                                          a2 = arg2 tac
                                          res = result tac
