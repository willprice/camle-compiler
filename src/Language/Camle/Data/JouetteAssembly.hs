module Language.Camle.Data.JouetteAssembly where

import Data.List

data Argument = ArgReg Register
              | ArgConst Integer
              | ArgAddr Address
              | ArgLabel Label
              deriving (Eq)

newtype Address = Address Integer
                deriving (Eq, Ord)

data Register = Register Integer
              deriving (Eq, Ord)

newtype Label = Label Integer
              deriving (Eq)

type Memory = [Byte]


newtype Byte = Byte Integer
             deriving (Eq, Ord)

instance Show Byte where
        show (Byte n) = show n

type AddressOffset = Integer

true :: Integer
true = 1

false :: Integer
false = 0

-- | A word consists of 4 bytes
wordLength ::  Int
wordLength = 4

data Instruction = ADD Register Register Register
                 | SUB Register Register Register
                 | MUL Register Register Register
                 | DIV Register Register Register
                 | XOR Register Register Register
                 | ADDI Register Register Integer
                 | SUBI Register Register Integer
                 | MULI Register Register Integer
                 | DIVI Register Register Integer
                 | XORI Register Register Integer
                 | RD Register
                 | WR Register
                 | WRS Integer
                 | BGEZ Register Label
                 | BLTZ Register Label
                 | BEQZ Register Label
                 | BNEZ Register Label
                 | JUMP Label
                 | LOAD Register Register AddressOffset 
                 | STORE Register Register AddressOffset
                 | LBL Label
                 | NOP
                 | HALT
                 | DATA Byte
                 | Comment String
                 deriving (Eq)

instance Show Instruction where
        show instr = case instr of 
            (ADD r1 r2 r3) -> "ADD " ++ showArgs [r1, r2, r3]
            (SUB r1 r2 r3) -> "SUB " ++ showArgs [r1, r2, r3]
            (MUL r1 r2 r3) -> "MUL " ++ showArgs [r1, r2, r3]
            (DIV r1 r2 r3) -> "DIV " ++ showArgs [r1, r2, r3]
            (XOR r1 r2 r3) -> "XOR " ++ showArgs [r1, r2, r3]
            (ADDI r1 r2 n) -> "ADDI " ++ showArgs [r1, r2] ++ "," ++ show n
            (SUBI r1 r2 n) -> "SUBI " ++ showArgs [r1, r2] ++ "," ++ show n
            (MULI r1 r2 n) -> "MULI " ++ showArgs [r1, r2] ++ "," ++ show n
            (DIVI r1 r2 n) -> "DIVI " ++ showArgs [r1, r2] ++ "," ++ show n
            (XORI r1 r2 n) -> "XORI " ++ showArgs [r1, r2] ++ "," ++ show n
            (RD r) -> "RD " ++ show r
            (WR r) -> "WR " ++ show r
            (WRS addr) -> "WRS " ++ show addr
            (BGEZ reg lbl) -> "BGEZ " ++ show reg ++ "," ++ show lbl
            (BLTZ reg lbl) -> "BLTZ " ++ show reg ++ "," ++ show lbl
            (BEQZ reg lbl) -> "BEQZ " ++ show reg ++ "," ++ show lbl
            (BNEZ reg lbl) -> "BNEZ " ++ show reg ++ "," ++ show lbl
            (LOAD r1 r2 offset) -> "LOAD " ++ showArgs [r1, r2] ++ "," 
                                   ++ show offset
            (STORE r1 r2 offset) -> "STORE " ++ showArgs [r1 ,r2] ++ ","
                                    ++ show offset
            (JUMP lbl) -> "JMP " ++ show lbl
            (LBL lbl) -> show lbl ++ ":"
            NOP -> "NOP"
            HALT -> "HALT"
            (DATA byte) -> "DATA " ++ show byte
            (Comment str) -> "; " ++ str
          where showArgs = concat . intersperse "," . map show

zeroReg = Register 0

instance Show Register where
        show (Register number) = "R" ++ show number

instance Show Address where
        show (Address address) = show address

instance Show Label where
    show (Label name) = "L" ++ show name
