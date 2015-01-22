module Language.Camle.Data.Assembly where

newtype Address = Address Integer
                deriving (Show, Eq, Ord)

newtype AddressOffset = AddressOffset Integer
                      deriving (Show, Eq, Ord)

newtype Byte = Byte Integer
             deriving (Show, Eq, Ord)

data Register = Register Integer Integer
              deriving (Eq, Ord)


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
                 | WRS Address
                 | LOAD Register Register AddressOffset 
                 | STORE Register Register
                 | NOP
                 | HALT
                 | DATA Byte
                 deriving (Eq)

instance Show Register where
        show (Register number contents) = "R" ++ show number ++ ": " ++ show contents
