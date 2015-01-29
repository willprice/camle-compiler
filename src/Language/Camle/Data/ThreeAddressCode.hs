{-# LANGUAGE DeriveDataTypeable #-}
module Language.Camle.Data.ThreeAddressCode where

import Data.Generics


type Program = [TAC]

data TAC = AssignI Temp Integer
         | AssignV Var Temp
         | AssignT Temp Temp
         | BinOp Op Temp Temp Temp
         | UnaryOp UnaryOp Temp Temp
         | Goto Label
         | WriteI Temp
         | WriteB Temp
         | WriteS String
         | Read Temp
         | IfNot Temp Label
         | L Label
         | Noop
         deriving (Data, Typeable, Show, Eq)

newtype Label = Label Integer
                deriving (Data, Typeable, Eq, Ord)

newtype Var = Var String
                deriving (Data, Typeable, Show, Eq, Ord)

newtype Temp = Temp Integer
               deriving (Data, Typeable, Show, Eq, Ord)


data Op = -- Arithmetic
        BOpAdd | BOpSub | BOpMul
        -- Comparison
        | BOpLTE | BOpEq 
        -- Logical
        | BOpAnd
        deriving (Data, Typeable, Eq)

data UnaryOp = UOpMinus | UOpNegate
             deriving (Data, Typeable, Eq)

instance Show Label where
        show (Label n) = "L" ++ show n

instance Show Op where
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
