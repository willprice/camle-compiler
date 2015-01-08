{-# LANGUAGE MultiParamTypeClasses #-}
module CamleCompiler.IR where

import Control.Monad.State
import qualified Data.Map.Strict as M

import qualified CamleCompiler.AST as AST
import qualified CamleCompiler.ThreeAddressCode as TAC

{- Abstract Syntax Tree to Three Address Code translation
-
- After parsing the code into an Abstract Syntax Tree (AST), we then
- convert the AST into an intermediate form known as three address code.
- This intermediate form is closer to register machine level and
- facilitates easier:
- * Optimisation
- * Register allocation
-}

newtype Label = Label Integer

type Gen = State TACTranslatorState

class Translateable src target where
        translate :: src -> Gen target



data TACTranslatorState = TACTranslatorState 
                        { labelNumber :: Integer
                        , tempVarNumber :: Integer
                        , code :: [TAC.Instruction]
                        }
                        deriving (Show, Eq)

initialTranslatorState = TACTranslatorState
                        { labelNumber = 0
                        , tempVarNumber = 0
                        , code = []
                        }



mkTemp :: Gen TAC.ID
mkTemp = do
        temp <- gets tempVarNumber
        modify $ \state -> state { tempVarNumber = temp + 1 } 
        return $ TAC.TemporaryVar temp


putInstruction instruction = modify $ \state -> state { code = instruction:(code state) }

putLabel label = putInstruction $ TAC.ILabel label

assignTemp temp exp = putInstruction $ TAC.Assign (TAC.TID temp) exp

assignVariable name exp = putInstruction $ TAC.Assign (TAC.TID (TAC.Var name)) exp


translateBinOp :: AST.BinaryArithmeticOperation -> Gen TAC.Op
translateBinOp op = return $ case op of AST.Plus -> TAC.Plus
                                        AST.Minus -> TAC.Minus
                                        AST.Times -> TAC.Times

translateTerm :: AST.Term -> Gen TAC.Term
translateTerm _ = return $ TAC.TValue $ TAC.Constant 1

getNextLabel = do
        l <- gets labelNumber 
        modify $ \state -> state { labelNumber = l + 1 }
        return $ TAC.Label ("L" ++ show l)

instance Translateable AST.Statement () where
        translate AST.Skip = putInstruction TAC.Nop
        translate (AST.Assign (AST.VarName name) exp) = do
            result <- translate exp
            assignVariable name (TAC.ETerm result)
        translate (AST.If boolexp ifTrue ifFalse) = do
            condition <- translate boolexp
            ifTrueLabel <- getNextLabel
            putInstruction $ TAC.If condition ifTrueLabel
            translate ifFalse
            ifFalseLabel <- getNextLabel
            putInstruction $ TAC.Goto ifFalseLabel
            putLabel ifTrueLabel
            translate ifTrue
            putLabel ifFalseLabel

instance Translateable AST.BooleanExpression where
        translate (AST.BooleanExpression boolexp) = translate boolexp
        translate (AST.BAnd term:[]) = translate term
        translate (AST.BAnd term:terms) = do
            
            
instance Translateable AST.Term TAC.Exp where
        translate (AST.Constant n) = return . TAC.ETerm . TAC.TValue . TAC.Constant $ n
        translate (AST.Var var) = return . TAC.ETerm . TAC.TID $ TAC.Var var

instance Translateable AST.Expression TAC.Term where
        translate (AST.BinOp astOp astExp1 astExp2) = do
            t1 <- translate astExp1
            t2 <- translate astExp2
            op <- translateBinOp astOp
            temp <- mkTemp
            assignTemp temp $ TAC.BinOp op t1 t2
            return $ TAC.TID temp

        translate (AST.Negate astExp) = do
            exp <- translate astExp
            temp <- mkTemp
            assignTemp temp $ TAC.ETerm exp
            return $ TAC.TID temp

        translate (AST.ETerm term) = translate term

instance Translateable AST.Term TAC.Term where
        translate (AST.Constant n) = return . TAC.TValue $ TAC.Constant n
        translate (AST.Var string) = return . TAC.TValue $ TAC.StringLiteral string
