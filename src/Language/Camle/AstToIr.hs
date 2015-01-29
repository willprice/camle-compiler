{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module Language.Camle.AstToIr (printIr, translate, astToIr) where

import Control.Monad.State
import qualified Data.Map as M

import qualified Language.Camle.Data.AbstractSyntaxTree as AST
import qualified Language.Camle.Data.ThreeAddressCode as TAC
import Language.Camle.Print.ThreeAddressCode (printIr)

{- Abstract Syntax Tree to Three Address Code translation
-
- After parsing the code into an Abstract Syntax Tree (AST), we then
- convert the AST into an intermediate form known as three address code.
- This intermediate form is closer to machine level and
- facilitates easier:
- * Optimisation
- * Register allocation
-}

type IRGen = State TACTranslatorState
type SymbolTable = M.Map AST.VarName TAC.Temp

class Translateable src target where
        translate :: src -> IRGen target

data TACTranslatorState = TACTranslatorState 
                        { nextLabel :: Integer
                        , nextTempVar :: Integer
                        , symTable :: SymbolTable
                        , code :: TAC.Program
                        }
                        deriving (Show, Eq)

initialState = TACTranslatorState
               { nextLabel = 1
               , nextTempVar = 1
               , symTable = M.empty
               , code = []
               }

instance Translateable AST.Program TAC.Program where
        translate (AST.Program statements) = do
            translate (AST.Statements statements) :: IRGen ()
            gets code

instance Translateable AST.Statement () where
        translate (AST.Skip) = putInstruction $ TAC.Noop

        translate (AST.Assign var@(AST.VarName name) e) = do
            -- TODO: handle case where var has already been assigned to
            t <- translate e :: IRGen TAC.Temp
            possibleTemp <- getVariableFromSymbolTable var
            case  possibleTemp of
                Nothing -> do
                    insertVariableToSymbolTable var t
                    putInstruction $ TAC.AssignV (TAC.Var name) t
                (Just initialAssignment) -> do
                    putInstruction $ TAC.AssignT initialAssignment t
                    putInstruction $ TAC.AssignV (TAC.Var name) initialAssignment

        translate (AST.Statements (statement:[])) = translate statement
        translate (AST.Statements (statement:rest)) = do
            translate statement :: IRGen ()
            translate $ AST.Statements rest


        translate (AST.WriteString str) = do
            putInstruction $ TAC.WriteS str

        translate (AST.WriteBoolean boolexp) = do
            t <- translate boolexp
            putInstruction $ TAC.WriteB t

        translate (AST.WriteExpression e) = do
            t <- translate e
            putInstruction $ TAC.WriteI t

        translate (AST.Read var@(AST.VarName name)) = do
            t <- mkTemp
            insertVariableToSymbolTable var t
            putInstruction $ TAC.Read t
            putInstruction $ TAC.AssignV (TAC.Var name) t

        translate (AST.If boolexp ifTrue ifFalse) = do
            elseLabel <- mkLabel
            endLabel <- mkLabel

            t <- translate boolexp
            putInstruction $ TAC.IfNot t elseLabel
            translate ifTrue :: IRGen ()
            putInstruction $ TAC.Goto $ endLabel
            putInstruction $ TAC.L elseLabel
            translate ifFalse :: IRGen ()
            putInstruction $ TAC.L endLabel

        translate (AST.While boolexp stmt) = do
            conditionLabel <- mkLabel
            endLabel <- mkLabel

            putLabel conditionLabel
            bexp <- translate boolexp
            putInstruction $ TAC.IfNot bexp endLabel
            translate stmt :: IRGen ()
            putInstruction $ TAC.Goto conditionLabel
            putLabel endLabel

        translate instruction = error $ "Conversion of " ++ show instruction ++ " is unimplemented"


instance Translateable AST.Expression TAC.Temp where
        translate (AST.BinOp op e1 e2) = do
            t1 <- translate e1
            t2 <- translate e2
            op' <- translate op
            result <- mkTemp
            putInstruction $ TAC.BinOp op' result t1 t2
            return result

        translate (AST.Negate e) = do
            t <- translate e
            result <- mkTemp
            putInstruction $ TAC.UnaryOp TAC.UOpMinus result t
            return result

        translate (AST.ETerm term) = translate term


instance Translateable AST.Term TAC.Temp where
        translate (AST.Constant n) = do
            t <- mkTemp
            putInstruction $ TAC.AssignI t n
            return t
        translate (AST.Var name) = do
            table <- gets symTable
            case (M.lookup var table) of
                Nothing -> fail "Expected variable to be assigned before being referenced"
                Just t -> return t
            where var = (AST.VarName name)


instance Translateable AST.BinaryArithmeticOperation TAC.Op where
        translate op = return $ case op of
                                    AST.Times -> TAC.BOpMul
                                    AST.Minus -> TAC.BOpSub
                                    AST.Plus -> TAC.BOpAdd

instance Translateable AST.BooleanExpression TAC.Temp where
        translate (AST.BooleanExpression term) = translate term
        translate (AST.BAnd t1 t2) = do
            t1' <- translate t1
            t2' <- translate t2
            result <- mkTemp
            putInstruction $ TAC.BinOp TAC.BOpAnd result t1' t2'
            return result


instance Translateable AST.BooleanTerm TAC.Temp where
        translate (AST.BNegate bool) = do
            t <- translate bool
            result <- mkTemp
            putInstruction $ TAC.UnaryOp TAC.UOpNegate result t
            return result

        translate (AST.BTerm bool) = translate bool

instance Translateable AST.Boolean TAC.Temp where
        translate AST.BTrue = do
            t <- mkTemp
            putInstruction $ TAC.AssignI t 1
            return t
        translate AST.BFalse = do
            t <- mkTemp
            putInstruction $ TAC.AssignI t 0
            return t
        translate (AST.BBinOp op e1 e2) = do
            t1 <- translate e1
            t2 <- translate e2
            op' <- translate op
            result <- mkTemp
            putInstruction $ TAC.BinOp op' result t1 t2
            return result
        translate (AST.BExp boolexp) = translate boolexp

instance Translateable AST.RelationalBinaryOp TAC.Op where
        translate AST.LessThanEqual = return TAC.BOpLTE
        translate AST.Equal = return TAC.BOpEq

mkTemp :: IRGen TAC.Temp
mkTemp = do
        temp <- gets nextTempVar
        modify $ \s -> s { nextTempVar = temp + 1 } 
        return $ TAC.Temp temp

mkLabel :: IRGen TAC.Label
mkLabel = do 
             lbl <- gets nextLabel
             modify $ \s -> s { nextLabel = lbl + 1 } 
             return $ TAC.Label lbl

putInstruction instruction = modify $ \s -> s { code = instruction:(code s) }

putLabel :: TAC.Label -> IRGen ()
putLabel = putInstruction . TAC.L

getVariableFromSymbolTable :: AST.VarName -> IRGen (Maybe TAC.Temp)
getVariableFromSymbolTable var = do
        table <- gets symTable
        return $ M.lookup var table
insertVariableToSymbolTable :: AST.VarName -> TAC.Temp -> IRGen ()
insertVariableToSymbolTable var temp = do
        table <- gets symTable
        case (M.fold (\t prev -> t == temp || prev) False table) of
            True -> do
                newTemp <- mkTemp
                putInstruction $ TAC.AssignT newTemp temp
                modify (\s -> s { symTable = M.insert var newTemp table }) 
            False -> modify (\s -> s { symTable = M.insert var temp table })

astToIr :: AST.Program -> TAC.Program
astToIr ast = reverse . fst $ runState (translate ast) initialState
