{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module Language.Camle.AstToIr where

import Control.Monad.State

import qualified Language.Camle.Data.AbstractSyntaxTree as AST
import qualified Language.Camle.Data.ThreeAddressCode as TAC

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

class Translateable src target where
        translate :: src -> IRGen target

data TACTranslatorState = TACTranslatorState 
                        { nextLabel :: Integer
                        , nextTempVar :: Integer
                        , code :: TAC.Program
                        }
                        deriving (Show, Eq)

initialTranslatorState = TACTranslatorState
                        { nextLabel = 1
                        , nextTempVar = 1
                        , code = []
                        }

instance Translateable AST.Program TAC.Program where
        translate (AST.Program statements) = do
            translate (AST.Statements statements) :: IRGen ()
            gets code


instance Translateable AST.Statement () where
        translate (AST.Skip) = putInstruction $ TAC.TAC { TAC.instruction = TAC.InstrNoop
                                                        , TAC.result = Nothing
                                                        , TAC.arg1 = Nothing
                                                        , TAC.arg2 = Nothing
                                                        }

        translate (AST.Assign (AST.VarName name) exp) = do
            e <- translate exp
            putInstruction $ TAC.TAC { TAC.instruction = TAC.InstrAssign
                                     , TAC.result = Just $ TAC.ArgVar name
                                     , TAC.arg1 = Just e
                                     , TAC.arg2 = Nothing
                                     }

        translate (AST.Statements (statement:[])) = translate statement
        translate (AST.Statements (statement:rest)) = do
            translate statement :: IRGen ()
            translate $ AST.Statements rest


        translate (AST.WriteString str) = do
            putInstruction TAC.TAC { TAC.instruction = TAC.InstrWriteString
                                   , TAC.result = Nothing
                                   , TAC.arg1 = Just $ TAC.ArgString str
                                   , TAC.arg2 = Nothing
                                   }

        translate (AST.WriteBoolean boolexp) = do
            t1 <- translate boolexp
            putInstruction TAC.TAC { TAC.instruction = TAC.InstrWriteInt
                                   , TAC.result = Nothing
                                   , TAC.arg1 = Just t1
                                   , TAC.arg2 = Nothing
                                   }

        translate (AST.WriteExpression exp) = do
            t <- translate exp
            putInstruction TAC.TAC { TAC.instruction = TAC.InstrWriteInt
                                   , TAC.result = Nothing
                                   , TAC.arg1 = Just t
                                   , TAC.arg2 = Nothing
                                   }


        translate instruction = error $ "Conversion of " ++ show instruction ++ " is unimplemented"


instance Translateable AST.Expression TAC.Argument where
        translate (AST.BinOp op e1 e2) = do
            result <- mkTemp
            t1 <- translate e1
            t2 <- translate e2
            op' <- translate op
            putInstruction $ TAC.TAC { TAC.instruction = TAC.InstrBinary op'
                                     , TAC.result = Just result
                                     , TAC.arg1 = Just t1
                                     , TAC.arg2 = Just t2
                                     }
            return result

        translate (AST.Negate e) = do
            result <- mkTemp
            t <- translate e
            putInstruction $ TAC.TAC { TAC.instruction = TAC.InstrUnary TAC.UOpNegate
                                     , TAC.result = Just result
                                     , TAC.arg1 = Just t
                                     , TAC.arg2 = Nothing
                                     }
            return result

        translate (AST.ETerm term) = translate term


instance Translateable AST.Term TAC.Argument where
        translate (AST.Constant n) = return $ TAC.ArgConstant n
        translate (AST.Var name) = return $ TAC.ArgVar name


instance Translateable AST.BinaryArithmeticOperation TAC.BinaryOp where
        translate op = return $ case op of
                                    AST.Times -> TAC.BOpMul
                                    AST.Minus -> TAC.BOpSub
                                    AST.Plus -> TAC.BOpAdd

instance Translateable AST.BooleanExpression TAC.Argument where
        translate (AST.BooleanExpression term) = translate term
        translate (AST.BAnd t1 t2) = do
            t1' <- translate t1
            t2' <- translate t2
            result <- mkTemp
            putInstruction TAC.TAC { TAC.instruction = TAC.InstrBinary TAC.BOpAnd
                                   , TAC.result = Just result
                                   , TAC.arg1 = Just t1'
                                   , TAC.arg2 = Just t2'
                                   }
            return result


instance Translateable AST.BooleanTerm TAC.Argument where
        translate (AST.BNegate bool) = do
            t <- translate bool
            result <- mkTemp
            putInstruction TAC.TAC { TAC.instruction = TAC.InstrUnary TAC.UOpNegate
                                   , TAC.result = Just result
                                   , TAC.arg1 = Just t
                                   , TAC.arg2 = Nothing
                                   }
            return result

        translate (AST.BTerm bool) = translate bool

instance Translateable AST.Boolean TAC.Argument where
        translate AST.BTrue = return $ TAC.ArgBool True
        translate AST.BFalse = return $ TAC.ArgBool False
        translate (AST.BBinOp op e1 e2) = do
            t1 <- translate e1
            t2 <- translate e2
            op' <- translate op
            result <- mkTemp
            putInstruction TAC.TAC { TAC.instruction = TAC.InstrBinary op'
                                   , TAC.result = Just result
                                   , TAC.arg1 = Just t1
                                   , TAC.arg2 = Just t2
                                   }
            return result
        translate (AST.BExp boolexp) = translate boolexp

instance Translateable AST.RelationalBinaryOp TAC.BinaryOp where
        translate AST.LessThanEqual = return TAC.BOpLTE
        translate AST.Equal = return TAC.BOpEq

mkTemp :: IRGen TAC.Argument
mkTemp = do
        temp <- gets nextTempVar
        modify $ \state -> state { nextTempVar = temp + 1 } 
        return $ TAC.ArgTempVar temp

putInstruction instruction = modify $ \state -> state { code = instruction:(code state) }

{-

putInstruction instruction = modify $ \state -> state { code = instruction:(code state) }

putLabel label = putInstruction $ TAC.ILabel label

assignTemp temp exp = putInstruction $ TAC.Assign (TAC.TVar temp) exp

assignVariable var exp = putInstruction $ TAC.Assign (TAC.TVar var) exp


translateBinOp :: AST.BinaryArithmeticOperation -> TAC.Op
translateBinOp op = case op of AST.Plus -> TAC.Plus
                               AST.Minus -> TAC.Minus
                               AST.Times -> TAC.Times

translateRelOp op = case op of AST.Equal -> TAC.Equal
                               AST.LessThanEqual -> TAC.LessThanEqual


getNextLabel :: Gen TAC.Label
getNextLabel = do
        l <- gets labelNumber 
        modify $ \state -> state { labelNumber = l + 1 }
        return $ TAC.Label ("L" ++ show l)

addVariableToSymbolTable :: AST.VarName -> Gen TAC.Variable
addVariableToSymbolTable (AST.VarName name) = do
        modify $ \s -> s { symbolTable = M.insert (AST.VarName name) assignedVar (symbolTable s) }
        return assignedVar
      where assignedVar = TAC.Var name

{-
- RIGHT! TAKE NOTE!
- Since practically all of these things can produce additional code, it
- makes sense for them to return temporary variables instead of terms,
- expressions etc etc, as it's just too much complexity to deal with. If
- you return a temporary variable from each translate, then you can
- optimise out all the unneccesary ones.
-
- TAC.Variable
-}

-- TODO
instance Translateable AST.Statement TAC.Variable where
        translate AST.Skip = undefined
        translate (AST.Assign var exp) = do
            e <- translate exp
            var' <- translate var
            assignVariable var' $ TAC.variableToExpression e
            return var'
        translate (AST.If boolexp ifTrue ifFalse) = do
            cond <- translate boolexp
            translate ifTrue
            translate ifFalse

-- FINISHED
instance Translateable AST.Expression TAC.Variable where
        translate (AST.BinOp aop e1 e2) = do
            t <- mkTemp
            t1 <- translate e1
            t2 <- translate e2
            assignTemp t $ TAC.binOpOfVariables (translateBinOp aop) t1 t2
            return t
        translate (AST.Negate e) = do
            t <- mkTemp
            e' <- translate e
            assignTemp t $ TAC.UnaryOp TAC.Minus $ TAC.variableToTerm e'
            return t
        translate (AST.ETerm term) = translate term

-- FINISHED
instance Translateable AST.Term TAC.Variable where
        translate (AST.Constant int) = do
            t <- mkTemp
            assignTemp t $ TAC.valueToExpression $ TAC.Constant int
            return t
        translate (AST.Var varname) = translate $ AST.VarName varname

-- FINISHED
instance Translateable AST.VarName TAC.Variable where
        translate var = do
            table <- gets symbolTable
            case (M.lookup var table) of
                Nothing -> addVariableToSymbolTable var
                Just v -> return v
            


-- FINISHED
instance Translateable AST.BooleanExpression TAC.Variable where
        translate (AST.BooleanExpression term) = translate term
        translate (AST.BAnd t1 t2) = do
            t1' <- translate t1
            t2' <- translate t2
            t <- mkTemp
            assignTemp t $ TAC.binOpOfVariables TAC.And t1' t2'
            return t

-- FINISHED
instance Translateable AST.BooleanTerm TAC.Variable where
        translate (AST.BTerm bool) = translate bool
        translate (AST.BNegate bool) = do
            term' <- translate bool
            t <- mkTemp
            assignTemp t $ TAC.UnaryOp TAC.Negate (TAC.variableToTerm term')
            return t

-- FINISHED
instance Translateable AST.Boolean TAC.Variable where
        translate AST.BTrue = do 
                                 t <- mkTemp
                                 assignTemp t $ TAC.valueToExpression (TAC.Boolean True)
                                 return t
        translate AST.BFalse = do 
                                 t <- mkTemp
                                 assignTemp t $ TAC.valueToExpression (TAC.Boolean True)
                                 return t

        translate (AST.BBinOp op e1 e2) = let op' = translateRelOp op in 
                                            do
                                                e1result <- translate e1
                                                e2result <- translate e2
                                                cond <- mkTemp
                                                assignTemp cond $ TAC.binOpOfVariables op' e1result e2result
                                                return cond

        translate (AST.BExp boolexp) = translate boolexp
-}
