{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Camle.IntermediateRepresentation where

import Control.Monad.State
import qualified Data.Map.Strict as M

import qualified Language.Camle.AbstractSyntaxTree as AST
import qualified Language.Camle.ThreeAddressCode as TAC

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



type SymbolTable = M.Map AST.VarName TAC.Variable

data TACTranslatorState = TACTranslatorState 
                        { labelNumber :: Integer
                        , tempVarNumber :: Integer
                        , symbolTable :: SymbolTable
                        , code :: [TAC.Instruction]
                        }
                        deriving (Show, Eq)

initialTranslatorState = TACTranslatorState
                        { labelNumber = 0
                        , symbolTable = M.empty
                        , tempVarNumber = 0
                        , code = []
                        }



mkTemp :: Gen TAC.Variable
mkTemp = do
        temp <- gets tempVarNumber
        modify $ \state -> state { tempVarNumber = temp + 1 } 
        return $ TAC.TemporaryVar temp


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
