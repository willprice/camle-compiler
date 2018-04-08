{-# OPTIONS_GHC -fno-warn-orphans #-}
module ParserTests where

import CamleCompiler.Parser
import CamleCompiler.AST
import Test.Tasty
import Test.Hspec
import Test.Tasty.Hspec
import qualified Text.ParserCombinators.Parsec

-- Need to define how ParseErrors are compared
import Text.ParserCombinators.Parsec.Error(ParseError, Message, errorMessages, messageEq)

instance Eq ParseError where
   a == b = errorMessages a == errorMessages b


parserSpec = describe "CamleCompiler.Parser parsing" $ do
  it "variable assignment" $ 
     shouldBe (parse "a := 123") $ wrapInProgramBoilerplate (Assign (VarName "a") (Constant 123))
  it "skip" $ 
     shouldBe (parse "skip") $ wrapInProgramBoilerplate Skip
  it "write string" $ 
     shouldBe (parse "write('hi')") $ wrapInProgramBoilerplate (Write $ WriteString "hi")
  it "write expression" $ 
     shouldBe (parse "write(2)") $ wrapInProgramBoilerplate (Write $ WriteExpression (Constant 2))
  it "write boolean expression" $ 
     shouldBe (parse "write(true)") $ wrapInProgramBoilerplate (Write . WriteBoolean . BooleanExpression $ BTerm BTrue)
  it "write variable" $ 
     shouldBe (parse "write(a)") $ wrapInProgramBoilerplate (Write . WriteExpression . Var $ VarName "a")
  it "read variable" $ 
     shouldBe (parse "read(a)") $ wrapInProgramBoilerplate (Read $ VarName "a")
  it "if statement" $ 
     shouldBe (parse "if true then writeln else writeln") $ wrapInProgramBoilerplate $ If (BooleanExpression . BTerm $ BTrue) (Write Writeln) (Write Writeln)
  it "multiple statements as a statement" $ 
     shouldBe (parse "(writeln; writeln)") $ wrapInProgramBoilerplate $ Statements [Write $ Writeln, Write $ Writeln]
  it "while loop" $ 
     shouldBe (parse "while true do writeln") $ wrapInProgramBoilerplate $ While (BooleanExpression . BTerm $ BTrue) (Write Writeln)
  expressionTests
  booleanExpressionTests

expressionTests = describe "CamleCompiler.Parser expression parsing" $ do
  it "constant" $
    shouldBe (parseExpression "2") $ Right (Constant 2) 
  it "variable" $
     shouldBe (parseExpression "i") $ Right (Var (VarName "i"))
  it "negated variable" $
     shouldBe (parseExpression "-i") $ Right (Negate (Var (VarName "i")))
  it "addition expression" $
     shouldBe (parseExpression "1 + 2") $ Right (BinOp Plus (Constant 1) (Constant 2))
  it "subtraction expression" $
     shouldBe (parseExpression "1 - 2") $ Right (BinOp Minus (Constant 1) (Constant 2))
  it "multiplication expression" $
     shouldBe (parseExpression "1 * 2") $ Right (BinOp Times (Constant 1) (Constant 2))
  it "nested expression" $
     shouldBe (parseExpression "(1 + 1) * 2") $ Right (BinOp Times (BinOp Plus (Constant 1) (Constant 1)) (Constant 2))

booleanExpressionTests = describe "CamleCompiler.Parser boolean expression parsing" $ do
  it "true" $
    shouldBe (parseBooleanExpression "true") $ wrapInBooleanExpBoilerplate (BTerm BTrue) 
  it "false" $
    shouldBe (parseBooleanExpression "false") $ wrapInBooleanExpBoilerplate (BTerm BFalse) 
  it "(false)" $
    shouldBe (parseBooleanExpression "(false)") $ wrapInBooleanExpBoilerplate (BTerm $ BExp $ BooleanExpression $ BTerm BFalse) 
  it "exp = exp" $
     shouldBe (parseBooleanExpression "1 = 1") $ wrapInBooleanExpBoilerplate (BTerm (BBinOp Equal (Constant 1) (Constant 1)))
  it "exp <= exp" $
     shouldBe (parseBooleanExpression "1 <= 1") $ wrapInBooleanExpBoilerplate (BTerm (BBinOp LessThanEqual (Constant 1) (Constant 1)))
  it "(true & false)" $
     shouldBe (parseBooleanExpression "(true & false)") $ wrapInBooleanExpBoilerplate $ BTerm . BExp $ BAnd [BTerm BTrue, BTerm BFalse]
  it "!true" $
     shouldBe (parseBooleanExpression "!true") $ wrapInBooleanExpBoilerplate $ BNegate BTrue


wrapInProgramBoilerplate statement = Right (Program [statement])
wrapInBooleanExpBoilerplate exp = Right (BooleanExpression exp)
