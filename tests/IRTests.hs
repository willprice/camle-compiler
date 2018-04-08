{-# OPTIONS_GHC -fno-warn-orphans #-}
module IRTests where

import CamleCompiler.Parser
import CamleCompiler.AST
import CamleCompiler.IR
import Test.Tasty
import Test.Hspec
import Test.Tasty.Hspec
import qualified Text.ParserCombinators.Parsec



irSpec = describe "CamleCompiler.IR intermediate representation" $ do
  it "converting writeln to WRS" $ 
    undefined
  it "converting write(exp) to WR" $ 
    undefined
