module Main where

import Test.Tasty
import Test.Hspec
import Test.Tasty.Hspec

import ParserTests
import IRTests

main = testSpec "CamleTests" tests >>= defaultMain

tests = describe "CamleTests" $ do
    parserSpec
    irSpec
