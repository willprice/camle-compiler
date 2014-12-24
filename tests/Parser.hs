{-# OPTIONS_GHC -fno-warn-orphans #-}
import CamleTokenParser
import Text.Parsec
import Test.Tasty
import Test.Tasty.HUnit

-- Need to define how ParseErrors are compared
import Text.ParserCombinators.Parsec.Error(ParseError, Message, errorMessages, messageEq)

instance Eq ParseError where
   a == b = errorMessages a == errorMessages b


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "CAMLE parser tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Token parser tests" 
  [ testCase "'do' is parsed "   $ Right () @=? (parse (reserved "do") "fail" "do") 
  , testCase "'while' is parsed" $ Right () @=? (parse (reserved "while") "fail" "while")
  ]
