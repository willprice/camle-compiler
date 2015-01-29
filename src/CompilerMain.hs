{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}
module Main where
import System.Environment

import qualified Language.Camle.Parser as P
import qualified Language.Camle.AstToIr as ATI
import qualified Language.Camle.JouetteBackend as JB
import Control.Applicative
import Text.Parsec
import System.Console.CmdLib

data Opts = Opts {
          program_filename :: FilePath,
          stdout :: Bool,
          output_filename :: FilePath
          }
          deriving (Typeable, Data, Eq, Ord)

instance Attributes Opts where
        attributes _ = group "Options" [
                program_filename %> [Help "The file name of the program you wish to compiler"],
                stdout %> [Help "Print assembly to STDOUT"],
                output_filename %> [Help "The name of the file the compiler will output assembly code to"]]
main = do
        [filename] <- getArgs
        src <- readFile filename
        printAsm . compile $ src

compile :: String -> Either ParseError JB.Code
compile program = (JB.irToAsm . ATI.astToIr) <$> P.parse program

printAsm :: (Either ParseError JB.Code) -> IO ()
printAsm (Right program) = do
        putStr . show $ program 
printAsm (Left error) = print error
