module Main where
import System.Environment

import CamleCompiler.Parser

main = do
        args <- getArgs
        code <- readFile $ head args
        case (parse code) of
            Left err -> print err
            Right ans -> print ans
