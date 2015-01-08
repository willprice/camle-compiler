module Main where
import System.Environment

import CamleCompiler.Parser

main = do
        [filename] <- getArgs
        ast <- parseWhile filename
        case ast of
            Left err -> print err
            Right ans -> print ans
