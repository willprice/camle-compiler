module Main where
import System.Environment

import Language.Camle.Parser

main = do
        [filename] <- getArgs
        ast <- parseWhile filename
        case ast of
            Left err -> print err
            Right ans -> print ans
