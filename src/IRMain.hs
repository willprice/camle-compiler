module Main where
import System.Environment

import Language.Camle.Parser
import Language.Camle.IntermediateRepresentation

main = do
        [filename] <- getArgs
        parseWhile filename
