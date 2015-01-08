module Main where
import System.Environment

import CamleCompiler.Parser
import CamleCompiler.IR

main = do
        [filename] <- getArgs
        parseWhile filename
