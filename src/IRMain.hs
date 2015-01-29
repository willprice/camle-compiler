module Main where
import System.Environment

import Language.Camle.Parser
import Language.Camle.AstToIr
import Text.PrettyPrint

main = do
        [filename] <- getArgs
        ast <- parseWhile filename
        case ast of 
                    (Left error) -> print error
                    (Right program) -> putStr . render . printIr . astToIr $ program

