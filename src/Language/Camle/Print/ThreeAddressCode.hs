module Language.Camle.Print.ThreeAddressCode where

import Text.PrettyPrint
import Language.Camle.Data.ThreeAddressCode

class PrettyPrintable a where
        pprint :: a -> Doc

instance PrettyPrintable TAC where
        pprint = text . show

printIr :: Program -> Doc
printIr = vcat . (map pprint)
