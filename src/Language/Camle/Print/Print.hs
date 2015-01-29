module Language.Camle.Print.Print where
import Text.PrettyPrint

class PrettyPrintable a where
        pprint :: a -> Doc
