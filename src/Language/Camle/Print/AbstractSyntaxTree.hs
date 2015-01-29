module Language.Camle.Print.AbstractSyntaxTree (pprint) where

import Text.PrettyPrint
import Language.Camle.Data.AbstractSyntaxTree
import Language.Camle.Print.Print

instance PrettyPrintable Program where
        pprint (Program []) = empty
        pprint (Program (stmt:rest)) = text "Seq"
                                   $$ (pprint stmt <+> pprint (Program rest))
instance PrettyPrintable Statement where
        pprint Skip = text "Skip"
        pprint (Assign var exp) = text ":="
                                $$ (pprint var <+> pprint exp)
        pprint (If boolexp s1 s2) = text "If"
                                  $$ (pprint s1 <+> pprint s2)
        pprint (While boolexp s) = text "While"
                                 $$ (pprint boolexp <+> pprint s)
        pprint (Read var) = text "Read"
                          $$ pprint var
        pprint (WriteString s) = text "WriteString" $$ text s
        pprint (WriteBoolean b) = text "WriteBoolean" $$ pprint b
        pprint (WriteExpression e) = text "WriteExpression" $$ pprint e

instance PrettyPrintable Expression where
        pprint (BinOp op e1 e2) = text "BinOp"
                                $$ (pprint op
                                $$ (pprint e1 <+> pprint e2))
        pprint (Negate exp) = text "Negate"
                            $$ pprint exp
        pprint (ETerm term) = pprint term

instance PrettyPrintable Term where
        pprint (Constant n) = int . fromIntegral $ n
        pprint (Var string) = text string

instance PrettyPrintable VarName where
        pprint (VarName name) = text name

instance PrettyPrintable BinaryArithmeticOperation where
        pprint op = char $ case op of 
                                      Times -> '*'
                                      Minus -> '-'
                                      Plus -> '+'

instance PrettyPrintable BooleanExpression where
        pprint (BooleanExpression term) = pprint term
        pprint (BAnd t1 t2) = char '&'
                            $$ (pprint t1 <+> pprint t2)

instance PrettyPrintable BooleanTerm where
        pprint _ = text ""

instance PrettyPrintable Boolean where
        pprint BTrue = text "True"
        pprint BFalse = text "False"
        pprint (BBinOp op e1 e2) = pprint op
                                 $$ (pprint e1 <+> pprint e2)

instance PrettyPrintable RelationalBinaryOp where
        pprint op = text $ case op of
                               Equal -> "="
                               LessThanEqual -> "<="
