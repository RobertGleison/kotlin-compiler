module Printer
    ( prettyPrintAST
    ) where

import AST



-- Função principal para imprimir a AST em formato legível
prettyPrintAST :: AST -> String
prettyPrintAST (Program fns) = unlines $ map showFunction fns



-- Funções auxiliares de formatação
showFunction :: Function -> String
showFunction (Function name params retType stmts) =
    "Function: " ++ name ++ "\n" ++
    "  Parameters: " ++ showParams params ++ "\n" ++
    "  Return Type: " ++ showType retType ++ "\n" ++
    -- "  Declarations: " ++ showDecls decls ++ "\n" ++
    "  Statements: " ++ showStmts stmts ++ "\n"



-- Formatação de parâmetros
showParams :: [Param] -> String
showParams [] = "none"
showParams ps = unlines $ map (\(Param name typ) -> "    " ++ name ++ ": " ++ showType typ) ps



-- Conversão de tipos para string
showType :: Type -> String
showType IntType = "Int"
showType DoubleType = "Double"
showType BooleanType = "Boolean"
showType StringType = "String"
showType FloatType = "Float"
showType UnitType = "Unit"



-- Formatação de declarações
-- showDecls :: [Declare] -> String
-- showDecls [] = "none"
-- showDecls ds = unlines $ map showDecl ds



-- Mostra diferentes tipos de declarações
-- showDecl :: Declare -> String
-- showDecl (ValDecl name typ expr) = "    val " ++ name ++ ": " ++ showType typ
-- showDecl (VarDecl name typ expr) = "    var " ++ name ++ ": " ++ showType typ
-- showDecl (VarDeclEmpty name typ) = "    var " ++ name ++ ": " ++ showType typ



-- Formatação de statements
showStmts :: [Stmt] -> String
showStmts [] = "none"
showStmts ss = unlines $ map (("    " ++) . show) ss