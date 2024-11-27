module Printer
    ( prettyPrintAST
    ) where

import AST



-- Função principal para imprimir a AST em formato legível
prettyPrintAST :: AST -> String
prettyPrintAST (Program fns) = unlines $ map showFunction fns



-- Funções auxiliares de formatação
showFunction :: Function -> String
showFunction (Function name params retType cmds) =
    "Function: " ++ name ++ "\n" ++
    "  Parameters: " ++ showParams params ++ "\n" ++
    "  Return Type: " ++ showType retType ++ "\n" ++
    "  Commands: " ++ showCmds cmds ++ "\n"
showFunction (Main params cmds) =
    "Function: Main \n"  ++
    "  Parameters: " ++ showParams params ++ "\n" ++
    "  Commands: " ++ showCmds cmds ++ "\n"


-- Formatação de parâmetros
showParams :: [Param] -> String
showParams [] = "none"
showParams ps = unlines $ map (\(Param name typ) -> "    " ++ name ++ ": " ++ showType typ) ps



-- Conversão de tipos para string
showType :: Type -> String
showType IntType = "Int"
showType BooleanType = "Boolean"
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
showCmds :: [Cmd] -> String
showCmds [] = "none"
showCmds ss = unlines $ map (("    " ++) . show) ss