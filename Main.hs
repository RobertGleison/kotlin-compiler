module Main where

import System.Environment (getArgs)
import System.IO (readFile)
import Control.Monad (when)
import System.Exit (exitFailure)

import Lexer (Token, lexer)
import Parser ( parse
             , AST(..)
             , Program(..)
             , Function(..)
             , Type(..)
             , Param(..)
             , Declare(..)
             , Stmt(..)
             )

-- For better error handling
data CompilerError 
    = LexerError String
    | ParserError String
    deriving Show

-- Helper function to print AST in a readable format
prettyPrintAST :: AST -> String
prettyPrintAST (Program fns) = unlines $ map showFunction fns
  where
    showFunction (Function name params retType decls stmts) =
        "Function: " ++ name ++ "\n" ++
        "  Parameters: " ++ showParams params ++ "\n" ++
        "  Return Type: " ++ showType retType ++ "\n" ++
        "  Declarations: " ++ showDecls decls ++ "\n" ++
        "  Statements: " ++ showStmts stmts ++ "\n"

    showParams [] = "none"
    showParams ps = unlines $ map (\(Param name typ) -> "    " ++ name ++ ": " ++ showType typ) ps

    showType IntType = "Int"
    showType DoubleType = "Double"
    showType BooleanType = "Boolean"
    showType StringType = "String"
    showType FloatType = "Float"
    showType UnitType = "Unit"

    showDecls [] = "none"
    showDecls ds = unlines $ map showDecl ds

    showDecl (ValDecl name typ expr) = "    val " ++ name ++ ": " ++ showType typ
    showDecl (VarDecl name typ expr) = "    var " ++ name ++ ": " ++ showType typ
    showDecl (VarDeclEmpty name typ) = "    var " ++ name ++ ": " ++ showType typ

    showStmts [] = "none"
    showStmts ss = unlines $ map (("    " ++) . show) ss

-- Main processing pipeline
processFile :: String -> IO ()
processFile input = do
    -- Step 1: Lexical Analysis
    let tokens = lexer input
    putStrLn "Tokens:"
    print tokens
    
    -- Step 2: Parsing
    case parse tokens of
        Right ast -> do
            putStrLn "\nAbstract Syntax Tree:"
            putStrLn $ prettyPrintAST ast
            
        Left err -> do
            putStrLn $ "Parser error: " ++ err
            exitFailure

-- Test function for quick tests
testInput :: String -> IO ()
testInput input = do
    putStrLn "Test input:"
    putStrLn input
    putStrLn "\nProcessing..."
    processFile input

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            putStrLn $ "Reading from file: " ++ filename
            input <- readFile filename
            processFile input
            
        [] -> do
            putStrLn "Reading from standard input (type your code, press Ctrl+D when done):"
            input <- getContents
            processFile input
            
        _ -> do
            putStrLn "Usage: ./Main [filename]"
            putStrLn "If no filename is provided, input will be read from stdin"
            exitFailure