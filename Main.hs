module Main where

import qualified Data.List as L
import System.Environment
import System.IO
import System.Exit  
import Control.Monad
import Lexer (Token(..), lexer)
import Parser (parse)
import AST
import Printer
import Printer (prettyPrintAST)



-- Tipo para tratamento de erros do compilador
data CompilerError 
    = LexerError String    -- Erro durante análise léxica
    | ParserError String   -- Erro durante análise sintática
    deriving Show



-- Pipeline principal de processamento do arquivo
processFile :: String -> IO ()
processFile input = do
    -- Passo 1: Análise Léxica (quebra o código em tokens)
    let tokens = lexer input
    putStrLn "Tokens:"
    print tokens
    -- print "\nhere\n"
    -- Passo 2: Análise Sintática (converte tokens em AST)
    case parse tokens of
        Right ast -> do
            putStrLn "\nAbstract Syntax Tree:"
            putStrLn $ prettyPrintAST ast
            
        Left err -> do
            putStrLn $ "Parser error: " ++ err
            exitFailure



-- Função para testes rápidos de input
testInput :: String -> IO ()
testInput input = do
    putStrLn "Test input:"
    putStrLn input
    putStrLn "\nProcessing..."
    processFile input



-- Função principal do programa
main :: IO ()
main = do
    args <- getArgs
    case args of
        -- Processamento de arquivo específico
        [filename] -> do
            putStrLn $ "Reading from file: " ++ filename
            input <- readFile filename
            processFile input
            
        -- Leitura da entrada padrão
        [] -> do
            putStrLn "Reading from standard input (type your code, press Ctrl+D when done):"
            input <- getContents
            processFile input
            
        -- Mensagem de uso em caso de argumentos inválidos
        _ -> do
            putStrLn "Usage: ./Main [filename]"
            putStrLn "If no filename is provided, input will be read from stdin"
            exitFailure