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
import SemanticAnalyzer (checkProgram)
import IR (IRInstr, IRProg)
import IRTranslator (translateProgram)
import MIPSGenerator (generateAssembly)

-- Tipo para tratamento de erros do compilador
data CompilerError 
    = LexerError String     -- Erro durante análise léxica
    | ParserError String    -- Erro durante análise sintática
    | SemanticError String  -- Erro durante análise semântica
    | IRError String        -- Erro durante geração de IR
    | MIPSError String      -- Erro durante geração de MIPS
    deriving Show

-- Pipeline principal de processamento do arquivo
processFile :: String -> IO ()
processFile input = do
    -- Passo 1: Análise Léxica (quebra o código em tokens)
    let tokens = lexer input
    putStrLn "Tokens:"
    print tokens
    
    -- Passo 2: Análise Sintática (converte tokens em AST)
    case parse tokens of
        Right ast -> do
            putStrLn "\nAbstract Syntax Tree:"
            putStrLn $ prettyPrintAST ast
            
            -- Passo 3: Análise Semântica (verifica tipos e escopo)
            putStrLn "\nPerforming Semantic Analysis..."
            case checkProgram ast of
                Right _ -> do
                    putStrLn "Semantic analysis completed successfully."
                    
                    -- Passo 4: Geração de IR
                    putStrLn "\nGenerating IR code..."
                    let irCode = translateProgram ast
                    putStrLn "IR Code generated:"
                    print irCode
                    
                    -- Passo 5: Geração de código MIPS
                    putStrLn "\nGenerating MIPS assembly..."
                    let mipsCode = generateAssembly irCode
                    putStrLn "MIPS Code generated:"
                    putStrLn mipsCode
                    
                    -- Salvar código MIPS em arquivo
                    let outputFile = "output.asm"
                    writeFile outputFile mipsCode
                    putStrLn $ "\nMIPS assembly written to " ++ outputFile
                
                Left err -> do
                    putStrLn $ "Semantic error: " ++ err
                    exitFailure
            
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

-- Função para exibir mensagem de erro e sair
handleCompilerError :: CompilerError -> IO ()
handleCompilerError (LexerError msg) = do
    putStrLn $ "Lexical Error: " ++ msg
    exitFailure
handleCompilerError (ParserError msg) = do
    putStrLn $ "Parser Error: " ++ msg
    exitFailure
handleCompilerError (SemanticError msg) = do
    putStrLn $ "Semantic Error: " ++ msg
    exitFailure
handleCompilerError (IRError msg) = do
    putStrLn $ "IR Generation Error: " ++ msg
    exitFailure
handleCompilerError (MIPSError msg) = do
    putStrLn $ "MIPS Generation Error: " ++ msg
    exitFailure

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