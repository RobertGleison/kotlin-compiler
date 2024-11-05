import Lexer (alexScanTokens)
import Parser (happyParseExpr)

main :: IO ()  -- lowercase 'main'
main = do
    input <- getContents 
    let tokens = alexScanTokens input
    putStrLn "Tokens generated:"
    print tokens
    putStrLn ""
    
    -- Get the AST using Parser.y
    --let parseResult = happyParseExpr tokens

    --case parseResult of

    --    Right ast -> do
    --        putStrLn "Success! AST:"
    --        print ast
            
    --    Left err -> do

    --        putStrLn "Failed! Error:"
    --        print err