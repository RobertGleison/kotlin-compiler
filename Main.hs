import Lexer (lexer)
import Parser (parser)

main :: IO ()  -- lowercase 'main'
main = do
    input <- getContents 
    let tokens = lexer input
    putStrLn "Tokens generated:"
    print tokens
    putStrLn ""
    
    -- Get the AST using Parser.y
    --let parseResult = parser tokens

    --case parseResult of

    --    Right ast -> do
    --        putStrLn "Success! AST:"
    --        print ast
            
    --    Left err -> do

    --        putStrLn "Failed! Error:"
    --        print err