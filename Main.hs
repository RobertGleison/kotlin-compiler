import Lexer (lexer)

main :: IO ()
main = do
    input <- getContents 
    let tokens = lexer input
    putStrLn "Tokens generated:"
    print tokens
    putStrLn ""
    
    -- Get the AST using Parser.y
    let parseResult = parser tokens

    case parseResult of

        Right ast -> do
            putStrLn "Success! AST:"
            print ast
            
        Left err -> do

            putStrLn "Failed! Error:"
            print err