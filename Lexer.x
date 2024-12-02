{
module Lexer 
    ( Token(..)
    , lexer
    ) where
}

%wrapper "basic"

tokens :-
  $white+                       ;
  \/\/.*$                       ; -- Single-line comment
  \/\*(.|\s)*\*\/               ; -- Multi-line comment 
  
  -- Delimiters
  "("                           { \_ -> LPAREN }
  ")"                           { \_ -> RPAREN }
  "{"                           { \_ -> LBRACE }
  "}"                           { \_ -> RBRACE }
  ","                           { \_ -> COMMA }
  ";"                           { \_ -> SEMICOLON }
  ":"                           { \_ -> COLON }
  
  -- Operators
  "="                           { \_ -> ASSIGN }
  "+"                           { \_ -> PLUS }
  "-"                           { \_ -> MINUS }
  "*"                           { \_ -> TIMES }
  "/"                           { \_ -> DIVIDE }
  "%"                           { \_ -> MOD }
  "=="                          { \_ -> EQUAL }
  "!="                          { \_ -> NEQ }
  "<"                           { \_ -> LTHAN }
  "<="                          { \_ -> LTE }
  ">"                           { \_ -> GTHAN }
  ">="                          { \_ -> GTE }
  "&&"                          { \_ -> AND }
  "||"                          { \_ -> OR }
  "!"                           { \_ -> NOT }
  
  -- Keywords and special operators
  fun                           { \_ -> FUN }
  val                           { \_ -> VAL }
  var                           { \_ -> VAR }
  if                            { \_ -> IF }
  else                          { \_ -> ELSE }
  while                         { \_ -> WHILE }
  return                        { \_ -> RETURN }
  print                         { \_ -> PRINT }
  readln                        { \_ -> READLN }
  
  -- Types
  Int                           { \_ -> INT }
  Boolean                       { \_ -> BOOLEAN }
  
  -- Literals
  [0-9]+                        { \s -> INTEGER (read s) } 
  \"([^\"\\]|\\.)*\"            { \s -> STRING_LIT (init (tail s)) }
  true                          { \_ -> BOOLEAN_LIT True }
  false                         { \_ -> BOOLEAN_LIT False }
  
  -- Identifiers
  ([a-zA-Z]|_)([a-zA-Z0-9]|_)*     { \s -> ID s }

{
data Token
  = ID String

  | INTEGER Int
  | BOOLEAN_LIT Bool
  | STRING_LIT String

  -- Delimiters
  | LPAREN | RPAREN | LBRACE | RBRACE
  | COMMA | SEMICOLON | COLON
  
  -- Arithmetic and comparison operators
  | PLUS | MINUS | TIMES | DIVIDE | MOD | ASSIGN
  | EQUAL | NEQ | LTHAN | LTE | GTHAN | GTE
  
  -- Logical operators
  | AND | OR | NOT
  
  -- Keywords
  | FUN | VAL | VAR | IF | ELSE | WHILE | RETURN | PRINT | READLN

  -- Types
  | INT | BOOLEAN
  deriving (Eq, Show)

lexer :: String -> [Token]
lexer = alexScanTokens
}