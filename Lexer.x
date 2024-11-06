{
module Lexer 
    ( Token(..)
    , lexer     -- Export lexer instead of alexScanTokens
    ) where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$alphanum = [a-zA-Z0-9]

tokens :-
  $white+                       ;
  "//".*                        ; -- Single-line comment
  "/*"(.|\n)*"*/"              ; -- Multi-line comment
  
  -- Delimiters
  "("                           { \_ -> LPAREN }
  ")"                           { \_ -> RPAREN }
  "{"                           { \_ -> LBRACE }
  "}"                           { \_ -> RBRACE }
  "["                           { \_ -> LBRACK }
  "]"                           { \_ -> RBRACK }
  ","                           { \_ -> COMMA }
  "."                           { \_ -> DOT }
  ";"                           { \_ -> SEMICOLON }
  ":"                           { \_ -> COLON }
  
  -- Operators
  "+="                          { \_ -> PLUS_ASSIGN }
  "-="                          { \_ -> MINUS_ASSIGN }
  "*="                          { \_ -> TIMES_ASSIGN }
  "/="                          { \_ -> DIV_ASSIGN }
  "%="                          { \_ -> MOD_ASSIGN }
  "="                           { \_ -> ASSIGN }
  "++"                          { \_ -> INCREMENT }
  "--"                          { \_ -> DECREMENT }
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
  for                           { \_ -> FOR }
  in                            { \_ -> IN }
  return                        { \_ -> RETURN }
  
  -- Types
  Int                           { \_ -> INT }
  Float                         { \_ -> FLOAT }
  Double                        { \_ -> DOUBLE }
  Boolean                       { \_ -> BOOLEAN }
  String                        { \_ -> STRING }
  
  -- Literals
  $digit+                       { \s -> INTEGER (read s) }
  $digit+\.$digit+              { \s -> DOUBLE_LIT (read s) }
  \"([^\"]|\\.)*\"              { \s -> STRING_LIT (init (tail s)) }
  true                          { \_ -> BOOLEAN_LIT True }
  false                         { \_ -> BOOLEAN_LIT False }
  
  -- Identifiers
  $alpha($alphanum|_)*          { \s -> ID s }

{
data Token
  = ID String
  | INTEGER Int
  | DOUBLE_LIT Double
  | STRING_LIT String
  | BOOLEAN_LIT Bool

  -- Delimiters
  | LPAREN | RPAREN | LBRACE | RBRACE | LBRACK | RBRACK
  | COMMA | DOT | SEMICOLON | COLON

  -- Assignment operators
  | ASSIGN | PLUS_ASSIGN | MINUS_ASSIGN | TIMES_ASSIGN | DIV_ASSIGN | MOD_ASSIGN

  -- Arithmetic and comparison operators
  | PLUS | MINUS | TIMES | DIVIDE | MOD
  | INCREMENT | DECREMENT
  | EQUAL | NEQ | LTHAN | LTE | GTHAN | GTE

  -- Logical operators
  | AND | OR | NOT

  -- Keywords
  | FUN | VAL | VAR | IF | ELSE | WHILE | FOR | IN | RETURN
  
  -- Types
  | INT | FLOAT | DOUBLE | BOOLEAN | STRING
  deriving (Eq, Show)

lexer :: String -> [Token]
lexer = alexScanTokens
}