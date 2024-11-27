-- Exporta os módulos para Main.hs
{
module Lexer 
    ( Token(..)
    , lexer     -- Export lexer instead of alexScanTokens
    ) where
}
%wrapper "basic"



-- Regex para digitos, letras e alfanuméricos
$digit = 0-9
$alpha = [a-zA-Z]
$alphanum = [a-zA-Z0-9]
$whitespace = [\t\f\v]


-- Mapeamento de caracteres para Tokens
tokens :-
  $white+                      ;
  \/\/.*$                       ; -- Single-line comment
  \/\*(.|\s)*\*\/               ; -- Multi-line comment
  
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
  -- \r?\n                         { \_ -> NEXTLINE }
  
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
  main                          { \_ -> MAIN }
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
  Double                        { \_ -> DOUBLE }
  Char                          { \_ -> CHAR }
  Boolean                       { \_ -> BOOLEAN }
  String                        { \_ -> STRING }
  
  -- Literals
  $digit+                       { \s -> INTEGER (read s) }
  $digit+\.$digit+              { \s -> DOUBLE_LIT (read s) }
  \"([^\"]|\\.)*\"              { \s -> STRING_LIT (init (tail s)) }
  \'$alpha\'                        { \s -> CHAR_LIT (read s) }
  true                          { \_ -> BOOLEAN_LIT True }
  false                         { \_ -> BOOLEAN_LIT False }
  
  -- Identifiers
  $alpha($alphanum|_)*          { \s -> ID s }


-- Criação dos Tokens
{
data Token
  = ID String
  | INTEGER Int
  | DOUBLE_LIT Double
  | STRING_LIT String
  | CHAR_LIT Char
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
  | FUN | MAIN | VAL | VAR | IF | ELSE | WHILE | RETURN | PRINT | READLN

  -- Types
  | INT | DOUBLE | CHAR | BOOLEAN | STRING
  deriving (Eq, Show)


-- Pega uma String como input e devolve uma lista de Tokens
lexer :: String -> [Token]
lexer = alexScanTokens
}