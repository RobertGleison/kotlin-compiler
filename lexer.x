{
module Main (main) where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$alphanum = [a-zA-Z0-9]

tokens :-
  $white+                       ;
  "--".*                        ; -- Single-line comment
  "/*"(.|\n)*"*/"               ; -- Multi-line comment
  
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

  -- Keywords
  fun                           { \_ -> FUN }
  val                           { \_ -> VAL }
  var                           { \_ -> VAR }
  if                            { \_ -> IF }
  else                          { \_ -> ELSE }
  when                          { \_ -> WHEN }
  while                         { \_ -> WHILE }
  for                           { \_ -> FOR }
  in                            { \_ -> IN }
  return                        { \_ -> RETURN }
  class                         { \_ -> CLASS }
  interface                     { \_ -> INTERFACE }
  object                        { \_ -> OBJECT }
  package                       { \_ -> PACKAGE }
  import                        { \_ -> IMPORT }

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
  | LPAREN | RPAREN | LBRACE | RBRACE | LBRACK | RBRACK
  | COMMA | DOT | SEMICOLON | COLON
  | ASSIGN | PLUS | MINUS | TIMES | DIVIDE | MOD
  | EQUAL | NEQ | LTHAN | LTE | GTHAN | GTE
  | AND | OR | NOT
  | FUN | VAL | VAR | IF | ELSE | WHEN | WHILE | FOR | IN | RETURN
  | CLASS | INTERFACE | OBJECT | PACKAGE | IMPORT
  | INT | FLOAT | DOUBLE | BOOLEAN | STRING
  deriving (Eq, Show)

main :: IO ()
main = do
  s <- getContents
  print (alexScanTokens s)
}