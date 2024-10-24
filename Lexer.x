{
module Lexer where
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
  
  -- Special operators
  ".."                          { \_ -> RANGE }
  "?."                          { \_ -> SAFE_ACCESS }
  "?:"                          { \_ -> ELVIS }
  "::"                          { \_ -> SCOPE_RES }
  "=>"                          { \_ -> ARROW }

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
  "!in"                         { \_ -> NOT_IN }
  "is"                          { \_ -> IS }
  "!is"                         { \_ -> NOT_IS }
  "as"                          { \_ -> AS }
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

  -- Delimiters
  | LPAREN | RPAREN | LBRACE | RBRACE | LBRACK | RBRACK
  | COMMA | DOT | SEMICOLON | COLON

  -- Special operators
  | RANGE | SAFE_ACCESS | ELVIS | SCOPE_RES | ARROW

  -- Assignment operators
  | ASSIGN | PLUS_ASSIGN | MINUS_ASSIGN | TIMES_ASSIGN | DIV_ASSIGN | MOD_ASSIGN

  -- Arithmetic and comparison operators
  | PLUS | MINUS | TIMES | DIVIDE | MOD
  | INCREMENT | DECREMENT
  | EQUAL | NEQ | LTHAN | LTE | GTHAN | GTE

  -- Logical operators
  | AND | OR | NOT

  -- Keywords and special operators
  | IS | NOT_IS | AS | IN | NOT_IN

  -- Other keywords
  | FUN | VAL | VAR | IF | ELSE | WHEN | WHILE | FOR | RETURN
  | CLASS | INTERFACE | OBJECT | PACKAGE | IMPORT
  
  -- Types
  | INT | FLOAT | DOUBLE | BOOLEAN | STRING
  deriving (Eq, Show)
}