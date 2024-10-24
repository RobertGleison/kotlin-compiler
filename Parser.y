{
module Parser where

import Data.Maybe
import qualified Data.List as L
}

%name happyParseExpr
%tokentype { Token }
%error { parseError }

%token
  -- Literals and identifiers
  id              { ID $$ }
  int             { INTEGER $$ }
  double          { DOUBLE_LIT $$ }
  string          { STRING_LIT $$ }
  bool            { BOOLEAN_LIT $$ }
  
  -- Delimiters
  '('             { LPAREN }
  ')'             { RPAREN }
  '{'             { LBRACE }
  '}'             { RBRACE }
  '['             { LBRACK }
  ']'             { RBRACK }
  ','             { COMMA }
  '.'             { DOT }
  ';'             { SEMICOLON }
  ':'             { COLON }
  
  -- Operators
  '='             { ASSIGN }
  '+='            { PLUS_ASSIGN }
  '-='            { MINUS_ASSIGN }
  '*='            { TIMES_ASSIGN }
  '/='            { DIV_ASSIGN }
  '%='            { MOD_ASSIGN }
  '+'             { PLUS }
  '-'             { MINUS }
  '*'             { TIMES }
  '/'             { DIVIDE }
  '%'             { MOD }
  '++'            { INCREMENT }
  '--'            { DECREMENT }
  '=='            { EQUAL }
  '!='            { NEQ }
  '<'             { LTHAN }
  '<='            { LTE }
  '>'             { GTHAN }
  '>='            { GTE }
  '&&'            { AND }
  '||'            { OR }
  '!'             { NOT }
  
  -- Keywords
  fun             { FUN }
  val             { VAL }
  var             { VAR }
  if              { IF }
  else            { ELSE }
  when            { WHEN }
  while           { WHILE }
  for             { FOR }
  in              { IN }
  return          { RETURN }
  class           { CLASS }
  interface       { INTERFACE }
  object          { OBJECT }
  package         { PACKAGE }
  import          { IMPORT }

  -- Keywords and special operators
  in              { IN }
  '!in'           { NOT_IN }
  is              { IS }
  '!is'           { NOT_IS }
  as              { AS }
  '?.'            { SAFE_ACCESS }
  '?:'            { ELVIS }
  '::'            { SCOPE_RES }
  '=>'            { ARROW }
  '..'            { RANGE }
  
  -- Types
  Int             { INT }
  Float           { FLOAT }
  Double          { DOUBLE }
  Boolean         { BOOLEAN }
  String          { STRING }

-- Operator Precedence
%right '=' '+=' '-=' '*=' '/=' '%='    -- lowest precedence
%left '||'
%left '&&'
%nonassoc '==' '!='                    -- separate equality
%nonassoc '<' '<=' '>' '>='            -- separate comparison
%left 'in'                             -- named checks
%left '..'                             -- range
%left '+' '-'                          -- additive
%left '*' '/' '%'                      -- multiplicative
%right PREFIX                          -- for prefix operators (-, !, etc.)
%left '.' '++' '--'                    -- highest precedence (postfix)

%%

}