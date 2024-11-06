{
module Parser where
import Data.Maybe
import qualified Data.List as L
}

%name parser
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
  while           { WHILE }
  for             { FOR }
  in              { IN }
  return          { RETURN }
  
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
%left '+' '-'                          -- additive
%left '*' '/' '%'                      -- multiplicative
%right PREFIX                          -- for prefix operators (-, !, etc.)
%left '.' '++' '--'                    -- highest precedence (postfix)

%%

Prog : ProgList              { Program $1 }  


ProgList : Function ProgList      { $1 : $2 }      
         |                        { [] }      


Function : fun id '(' ParamList ')' ':' Type '{' Declares Stmts '}' { Function $2 $4 $7 $9 $10 }
         | fun id '(' ParamList ')' '{' Declares Stmts '}' { Function $2 $4 UnitType $7 $8 }


Type : Int     { IntType }
     | Double   { DoubleType }
     | Boolean  { BooleanType }
     | String   { StringType }
     | Float    { FloatType }


ParamList : Param ',' ParamList   { $1 : $3 }
          | Param                  { [$1] }
          |                       { [] }


Param : id ':' Type        { Param $1 $3 }


Declares : Declare Declares      { $1 : $2 }
        |                       { [] }


Declare : val id ':' Type '=' Expr ';'  { ValDecl $2 $4 $6 }
        | var id ':' Type '=' Expr ';'  { VarDecl $2 $4 $6 }
        | var id ':' Type ';'           { VarDeclEmpty $2 $4 }


Stmts : Stmt Stmts              { $1 : $2 }
      |                        { [] }


Stmt : Expr ';'                { ExprStmt $1 }
     | return Expr ';'         { ReturnStmt $2 }
     | if '(' Expr ')' '{' Stmts '}' else '{' Stmts '}'  { IfStmt $3 $6 $10 }
     | if '(' Expr ')' '{' Stmts '}'                     { IfStmt $3 $6 [] }
     | while '(' Expr ')' '{' Stmts '}'                  { WhileStmt $3 $6 }
     | for '(' id in Expr ')' '{' Stmts '}'             { ForStmt $3 $5 $8 }
