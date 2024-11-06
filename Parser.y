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


data Function = Function {
    funcName :: String,        -- nome da função
    params :: [Parameter],     -- lista de parâmetros
    returnType :: Type,        -- tipo de retorno
    declarations :: [Declare], -- declarações locais
    statements :: [Stmt]       -- statements da função
}

-- Estrutura de um parâmetro
data Parameter = Parameter {
    paramName :: String,       -- nome do parâmetro
    paramType :: Type         -- tipo do parâmetro
}

-- Tipos possíveis em Kotlin
data Type 
    = IntType                -- Int
    | StringType            -- String
    | BooleanType          -- Boolean
    | DoubleType           -- Double
    | FloatType            -- Float
    | UnitType             -- Unit (void)
    deriving (Show, Eq)

-- Declarações possíveis
data Declare 
    = ValDecl String Type Expr  -- val x: Int = 10
    | VarDecl String Type Expr  -- var x: Int = 10
    deriving (Show, Eq)

-- Statements possíveis
data Stmt 
    = Return Expr              -- return x
    | If Expr [Stmt] [Stmt]    -- if (cond) { ... } else { ... }
    | While Expr [Stmt]        -- while (cond) { ... }
    | ExprStmt Expr           -- x = 10, foo(), etc.
    deriving (Show, Eq)

-- Expressões possíveis
data Expr 
    = IntLit Int              -- literais inteiros: 42
    | StringLit String        -- literais string: "hello"
    | BoolLit Bool           -- literais boolean: true/false
    | Var String             -- variáveis: x, y, etc
    | BinOp Op Expr Expr     -- operações binárias: x + y
    | Call String [Expr]     -- chamadas de função: foo(x, y)
    deriving (Show, Eq)

-- Operadores binários
data Op 
    = Plus | Minus | Times | Div  -- +, -, *, /
    | Eq | Neq | Lt | Gt         -- ==, !=, <, >
    | And | Or                    -- &&, ||
    deriving (Show, Eq)





-- Aqui começa a putaria da gramática


-- Inicio da gramática, pega o input inteiro e joga numa variavel ProgList que é uma lista de códigos, nomeadamente uma lista de funções
Prog : ProgList              { Program $1 }  


-- Aqui ele pega a lista dos códigos e ve se é vazia. Se vazia retorna nada, se tiver código lá ele processa a primeira em $1 e o resto em $2. a primeira vira uma função 
-- e a segunda é recursiva e volta pra ProgList
ProgList 
    : Function ProgList      { $1 : $2 }      
    |                        { [] }      


-- Aqui tem as duas formas de uma função kotlin, a primeira é uma função com retorno e a segunda é uma função void.
-- Os numeros são onde ficam os tokens: Function : fun id '(' ParamList ')' ':' ReturnType '{' Declares Stmts '}'
--                                                 $1  $2  $3    $4     $5  $6     $7     $8    $9     $10   $11
-- Queremos passar para o tipo Function, as partes que tem valor, como: id, ParamList, ReturnType, Declares e Stmts
Function 
    : fun id '(' ParamList ')' ':' ReturnType '{' Declares Stmts '}'  
        { Function $2 $4 $7 $9 $10 }
    | fun id '(' ParamList ')' '{' Declares Stmts '}'             
        { Function $2 $4 UnitType $8 $9 }  

-- Lista de parâmetros da função
-- Pode ser:
--   - vazia: fun teste() 
--   - um único parâmetro: fun teste(x: Int)
--   - múltiplos parâmetros: fun teste(x: Int, y: String)
ParamList 
    : Param ',' ParamList    { $1 : $3 }      -- vários parâmetros separados por vírgula
    | Param                  { [$1] }         -- apenas um parâmetro
    |                        { [] }           -- sem parâmetros

-- Definição de um parâmetro individual
-- Formato: nome: Tipo
-- Exemplo: idade: Int
Param 
    : id ':' Type           { Parameter $1 $3 }

-- Tipo de retorno da função
-- Exemplo: Int, String, Boolean, etc
ReturnType 
    : Type                  { $1 }

-- Tipos que podem ser usados em Kotlin
Type 
    : Int                   { IntType }
    | String                { StringType }
    | Boolean               { BooleanType }
    | Double                { DoubleType }
    | Float                 { FloatType }
    | Unit                  { UnitType }      -- void em outras linguagens

-- Declarações dentro da função (val, var)
Declares 
    : DeclareList           { $1 }
    |                       { [] }           -- pode não ter declarações

DeclareList
    : Declare DeclareList   { $1 : $2 }     -- lista de declarações
    |                       { [] }

-- Statements dentro da função (if, while, return, etc)
Stmts 
    : StmtList              { $1 }
    |                       { [] }           -- pode não ter statements

StmtList
    : Stmt StmtList         { $1 : $2 }     -- lista de statements
    |                       { [] }


}