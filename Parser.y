-- Exporta os modulos para Main.hs e importa outros recursos, dentre eles o arquivo AST que contem as data structures
{
module Parser 
    ( parse
    ) where

import Data.Maybe
import qualified Data.List as L
import Lexer (Token(..))
import AST
}



%name parser
%tokentype { Token }
%error { parseError }
%monad { Either String } { (>>=) } { return }



-- Mapeamento dos inputs para Tokens. Os tipos de tokens vem do Lexer.
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
  return          { RETURN }
  print           { PRINT }
  readln          { READLN }
  
  -- Types
  Int             { INT }
  Float           { FLOAT }
  Double          { DOUBLE }
  Boolean         { BOOLEAN }
  String          { STRING }



-- Precedência de operadores. Uma multiplicação por exemplo tem precedência sobre uma soma, e por aí vai.
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




-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: INÍCIO DA GRAMÁTICA :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

Prog : ProgList              { Program $1 }   -- Um programa consiste em uma lista de funções
  


-- Lista de funções que compõem o programa
ProgList : Function ProgList      { $1 : $2 }  -- Uma ou mais funções      
         |                        { [] }       -- Programa vazio é válido




--    Declarações de função com duas formas:
--        -Com tipo de retorno: fun soma(x: Int): Int { return x + 1 }
--        -Sem tipo retorno: fun hello(name: String) { println(name) }

--        fun id '(' ParamList ')' ':' Type '{' Stmts '}'
--        $1  $2 $3     $4     $5  $6   $7   $8   $9  $10

--        fun id '(' ParamList ')' '{' Stmts '}'
--        $1  $2 $3     $4     $5  $6    $7  $8    

Function : fun id '(' ParamList ')' ':' Type '{' Stmts '}' { Function $2 $4 $7 $9 } 
         | fun id '(' ParamList ')' '{' Stmts '}' { Function $2 $4 UnitType $7 } 



-- Tipos básicos suportados na linguagem
Type : Int     { IntType }
     | Double   { DoubleType }
     | Boolean  { BooleanType }
     | String   { StringType }
     | Float    { FloatType }



-- Lista de parâmetros em declarações de função. Ex: [Param "altura" DoubleType, Param "largura" DoubleType, Param "profundidade" DoubleType]
ParamList : Param ',' ParamList   { $1 : $3 }   -- Múltiplos parâmetros separados por vírgula
          | Param                  { [$1] }     -- Parâmetro único
          |                       { [] }        -- Lista de parâmetros vazia



-- Declaração individual de parâmetro com anotação de tipo. Ex: [Idade, IntType]
Param : id ':' Type        { Param $1 $3 }



-- Lista de declarações no corpo da função
Stmts : Stmt Stmts              { $1 : $2 }
      |                            { [] }



-- Separa o statement de If para analisar separadamente.
Stmt : Declare OptSemiColon        { $1 }
     | IfStmt                      { $1 }
     | WhileStmt                   { $1 }
     | OtherStmt OptSemiColon                  { $1 }

OptSemiColon : ';'                 {}
             |                     {}

-- Cinco formas de declarações:
-- 1. Valor imutável (val) com inicialização
-- 2. Valor imutável (val) com inicialização sem tipo explícito
-- 3. Variável mutável (var) com inicialização
-- 4. Variável mutável (var) com inicialização sem tipo explícito
-- 5. Variável mutável (var) sem inicialização
-- Repetição para caso o ';' não seja colocado
Declare : val id ':' Type '=' Expr      { ValDecl $2 $4 $6 }    -- val x: Int = 5;
        -- | val id ':' Type '=' Expr ';'  { ValDecl $2 $4 $6 }    -- val x: Int = 5
        -- | val id '=' Expr ';'           { ValDecl $2 UnitType $4 } -- val x = 5;
        | val id '=' Expr               { ValDecl $2 UnitType $4 } -- val x = 5
        -- | var id ':' Type '=' Expr ';'  { VarDecl $2 $4 $6 }    -- var x: Int = 5;
        | var id ':' Type '=' Expr      { VarDecl $2 $4 $6 }    -- var x: Int = 5
        -- | var id '=' Expr ';'           { VarDecl $2 UnitType $4 } -- var x = 5;
        | var id '=' Expr               { VarDecl $2 UnitType $4 } -- var x = 5
        -- | var id ':' Type ';'           { VarDeclEmpty $2 $4 }   -- var x: Int;
        | var id ':' Type               { VarDeclEmpty $2 $4 }   -- var x: Int
        



-- Isso aq parece que tem ambiguidade, mas é tratado pelo happy.
IfStmt : if '(' Expr ')' '{' Stmts '}' else '{' Stmts '}'  { IfStmt $3 $6 $10 }
       | if '(' Expr ')' '{' Stmts '}'                     { IfStmt $3 $6 [] }



WhileStmt : while '(' Expr ')' '{' Stmts '}'               { WhileStmt $3 $6 }



-- Statements sem ser If
OtherStmt : Expr                                       { ExprStmt $1 }
        --   | Expr                                  { ExprStmt $1 }
          | return Expr                                { ReturnStmt $2 }
        --   | return Expr                            { ReturnStmt $2 }



-- Lista de expressões (para chamadas de função, etc.)
ExprList : Expr ',' ExprList        { $1 : $3 }
         | Expr                     { [$1] }
         |                          { [] }



-- Expressões podem ser atribuições ou expressões lógicas
Expr : AssignExpr                { $1 }
     | LogicalExpr               { $1 }



-- Expressões de atribuição (simples e compostas)
AssignExpr : id '=' LogicalExpr          { Assignment $1 $3 }
           | id '+=' AddExpr             { CompoundAssign $1 Add $3 }
           | id '-=' AddExpr             { CompoundAssign $1 Sub $3 }
           | id '*=' AddExpr             { CompoundAssign $1 Mul $3 }
           | id '/=' AddExpr             { CompoundAssign $1 Div $3 }
           | id '%=' AddExpr             { CompoundAssign $1 Mod $3 }



-- Expressões lógicas (OR)
LogicalExpr : LogicalExpr '||' AndExpr       { BinOp $1 Or $3 }
           | AndExpr                            { $1 }



-- Expressões AND
AndExpr : AndExpr '&&' CompareExpr          { BinOp $1 And $3 }
        | CompareExpr                       { $1 }



-- Expressões de comparação
CompareExpr : CompareExpr '==' AddExpr      { BinOp $1 Eq $3 }
            | CompareExpr '!=' AddExpr      { BinOp $1 Neq $3 }
            | CompareExpr '<' AddExpr       { BinOp $1 Lt $3 }
            | CompareExpr '<=' AddExpr      { BinOp $1 Lte $3 }
            | CompareExpr '>' AddExpr       { BinOp $1 Gt $3 }
            | CompareExpr '>=' AddExpr      { BinOp $1 Gte $3 }
            | AddExpr                       { $1 }



-- Expressões de adição e subtração
AddExpr : AddExpr '+' AddExpr                  { BinOp $1 Add $3 }
        | AddExpr '-' AddExpr                  { BinOp $1 Sub $3 }
        | Term                                   { $1 }



-- Expressões de multiplicação, divisão e módulo
Term : Term '*' Term                   { BinOp $1 Mul $3 }
     | Term '/' Term                   { BinOp $1 Div $3 }
     | Term '%' Term                   { BinOp $1 Mod $3 }
     | UnaryExpr                           { $1 }



-- Expressões unárias (prefixo)
UnaryExpr : '!' UnaryExpr                   { UnOp Not $2 }             -- ! doesn't make too much sense
          | '-' UnaryExpr                   { UnOp Neg $2 }
          | '++' PostfixExpr                { UnOp PreInc $2 }
          | '--' PostfixExpr                { UnOp PreDec $2 }
          | PostfixExpr                     { $1 }



-- Expressões pós-fixadas (postfix)
PostfixExpr : PostfixExpr '++'              { PostOp $1 PostInc }
            | PostfixExpr '--'              { PostOp $1 PostDec }
            | PostfixExpr '[' Expr ']'      { ArrayAccess $1 $3 }
            | PostfixExpr '.' id            { MemberAccess $1 $3 }
            | Primary                       { $1 }



-- Expressões primárias (valores literais, identificadores, etc.)
Primary : int                               { IntLit $1 }
        | double                            { DoubleLit $1 }
        | string                            { StringLit $1 }
        | bool                              { BoolLit $1 }
        | id                                { Id $1 }
        | '(' Expr ')'                      { $2 }
        | FunctionCall                      { $1 }
        | readln '(' ')'                    { ReadLn }
        | print '(' Expr ')'                { PrintStmt $3 }



-- Chamada de função
FunctionCall : id '(' ExprList ')'  { Call $1 $3 }


-- Usa como input uma lista de Tokens e retorna uma AST se não houver erro. Se houver erro retorna uma string de mensagem contendo o erro.
{
parseError :: [Token] -> Either String a
parseError toks = Left $ "Parse error at token(s): " ++ show toks

parse :: [Token] -> Either String AST
parse = parser
}
