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
  char            { CHAR_LIT $$ }
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
  main            { MAIN }
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
  Double          { DOUBLE }
  Boolean         { BOOLEAN }
  Char            { CHAR }
  String          { STRING }



-- Precedência de operadores. Uma multiplicação por exemplo tem precedência sobre uma soma, e por aí vai.
%right '=' '+=' '-=' '*=' '/=' '%='    -- lowest precedence
%left '||'
%left '&&'
%nonassoc '==' '!='                    -- separate equality
%nonassoc '<' '<=' '>' '>='            -- separate comparison
%left '+' '-'                          -- additive
%left '*' '/' '%'                      -- multiplicative
%left NEG
%left '.' '++' '--'                    -- highest precedence (postfix)
%left EXPR
%nonassoc RETURN

%%


-- :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: INÍCIO DA GRAMÁTICA :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


Prog : FunctionList                                                     { Program $1 }   -- Um programa consiste em uma lista de funções


FunctionList : Function FunctionList                                { $1 : $2 }  -- Uma ou mais funções
             |                                                      { [] }       -- Programa vazio é válido

--    Declarações de função com três formas:
--        - Main: fun main(): { return }
--        - Com tipo de retorno: fun soma(x: Int): Int { return x + 1 }
--        - Sem tipo retorno: fun hello(name: String) { println(name) }
Function : fun main '(' ParamList ')' '{' Cmds '}'                  { Main $4 $7 } 
         | fun id '(' ParamList ')' ':' Type '{' Cmds '}'           { Function $2 $4 $7 $9 } 
         | fun id '(' ParamList ')' '{' Cmds '}'                    { Function $2 $4 UnitType $7 } 

-- Tipos básicos suportados na linguagem
Type : Int                                                          { IntType }
     | Double                                                       { DoubleType }
     | Boolean                                                      { BooleanType }
     | Char                                                         { CharType }
     | String                                                       { StringType }

-- Lista de parâmetros em declarações de função. Ex: [Param "altura" DoubleType, Param "largura" DoubleType, Param "profundidade" DoubleType]
ParamList : Param ',' ParamList                                     { $1 : $3 }   -- Múltiplos parâmetros separados por vírgula
          | Param                                                   { [$1] }      -- Parâmetro único
          |                                                         { [] }        -- Lista de parâmetros vazia

-- Declaração individual de parâmetro com anotação de tipo. Ex: [Idade, IntType]
Param : id ':' Type                                                 { Param $1 $3 }

-- Lista de comandos
Cmds : Cmd Cmds                                                     { $1 : $2 }
     |                                                              { [] }

-- Comandos individuais: Declarações de variaveis, 
Cmd  : Declare                                                      { DeclareCmd $1 }   -- Declarações de variáveis
     | Declare ';'                                                  { DeclareCmd $1 }
     | Assign                                                       { AssignCmd $1 }    -- Atribuição de valores a variaveis
     | Assign ';'                                                   { AssignCmd $1 }
     | If                                                           { IfCmd $1 }        -- Comandos de if, else if e else
     | WhileCmd                                                     { $1 }              -- While loops
     | Return                                                       { ReturnCmd $1 }    -- Retornos
     | Return ';'                                                   { ReturnCmd $1 }
     | Expr        %prec EXPR                                                 { ExprCmd $1 }      -- Expressões
     | Expr ';'    %prec EXPR                                                 { ExprCmd $1 }

Declare : val id ':' Type '=' Expr                                  { ValDecl $2 $4 $6 }        -- val x : Int = 5
        | val id '=' Expr                                           { ValDecl $2 UnitType $4 }  -- val x = 5
        | var id ':' Type '=' Expr                                  { VarDecl $2 $4 $6 }        -- var x : Int = 5
        | var id '=' Expr                                           { VarDecl $2 UnitType $4 }  -- var x = 5
        | var id ':' Type                                           { VarDeclEmpty $2 $4 }      -- var x: Int

Assign  : id '=' Expr                                               { Assign $1 $3 }            -- x = 5
        | id '+=' Expr                                              { CompoundAssign $1 Add $3 }-- x += 5
        | id '-=' Expr                                              { CompoundAssign $1 Sub $3 }-- x -= 5
        | id '*=' Expr                                              { CompoundAssign $1 Mul $3 }-- x *= 5
        | id '/=' Expr                                              { CompoundAssign $1 Div $3 }-- x /= 5
        | id '%=' Expr                                              { CompoundAssign $1 Mod $3 }-- x %= 5

If      : if '(' BoolExpr ')' '{' Cmds '}'                          { If $3 $6 }               -- if (x == 5) { x = 4 }
        | else if '(' BoolExpr ')' '{' Cmds '}'                     { ElseIf $4 $7 }           -- else if ( x == 3 ) { x = 2 }
        | else '{' Cmds '}'                                         { Else $3 }                -- else { x = 1 }

WhileCmd : while '(' BoolExpr ')' '{' Cmds '}'                      { WhileCmd $3 $6 }          -- while (x <= 5) { x++ }

Return  : return Expr %prec RETURN                                              { Return $2 }               -- return x
        | return %prec RETURN                                                    { ReturnEmpty }             -- return       --! FLAG


Expr    : ArithmExpr                            { $1 }                  -- Expressões aritméticas
        | BoolExpr                              { $1 }                  -- Expressões booleanas
        | Print                                 { $1 }                  -- Função print
        | Readln                                { $1 }                  -- Função readln
        | FunCall                               { $1 }                  -- Chamadas de funções
        | Term                                  { $1 }                  -- Termos
        | Access                                { $1 }                  -- Acessos
        | '(' Expr ')'                          { $2 }                  -- Expressões entre ( )     --! FLAG
        

ArithmExpr  : Expr '+' Expr                     { BinOp $1 Add $3 }     -- Soma
            | Expr '-' Expr                     { BinOp $1 Sub $3 }     -- Subtração
            | Expr '*' Expr                     { BinOp $1 Mul $3 }     -- Multiplicação
            | Expr '/' Expr                     { BinOp $1 Div $3 }     -- Divisão
            | Expr '%' Expr                     { BinOp $1 Mod $3 }     -- Módulo
            | '-' Expr %prec NEG             { UnOp Neg $2 }         -- Expressão Negativa       --! FLAG
            | '++' Expr                          { UnOp PreInc $2 }      -- Pré-Incrementação
            | '--' Expr                         { UnOp PreDec $2 }      -- Pré-Decrementação
            | Expr '++'                         { PostOp $1 PostInc }   -- Pós-Incrementação        --! FLAG
            | Expr '--'                         { PostOp $1 PostDec }   -- Pós-Decrementação        --! FLAG

BoolExpr    : Expr '==' Expr                    { BinOp $1 Eq $3 }      -- Equalidade
            | Expr '!=' Expr                    { BinOp $1 Neq $3 }     -- Inequalidade
            | Expr '<' Expr                     { BinOp $1 Lt $3 }      -- Menor que
            | Expr '<=' Expr                    { BinOp $1 Lte $3 }     -- Menor ou igual a
            | Expr '>' Expr                     { BinOp $1 Gt $3 }      -- Maior que
            | Expr '>=' Expr                    { BinOp $1 Gte $3 }     -- Maior ou igual a
            | Expr '&&' Expr                    { BinOp $1 And $3 }     -- Conjunção
            | Expr '||' Expr                    { BinOp $1 Or $3 }      -- Disjução
            | '!' Expr %prec NEG             { UnOp Not $2 }         -- Negação                  --! FLAG

Print   : print '(' Expr ')'                    { Print $3 }            -- Função print

Readln  : readln '(' ')'                        { ReadLn }              -- Função readln

FunCall : id '(' ArgList ')'                    { Call $1 $3 }          -- Chamada de Função        --! FLAG

ArgList : Expr ',' ArgList                      { $1 : $3 }             -- Lista de argumentos
        | Expr                                  { [$1] }
        |                                       { [] }
        
Term    : int                                   { IntLit $1 }           -- Inteiros literais
        | double                                { DoubleLit $1 }        -- Doubles literais
        | char                                  { CharLit $1 }          -- Caracteres literais
        | string                                { StringLit $1 }        -- String literais
        | bool                                  { BoolLit $1 }          -- Booleanos literais
        | id                                    { Id $1 }               -- Variaveis                --! FLAG

Access  : Expr '[' Expr ']'                     { ArrayAccess $1 $3 }      -- Acesso a Arrays       --! FLAG
        | Expr '.' id '(' ArgList ')'           { MethodAccess $1 $3 $5 }  -- Acesso a métodos      --! FLAG
        | Expr '.' id                           { MemberAccess $1 $3 }     -- Acesso a membros      --! FLAG

-- Usa como input uma lista de Tokens e retorna uma AST se não houver erro. Se houver erro retorna uma string de mensagem contendo o erro.
{
parseError :: [Token] -> Either String a
parseError toks = Left $ "Parse error at token(s): " ++ show toks

parse :: [Token] -> Either String AST
parse = parser
}
