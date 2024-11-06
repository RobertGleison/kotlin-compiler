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

Prog : ProgList              { Program $1 }   -- A program consists of a list of functions
  

-- List of functions that make up the program
ProgList : Function ProgList      { $1 : $2 }  -- One or more functions      
         |                        { [] }       -- Empty program is valid


-- Function declarations with two forms:
-- 1. Function with explicit return type
-- 2. Function void (implicitly returns Unit)
Function : fun id '(' ParamList ')' ':' Type '{' Declares Stmts '}' { Function $2 $4 $7 $9 $10 }
         | fun id '(' ParamList ')' '{' Declares Stmts '}' { Function $2 $4 UnitType $7 $8 }


-- Basic types supported in the language
Type : Int     { IntType }
     | Double   { DoubleType }
     | Boolean  { BooleanType }
     | String   { StringType }
     | Float    { FloatType }


-- Parameter list in function declarations
ParamList : Param ',' ParamList   { $1 : $3 }   -- Multiple parameters separated by commas
          | Param                  { [$1] }     -- Single parameter
          |                       { [] }        -- Empty parameter list


-- Individual parameter declaration with type annotation
Param : id ':' Type        { Param $1 $3 }


-- List of declarations (variables and values)
Declares : Declare Declares      { $1 : $2 }   -- Multiple declarations
        |                       { [] }         -- Empty declarations block


-- Three forms of declarations:
-- 1. Immutable value (val) with initialization
-- 2. Mutable variable (var) with initialization
-- 3. Mutable variable (var) without initialization
Declare : val id ':' Type '=' Expr ';'  { ValDecl $2 $4 $6 }    -- val x: Int = 5;
        | var id ':' Type '=' Expr ';'  { VarDecl $2 $4 $6 }    -- var x: Int = 5;
        | var id ':' Type ';'           { VarDeclEmpty $2 $4 }   -- var x: Int;


-- List of statements in a function body
Stmts : Stmts Stmt                 { $2 : $1 }
      |                            { [] }


-- Different types of statements supported:
Stmt : IfStmt                      { $1 }
     | OtherStmt                   { $1 }


IfStmt : if '(' Expr ')' '{' Stmts '}' else '{' Stmts '}'  { IfStmt $3 $6 $10 }
       | if '(' Expr ')' '{' Stmts '}'                     { IfStmt $3 $6 [] }


OtherStmt : Expr ';'                                       { ExprStmt $1 }
          | return Expr ';'                                { ReturnStmt $2 }
          | while '(' Expr ')' '{' Stmts '}'               { WhileStmt $3 $6 }
          | for '(' id in Expr ')' '{' Stmts '}'           { ForStmt $3 $5 $8 }


ExprList : Expr ',' ExprList        { $1 : $3 }
         | Expr                     { [$1] }
         |                         { [] }


Expr : AssignExpr                { $1 }
     | LogicalExpr              { $1 }              { $1 }


AssignExpr : id '=' LogicalExpr              { Assign $1 $3 }
           | id '+=' LogicalExpr             { CompoundAssign $1 Add $3 }
           | id '-=' LogicalExpr             { CompoundAssign $1 Sub $3 }
           | id '*=' LogicalExpr             { CompoundAssign $1 Mul $3 }
           | id '/=' LogicalExpr             { CompoundAssign $1 Div $3 }
           | id '%=' LogicalExpr             { CompoundAssign $1 Mod $3 }
           | LogicalExpr                     { $1 }


LogicalExpr : LogicalExpr '||' AndExpr       { BinOp $1 Or $3 }
           | AndExpr    

AndExpr : AndExpr '&&' CompareExpr          { BinOp $1 And $3 }
        | CompareExpr                       { $1 }

CompareExpr : CompareExpr '==' AddExpr      { BinOp $1 Eq $3 }
            | CompareExpr '!=' AddExpr      { BinOp $1 Neq $3 }
            | CompareExpr '<' AddExpr       { BinOp $1 Lt $3 }
            | CompareExpr '<=' AddExpr      { BinOp $1 Lte $3 }
            | CompareExpr '>' AddExpr       { BinOp $1 Gt $3 }
            | CompareExpr '>=' AddExpr      { BinOp $1 Gte $3 }
            | AddExpr                       { $1 }

AddExpr : AddExpr '+' Term                  { BinOp $1 Add $3 }
        | AddExpr '-' Term                  { BinOp $1 Sub $3 }
        | Term           

Term : Term '*' UnaryExpr                   { BinOp $1 Mul $3 }
     | Term '/' UnaryExpr                   { BinOp $1 Div $3 }
     | Term '%' UnaryExpr                   { BinOp $1 Mod $3 }
     | UnaryExpr                           { $1 }


UnaryExpr : '!' UnaryExpr                   { UnOp Not $2 }
          | '-' UnaryExpr                   { UnOp Neg $2 }
          | '++' PostfixExpr                { UnOp PreInc $2 }
          | '--' PostfixExpr                { UnOp PreDec $2 }
          | PostfixExpr                     { $1 }

PostfixExpr : PostfixExpr '++'              { PostOp $1 PostInc }
            | PostfixExpr '--'              { PostOp $1 PostDec }
            | PostfixExpr '[' Expr ']'      { ArrayAccess $1 $3 }
            | PostfixExpr '.' id            { MemberAccess $1 $3 }
            | Primary                       { $1 }

Primary : int                               { IntLit $1 }
        | double                            { DoubleLit $1 }
        | string                            { StringLit $1 }
        | bool                              { BoolLit $1 }
        | id                                { Id $1 }
        | '(' Expr ')'                      { $2 }
        | FunctionCall                      { $1 }


Factor : int                   { IntLit $1 }
       | double                { DoubleLit $1 }
       | string                { StringLit $1 }
       | bool                  { BoolLit $1 }
       | id                    { Id $1 }
       | '(' Expr ')'          { $2 }
       | FunctionCall          { $1 }


FunctionCall : id '(' ExprList ')'  { Call $1 $3 }



data Stmt 
    = ExprStmt Expr                    -- Expression statement (e.g., x = 5;)
    | ReturnStmt Expr                  -- Return statement (e.g., return x;)
    | IfStmt Expr [Stmt] [Stmt]        -- If statement with condition, then-block, and optional else-block
    | WhileStmt Expr [Stmt]            -- While loop with condition and body
    | ForStmt String Expr [Stmt]       -- For loop with iterator variable, collection expression, and body
    deriving (Show, Eq)


-- Binary operators - expanded to include all operators from tokens
data BinOperator 
    = Add | Sub | Mul | Div | Mod           -- Arithmetic (+, -, *, /, %)
    | And | Or                              -- Logical (&&, ||)
    | Eq | Neq | Lt | Lte | Gt | Gte        -- Comparison (==, !=, <, <=, >, >=)
    | Assign                                -- Simple assignment (=)
    | PlusAssign | MinusAssign             -- Compound assignment (+=, -=)
    | TimesAssign | DivAssign | ModAssign   -- Compound assignment (*=, /=, %=)
    | Dot                                   -- Member access (.)
    deriving (Show, Eq)


-- Unary operators - expanded to include all from tokens
data UnOperator 
    = Neg                    -- Numeric negation (-)
    | Not                    -- Logical negation (!)
    | PreInc | PreDec        -- Prefix increment/decrement (++x, --x)
    | PostInc | PostDec      -- Postfix increment/decrement (x++, x--)
    deriving (Show, Eq)


-- Expression type - expanded to handle all operators
data Expr 
    = IntLit Int                    -- Integer literal
    | DoubleLit Double              -- Double literal
    | BoolLit Bool                  -- Boolean literal
    | StringLit String              -- String literal
    | Id String                     -- Variable reference
    | BinOp Expr BinOperator Expr   -- Binary operations
    | UnOp UnOperator Expr          -- Prefix unary operations
    | PostOp Expr UnOperator        -- Postfix unary operations (for ++ and --)
    | Assign String Expr            -- Simple assignment
    | CompoundAssign String BinOperator Expr  -- Compound assignments (+=, -=, etc.)
    | Call String [Expr]            -- Function call
    | ArrayAccess Expr Expr         -- Array indexing with []
    | MemberAccess Expr String      -- Member access with dot (.)
    deriving (Show, Eq)