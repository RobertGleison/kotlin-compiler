{
module Parser where

import Data.Maybe
import qualified Data.List as L
}

%name parseExpr
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
  '+'             { PLUS }
  '-'             { MINUS }
  '*'             { TIMES }
  '/'             { DIVIDE }
  '%'             { MOD }
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
  
  -- Types
  Int             { INT }
  Float           { FLOAT }
  Double          { DOUBLE }
  Boolean         { BOOLEAN }
  String          { STRING }

%left '||'
%left '&&'
%nonassoc '==' '!=' '<' '<=' '>' '>='
%left '+' '-'
%left '*' '/' '%'
%right '!'
%left '.'

%%

Program : PackageDecl ImportDecls TopLevelDecls    { Program $1 $2 $3 }

PackageDecl : package QualifiedId ';'             { PackageDecl $2 }
            |                                      { NoPackage }

ImportDecls : ImportDecls ImportDecl              { $2 : $1 }
           |                                      { [] }

ImportDecl : import QualifiedId ';'               { ImportDecl $2 }

QualifiedId : id                                  { [$1] }
            | QualifiedId '.' id                  { $3 : $1 }

TopLevelDecls : TopLevelDecls TopLevelDecl        { $2 : $1 }
              |                                   { [] }

TopLevelDecl : ClassDecl                          { ClassD $1 }
             | InterfaceDecl                      { InterfaceD $1 }
             | ObjectDecl                         { ObjectD $1 }
             | FunDecl                            { FunD $1 }
             | PropertyDecl                       { PropertyD $1 }

ClassDecl : class id ParamList '{' ClassBody '}'  { ClassDecl $2 $3 $5 }

InterfaceDecl : interface id '{' InterfaceBody '}' { InterfaceDecl $2 $4 }

ObjectDecl : object id '{' ClassBody '}'          { ObjectDecl $2 $4 }


ClassBody : ClassBody ClassMember                 { $2 : $1 }
         |                                       { [] }

ClassMember : FunDecl                            { MethodMember $1 }
            | PropertyDecl                       { PropertyMember $1 }

InterfaceBody : InterfaceBody InterfaceMember    { $2 : $1 }
              |                                  { [] }

InterfaceMember : FunDecl                        { AbstractMethod $1 }
                | PropertyDecl 
                
ParamList : '(' Params ')'                        { $2 }
         | '(' ')'                                { [] }

Params : Param                                    { [$1] }
       | Params ',' Param                         { $3 : $1 }

Param : id ':' Type                               { Param $1 $3 }

Type : Int                                        { IntType }
     | Float                                      { FloatType }
     | Double                                     { DoubleType }
     | Boolean                                    { BooleanType }
     | String                                     { StringType }
     | id                                         { CustomType $1 }

FunDecl : fun id ParamList ':' Type Block         { FunDecl $2 $3 $5 $6 }
        | fun id ParamList Block                  { FunDecl $2 $3 VoidType $4 }

PropertyDecl : val id ':' Type '=' Expr ';'       { ValDecl $2 $4 $6 }
             | var id ':' Type '=' Expr ';'       { VarDecl $2 $4 $6 }
             | val id '=' Expr ';'                { ValDeclInferred $2 $4 }
             | var id '=' Expr ';'                { VarDeclInferred $2 $4 }

Block : '{' Stmts '}'                             { Block $2 }

Stmts : Stmts Stmt                               { $2 : $1 }
      |                                          { [] }

Stmt : PropertyDecl                              { DeclStmt $1 }
     | Expr '=' Expr ';'                         { AssignStmt $1 $3 }
     | if '(' Expr ')' Block                     { IfStmt $3 $5 Nothing }
     | if '(' Expr ')' Block else Block          { IfStmt $3 $5 (Just $7) }
     | while '(' Expr ')' Block                  { WhileStmt $3 $5 }
     | for '(' id in Expr ')' Block              { ForStmt $3 $5 $7 }
     | return Expr ';'                           { ReturnStmt $2 }
     | return ';'                                { ReturnVoidStmt }
     | Block                                     { BlockStmt $1 }
     | Expr ';'                                  { ExprStmt $1 }

Expr : int                                       { IntLit $1 }
     | double                                    { DoubleLit $1 }
     | string                                    { StringLit $1 }
     | bool                                      { BoolLit $1 }
     | id                                        { Var $1 }
     | Expr '.' id                              { FieldAccess $1 $3 }
     | Expr '(' Args ')'                        { FunCall $1 $3 }
     | '(' Expr ')'                             { $2 }
     | Expr '+' Expr                            { BinOp Add $1 $3 }
     | Expr '-' Expr                            { BinOp Sub $1 $3 }
     | Expr '*' Expr                            { BinOp Mul $1 $3 }
     | Expr '/' Expr                            { BinOp Div $1 $3 }
     | Expr '%' Expr                            { BinOp Mod $1 $3 }
     | Expr '==' Expr                           { BinOp Eq $1 $3 }
     | Expr '!=' Expr                           { BinOp Neq $1 $3 }
     | Expr '<' Expr                            { BinOp Lt $1 $3 }
     | Expr '<=' Expr                           { BinOp Lte $1 $3 }
     | Expr '>' Expr                            { BinOp Gt $1 $3 }
     | Expr '>=' Expr                           { BinOp Gte $1 $3 }
     | Expr '&&' Expr                           { BinOp And $1 $3 }
     | Expr '||' Expr                           { BinOp Or $1 $3 }
     | '!' Expr                                 { UnOp Not $2 }
     | '-' Expr                                 { UnOp Neg $2 }

Args : Expr                                      { [$1] }
     | Args ',' Expr                            { $3 : $1 }
     |                                          { [] }

{
parseError :: [Token] -> a
parseError tokens = error $ "Parse error on tokens: " ++ show tokens

-- AST Data Types
data Program = Program PackageDecl [ImportDecl] [TopLevelDecl]
  deriving (Show, Eq)

data PackageDecl = PackageDecl [String] | NoPackage
  deriving (Show, Eq)

data ImportDecl = ImportDecl [String]
  deriving (Show, Eq)

data TopLevelDecl
  = ClassD ClassDecl
  | InterfaceD InterfaceDecl
  | ObjectD ObjectDecl
  | FunD FunDecl
  | PropertyD PropertyDecl
  deriving (Show, Eq)

data ClassDecl = ClassDecl String [Param] [ClassMember]
  deriving (Show, Eq)

data InterfaceDecl = InterfaceDecl String [InterfaceMember]
  deriving (Show, Eq)

data ObjectDecl = ObjectDecl String [ClassMember]
  deriving (Show, Eq)

data ClassMember
  = MethodMember FunDecl
  | PropertyMember PropertyDecl
  deriving (Show, Eq)

data InterfaceMember
  = AbstractMethod FunDecl
  | AbstractProperty PropertyDecl
  deriving (Show, Eq)

data Param = Param String Type
  deriving (Show, Eq)

data Type
  = IntType
  | FloatType
  | DoubleType
  | BooleanType
  | StringType
  | CustomType String
  | VoidType
  deriving (Show, Eq)

data FunDecl = FunDecl String [Param] Type Block
  deriving (Show, Eq)

data PropertyDecl
  = ValDecl String Type Expr
  | VarDecl String Type Expr
  | ValDeclInferred String Expr
  | VarDeclInferred String Expr
  deriving (Show, Eq)

data Block = Block [Stmt]
  deriving (Show, Eq)

data Stmt
  = DeclStmt PropertyDecl
  | AssignStmt Expr Expr
  | IfStmt Expr Block (Maybe Block)
  | WhileStmt Expr Block
  | ForStmt String Expr Block
  | ReturnStmt Expr
  | ReturnVoidStmt
  | BlockStmt Block
  | ExprStmt Expr
  deriving (Show, Eq)

data Expr
  = IntLit Int
  | DoubleLit Double
  | StringLit String
  | BoolLit Bool
  | Var String
  | FieldAccess Expr String
  | FunCall Expr [Expr]
  | BinOp BinOp Expr Expr
  | UnOp UnOp Expr
  deriving (Show, Eq)

data BinOp
  = Add | Sub | Mul | Div | Mod
  | Eq | Neq | Lt | Lte | Gt | Gte
  | And | Or
  deriving (Show, Eq)

data UnOp = Not | Neg
  deriving (Show, Eq)
}