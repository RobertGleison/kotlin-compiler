-- Exporta as estruturas de dados para o Parser.y
module AST
    ( AST(..)
    , Program(..)
    , Function(..)
    , Type(..)
    , Param(..)
    -- , Declare(..)
    , Stmt(..)
    , Expr(..)
    , BinOperator(..)
    , UnOperator(..)
    , prettyPrint
    ) where



-- Alias AST para chamar o data Program
type AST = Program



-- Ponto de inicio das estruturas de dados. O input Programa é um array de Functions
data Program = Program [Function]
    deriving (Show, Eq)



-- Representação de uma Função. Ex: 
{- 
    fun greet(name: String): String {
        val message: String = "Hello, " + name
        return message
    }

    Function 
        "greet"                                -- name
        [Param "name" StringType]              -- parameters
        StringType                             -- return type
        --!!!!!! NOT ANYMORE
        [ValDecl                               -- declarations
            "message" 
            StringType 
            (BinOp 
                (StringLit "Hello, ") 
                Add 
                (Id "name")
            )
        ]
        --!!!!!! UNTIL HERE
        [ReturnStmt (Id "message")]
-}  
data Function = Function String [Param] Type [Stmt]
    deriving (Show, Eq)



-- Tipos de variáveis em Kotlin. UnitType é void ( Se quiser trocar o nome fica a vontade, mas tem q mudar em mais lugares )
data Type 
    = IntType 
    | DoubleType 
    | BooleanType 
    | StringType 
    | FloatType 
    | UnitType
    deriving (Show, Eq)



-- Representação de um Parâmetro. Ex: [Param "name" StringType, Param "age" IntType]
data Param = Param String Type
    deriving (Show, Eq)



-- Exemplo de representação: ValDecl "pi" DoubleType (DoubleLit 3.14159) 
-- data Declare 
--     = ValDecl String Type Expr  -- Ex: val pi: Double = 3.14159  
--     | VarDecl String Type Expr  -- Ex: var counter: Int = 0  
--     | VarDeclEmpty String Type  -- Ex: var name: String  
--     deriving (Show, Eq)



-- Tipos de Statements (Declarações)
data Stmt 
    = ExprStmt Expr                    -- Declaração de expressão (x = 5;)
    | ReturnStmt Expr                  -- Declaração de retorno (return x;)
    | IfStmt Expr [Stmt] [Stmt]        -- Declaração If com condição, bloco then e bloco else opcional
    | WhileStmt Expr [Stmt]            -- Loop While com condição e corpo
    | ValDecl String Type Expr         -- Ex: val pi: Double = 3.14159  
    | VarDecl String Type Expr         -- Ex: var counter: Int = 0  
    | VarDeclEmpty String Type         -- Ex: var name: String  
    deriving (Show, Eq)



-- Tipos de operadores binários
data BinOperator 
    = Add | Sub | Mul | Div | Mod           -- Operadores aritméticos (+, -, *, /, %)
    | And | Or                              -- Operadores lógicos (&&, ||)
    | Eq | Neq | Lt | Lte | Gt | Gte        -- Operadores de comparação (==, !=, <, <=, >, >=)
    | AssignOp                              -- Atribuição simples (=)
    | PlusAssign | MinusAssign              -- Atribuição composta (+=, -=)
    | TimesAssign | DivAssign | ModAssign   -- Atribuição composta (*=, /=, %=)
    | Dot                                   -- Acesso a membro (.)
    deriving (Show, Eq)



-- Tipos de operadores unários
data UnOperator 
    = Neg                    -- Negação numérica (-)
    | Not                    -- Negação lógica (!)
    | PreInc | PreDec        -- Incremento/decremento prefixo (++x, --x)
    | PostInc | PostDec      -- Incremento/decremento pósfixo (x++, x--)
    deriving (Show, Eq)



-- Tipos de Expressão
data Expr 
    = IntLit Int                    -- Literal inteiro
    | DoubleLit Double              -- Literal double
    | BoolLit Bool                  -- Literal booleano
    | StringLit String              -- Literal string
    | Id String                     -- Referência a variável
    | BinOp Expr BinOperator Expr   -- Operações binárias
    | UnOp UnOperator Expr          -- Operações unárias prefixas
    | PostOp Expr UnOperator        -- Operações unárias pósfixas (para ++ e --)
    | Assignment String Expr        -- Atribuição simples
    | CompoundAssign String BinOperator Expr  -- Atribuições compostas (+=, -=, etc.)
    | Call String [Expr]            -- Chamada de função
    | ArrayAccess Expr Expr         -- Acesso a array com []
    | MemberAccess Expr String      -- Acesso a membro com ponto (.)
    | ReadLn                        -- Ler linha da entrada padrão
    | PrintStmt Expr                -- Print da expressão fornecida
    deriving (Show, Eq)



-- Printar a AST (Abstract Syntatic Tree)
prettyPrint :: AST -> String
prettyPrint (Program fns) = "Program:\n" ++ concatMap printFunction fns
  where
    printFunction (Function name params retType stmts) =
        "Function " ++ name ++ ":\n" ++
        "  Parameters: " ++ show params ++ "\n" ++
        "  Return Type: " ++ show retType ++ "\n" ++
        -- "  Declarations: " ++ show decls ++ "\n" ++
        "  Statements: " ++ show stmts ++ "\n"




