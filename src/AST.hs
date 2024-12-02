-- Exporta as estruturas de dados para o Parser.y
module AST
    ( AST(..)
    , Program(..)
    , Function(..)
    , Type(..)
    , Param(..)
    , Cmd(..)
    , Declare(..)
    , Assignment(..)
    , If(..)
    , Return(..)
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
        [ReturnStmt (Id "message")]
-}  



data Function = Function String [Param] Type [Cmd]
              | Main [Param] [Cmd]
              deriving (Show, Eq)

-- Tipos de variáveis em Kotlin. UnitType é void ( Se quiser trocar o nome fica a vontade, mas tem q mudar em mais lugares )
data Type 
    = IntType
    | BooleanType
    | UnitType
    deriving (Show, Eq)



-- Representação de um Parâmetro. Ex: [Param "name" StringType, Param "age" IntType]
data Param = Param String Type
    deriving (Show, Eq)

-- Tipos de Statements (Declarações)
data Cmd 
    = DeclareCmd Declare              -- Declaração de variavel (var x = 5)
    | AssignCmd Assignment              -- Atribuição de valor   
    | IfCmd If        -- Declaração If com condição, bloco then e bloco else opcional  
    | WhileCmd Expr [Cmd]            -- Loop While com condição e corpo     
    | ReturnCmd Return                  -- Declaração de retorno (return x)  
    | ExprCmd Expr                    -- Declaração de expressão (x = 5)
    deriving (Show, Eq)

-- Exemplo de representação: ValDecl "pi" DoubleType (DoubleLit 3.14159) 
data Declare 
    = ValDecl String Type Expr  -- Ex: val pi: Double = 3.14159  
    | VarDecl String Type Expr  -- Ex: var counter: Int = 0  
    | VarDeclEmpty String Type  -- Ex: var name: String  
    deriving (Show, Eq)


data Assignment
    = Assign String Expr        -- Atribuição simples
    deriving (Show, Eq)   

data If
    = If Expr [Cmd] [Cmd]       -- If Then Else
    deriving (Show, Eq)

data Return = ReturnEmpty       -- Ex: Return;
            deriving (Show, Eq)

-- Tipos de Expressão
data Expr 
    = IntLit Int                    -- Literal inteiro
    | BoolLit Bool                  -- Literal booleano
    | StringLit String              -- Literal string
    | Id String                     -- Referência a variável
    | BinOp Expr BinOperator Expr   -- Operações binárias
    | UnOp UnOperator Expr          -- Operações unárias prefixas
    | ReadLn                        -- Ler linha da entrada padrão
    | Print Expr                    -- Print da expressão fornecida
    deriving (Show, Eq)

-- Tipos de operadores binários
data BinOperator 
    = Add | Sub | Mul | Div | Mod           -- Operadores aritméticos (+, -, *, /, %)
    | And | Or                              -- Operadores lógicos (&&, ||)
    | Eq | Neq | Lt | Lte | Gt | Gte        -- Operadores de comparação (==, !=, <, <=, >, >=)
    | AssignOp                              -- Atribuição simples (=)
    deriving (Show, Eq)

-- Tipos de operadores unários
data UnOperator 
    = Neg                    -- Negação numérica (-)
    | Not                    -- Negação lógica (!)
    deriving (Show, Eq)

-- Printar a AST (Abstract Syntatic Tree)
prettyPrint :: AST -> String
prettyPrint (Program fns) = "Program:\n" ++ concatMap printFunction fns
  where
    printFunction (Function name params retType cmds) =
        "Function " ++ name ++ ":\n" ++
        "  Parameters: " ++ show params ++ "\n" ++
        "  Return Type: " ++ show retType ++ "\n" ++
        "  Commands: " ++ show cmds ++ "\n"




