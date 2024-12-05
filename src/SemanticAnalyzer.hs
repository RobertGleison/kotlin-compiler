module SemanticAnalyzer where

import qualified Data.Map as Map
import AST (AST(..), Program(..), Function(..), Expr(..), Cmd(..), Declare(..), 
            Assignment(..), If(..), Param(..), Type(..), BinOperator(..), UnOperator(..))



-- Maps the name of variables and the type of them.
-- Ex:  "x" -> IntType, "y" -> BooleanType
type TypeEnv = Map.Map String Type



-- Maps the name of a function, the return type and the parameters.
-- Ex: "calculateArea" -> (IntType, [IntType, IntType])
type FunEnv = Map.Map String (Type, [Type]) -- (return type, parameter types)



-- Main type checking function for the whole program
checkProgram :: AST -> Either String Type                       -- Returns or a String with error, or a type for successfullly checking
checkProgram (Program functions) = do
    let funEnv = buildFunctionEnv functions                     -- create a Map of functions
    checkMain functions                                         -- verify if the main function exist and have only one
    mapM_ (checkFunction funEnv) functions                      -- takes each function from list and applies check function to each on using Map
    return UnitType                                             -- Return UnitType (void) if all checks passed



-- Build environment containing function signatures
-- Ex: (fun main(x: Int, y: Boolean) { ... }) returns ("main" -> (UnitType, [IntType, BooleanType]))
buildFunctionEnv :: [Function] -> FunEnv                        -- takes a list of functions and return a Map of functions 
buildFunctionEnv functions = 
    Map.fromList [(name, (retType, paramTypes))                 -- Create a Map of functions
                 | Main params _ <- functions                   -- Extract the params of each function
                 , let name = "main"                            -- set the name of function to "main"
                 , let retType = UnitType                       -- set the return to UnitType (void)
                 , let paramTypes = map getParamType params     -- Maps the parameters and their types
                 ]
    where
        getParamType (Param _ t) = t                            -- Helper function that ignores the param name an returns just the type



-- Check that main function exists and has correct signature
checkMain :: [Function] -> Either String ()                     -- Takes a list of function and return or a string of error, or nothing if the checks passed
checkMain functions = 
    case filter isMain functions of                             -- Get all main functions
        [] -> Left "No main function defined"                   -- Returns an error if no main function detected
        [_] -> Right ()                                         -- Sucess if only one main function detected
        _ -> Left "Multiple main functions defined"             -- Returns an error if multiple main functions were detected
    where isMain (Main _ _) = True                              -- Returns true if its a main function


-- Type check a single function
-- Ex: fun main(x: Int, y: Boolean) { body... } return  "x" -> IntType, "y" -> BooleanType
checkFunction :: FunEnv -> Function -> Either String Type                           -- Gets a map of functions, a map of variables, a function and return a Error String or a Type
checkFunction funEnv (Main params body) = do                                        -- Extracts from function params and body
    let env = Map.fromList [(name, paramType) | Param name paramType <- params]     -- Puts into Map the name and type of each parameter
    checkCmds funEnv env body                                                       -- Check all commands of the function body



-- Check a list of commands
--Ex: fun main() {
--       val x: Int = 5      // cmd 1
--       val y: Int = x + 1  // cmd 2
--       print(y)            // cmd 3
}
checkCmds :: FunEnv -> TypeEnv -> [Cmd] -> Either String Type           -- Gets the function map, the variable map, the cmd entry and return a error String or a Type  
checkCmds _ _ [] = Right UnitType                                       -- If no commands to check, return void
checkCmds funEnv env (cmd:cmds) = do                                    -- For each command
    (cmdType, env') <- checkCmd funEnv env cmd                          -- Calls checkCmd for command and return the command Type and possibly updated environment
    checkCmds funEnv env' cmds                                          -- Recursive call



-- Check individual commands
checkCmd :: FunEnv -> TypeEnv -> Cmd -> Either String (Type, TypeEnv)           -- Takes the function Map, the Variaboe Env, the Commando and return an Error String or a tuple with Typen and updated Variable Map
checkCmd funEnv env (DeclareCmd decl) = checkDeclare funEnv env decl            -- Check declaration. Ex: val x: Int = 5
checkCmd funEnv env (AssignCmd assign) = checkAssign funEnv env assign          -- Check assignments. Ex: x = 10
checkCmd funEnv env (IfCmd ifStmt) = checkIf funEnv env ifStmt                  -- Check If statements. Ex: if (x > 0) { ... } else { ... }
checkCmd funEnv env (WhileCmd cond body) = checkWhile funEnv env cond body      -- Check While commands. Ex: while (x < 10) { ... }
checkCmd funEnv env (ReturnCmd _) = Right (UnitType, env)                       -- Check return statement. Ex: return 
checkCmd funEnv env (ExprCmd expr) = do                                         -- Check expressions. Ex: print(x), readln()
    exprType <- checkExpr funEnv env expr
    return (exprType, env)



-- Check declarations for val
checkDeclare :: FunEnv -> TypeEnv -> Declare -> Either String (Type, TypeEnv)
checkDeclare funEnv env (ValDecl name declType expr) = do
    exprType <- checkExpr funEnv env expr                                       -- Get the Expression type. Ex: val x: Int = 5 -> Int
    if exprType == declType                                                     -- If matches
        then return (UnitType, Map.insert name declType env)                    -- Return void, add new variabble to env, insert into Map = X -> IntType to env
        else Left $ "Type mismatch in declaration of " ++ name                  -- Else return error


-- Check declarations for var
checkDeclare funEnv env (VarDecl name declType expr) = do                       
    exprType <- checkExpr funEnv env expr
    if exprType == declType
        then return (UnitType, Map.insert name declType env)
        else Left $ "Type mismatch in declaration of " ++ name


-- Chek declarations without initialization ( var )
checkDeclare _ env (VarDeclEmpty name declType) =
    return (UnitType, Map.insert name declType env)                             -- Insert variable name in environment with the type and return Null



-- Check assignments
checkAssign :: FunEnv -> TypeEnv -> Assignment -> Either String (Type, TypeEnv)
checkAssign funEnv env (Assign name expr) = do
    case Map.lookup name env of                                                 -- Checks is variable exist
        Nothing -> Left $ "Variable " ++ name ++ " not declared"                -- Error if variable not declared
        Just varType -> do
            exprType <- checkExpr funEnv env expr                               -- get the Type of expression and compare with variable type
            if exprType == varType                                              
                then return (UnitType, env)                                     -- Return void if matches 
                else Left $ "Type mismatch in assignment to " ++ name           -- Retunt error String ig not match



-- Check expressions
checkExpr :: FunEnv -> TypeEnv -> Expr -> Either String Type
checkExpr _ _ (IntLit _) = Right IntType
checkExpr _ _ (BoolLit _) = Right BooleanType
checkExpr _ _ (StringLit _) = Right UnitType  -- Strings are only used for printing, so treat as UnitType
checkExpr _ env (Id name) = case Map.lookup name env of
    Nothing -> Left $ "Undefined variable: " ++ name
    Just t -> Right t
checkExpr funEnv env (BinOp e1 op e2) = checkBinOp funEnv env e1 op e2
checkExpr funEnv env (UnOp op e) = checkUnOp funEnv env op e
checkExpr funEnv env (Print expr) = do
    _ <- checkExpr funEnv env expr
    Right UnitType
checkExpr _ _ ReadLn = Right UnitType  -- ReadLn returns UnitType since we're not handling strings



-- Check binary operations
checkBinOp :: FunEnv -> TypeEnv -> Expr -> BinOperator -> Expr -> Either String Type
checkBinOp funEnv env e1 op e2 = do
    t1 <- checkExpr funEnv env e1
    t2 <- checkExpr funEnv env e2
    case op of
        Add -> checkArithOp t1 t2
        Sub -> checkArithOp t1 t2
        Mul -> checkArithOp t1 t2
        Div -> checkArithOp t1 t2
        Mod -> checkArithOp t1 t2
        Eq -> if t1 == t2 then Right BooleanType else Left "Type mismatch in equality"
        Neq -> if t1 == t2 then Right BooleanType else Left "Type mismatch in inequality"
        Lt -> checkCompOp t1 t2
        Lte -> checkCompOp t1 t2
        Gt -> checkCompOp t1 t2
        Gte -> checkCompOp t1 t2
        And -> checkBoolOp t1 t2
        Or -> checkBoolOp t1 t2
        AssignOp -> if t1 == t2 then Right t1 else Left "Type mismatch in assignment"



-- Helper functions for type checking operations
checkArithOp :: Type -> Type -> Either String Type
checkArithOp IntType IntType = Right IntType
checkArithOp _ _ = Left "Type mismatch in arithmetic operation"



checkCompOp :: Type -> Type -> Either String Type
checkCompOp IntType IntType = Right BooleanType
checkCompOp _ _ = Left "Type mismatch in comparison operation"



checkBoolOp :: Type -> Type -> Either String Type
checkBoolOp BooleanType BooleanType = Right BooleanType
checkBoolOp _ _ = Left "Type mismatch in boolean operation"



-- Check unary operations
checkUnOp :: FunEnv -> TypeEnv -> UnOperator -> Expr -> Either String Type
checkUnOp funEnv env op e = do
    t <- checkExpr funEnv env e
    case (op, t) of
        (Neg, IntType) -> Right IntType
        (Not, BooleanType) -> Right BooleanType
        _ -> Left "Type mismatch in unary operation"



-- Check if statements
checkIf :: FunEnv -> TypeEnv -> If -> Either String (Type, TypeEnv)
checkIf funEnv env (If cond thenBlock elseBlock) = do
    condType <- checkExpr funEnv env cond                   -- Get condition type
    if condType /= BooleanType                              -- Compares if the condition is boolean
        then Left "Condition must be boolean"               -- Error if condition not boolean
        else do
            _ <- checkCmds funEnv env thenBlock
            _ <- checkCmds funEnv env elseBlock
            return (UnitType, env)                          -- Return void if no errors and atualized environment



-- Check while loops
checkWhile :: FunEnv -> TypeEnv -> Expr -> [Cmd] -> Either String (Type, TypeEnv)
checkWhile funEnv env cond body = do
    condType <- checkExpr funEnv env cond                   -- Get condition type
    if condType /= BooleanType                              -- Compares if the condition is boolean
        then Left "While condition must be boolean"         -- Error if condition not boolean
        else do
            _ <- checkCmds funEnv env body
            return (UnitType, env)