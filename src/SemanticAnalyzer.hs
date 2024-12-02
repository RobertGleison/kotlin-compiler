module SemanticAnalyzer where

import qualified Data.Map as Map
import AST (AST(..), Program(..), Function(..), Expr(..), Cmd(..), Declare(..), 
            Assignment(..), If(..), Param(..), Type(..), BinOperator(..), UnOperator(..))

-- Environment maps variable names to their types
type TypeEnv = Map.Map String Type

-- Function environment maps function names to their type signatures
type FunEnv = Map.Map String (Type, [Type]) -- (return type, parameter types)

-- Main type checking function for the whole program
checkProgram :: AST -> Either String Type
checkProgram (Program functions) = do
    -- Build initial function environment
    let funEnv = buildFunctionEnv functions
    -- Check main function exists
    checkMain functions
    -- Type check each function
    mapM_ (checkFunction funEnv) functions
    return UnitType

-- Build environment containing function signatures
buildFunctionEnv :: [Function] -> FunEnv
buildFunctionEnv functions = 
    Map.fromList [(name, (retType, paramTypes)) 
                 | Main params _ <- functions
                 , let name = "main"
                 , let retType = UnitType
                 , let paramTypes = map getParamType params
                 ]
    where
        getParamType (Param _ t) = t

-- Check that main function exists and has correct signature
checkMain :: [Function] -> Either String ()
checkMain functions = 
    case filter isMain functions of
        [] -> Left "No main function defined"
        [_] -> Right ()
        _ -> Left "Multiple main functions defined"
    where isMain (Main _ _) = True

-- Type check a single function
checkFunction :: FunEnv -> Function -> Either String Type
checkFunction funEnv (Main params body) = do
    -- Create initial environment from parameters
    let env = Map.fromList [(name, paramType) | Param name paramType <- params]
    -- Check all statements in function body
    checkCmds funEnv env body

-- Check a list of commands
checkCmds :: FunEnv -> TypeEnv -> [Cmd] -> Either String Type
checkCmds _ _ [] = Right UnitType
checkCmds funEnv env (cmd:cmds) = do
    -- Check current command
    (cmdType, env') <- checkCmd funEnv env cmd
    -- Check remaining commands with potentially updated environment
    checkCmds funEnv env' cmds

-- Check individual commands
checkCmd :: FunEnv -> TypeEnv -> Cmd -> Either String (Type, TypeEnv)
checkCmd funEnv env (DeclareCmd decl) = checkDeclare funEnv env decl
checkCmd funEnv env (AssignCmd assign) = checkAssign funEnv env assign
checkCmd funEnv env (IfCmd ifStmt) = checkIf funEnv env ifStmt
checkCmd funEnv env (WhileCmd cond body) = checkWhile funEnv env cond body
checkCmd funEnv env (ReturnCmd _) = Right (UnitType, env)
checkCmd funEnv env (ExprCmd expr) = do
    exprType <- checkExpr funEnv env expr
    return (exprType, env)

-- Check declarations
checkDeclare :: FunEnv -> TypeEnv -> Declare -> Either String (Type, TypeEnv)
checkDeclare funEnv env (ValDecl name declType expr) = do
    exprType <- checkExpr funEnv env expr
    if exprType == declType
        then return (UnitType, Map.insert name declType env)
        else Left $ "Type mismatch in declaration of " ++ name

checkDeclare funEnv env (VarDecl name declType expr) = do
    exprType <- checkExpr funEnv env expr
    if exprType == declType
        then return (UnitType, Map.insert name declType env)
        else Left $ "Type mismatch in declaration of " ++ name

checkDeclare _ env (VarDeclEmpty name declType) =
    return (UnitType, Map.insert name declType env)

-- Check assignments
checkAssign :: FunEnv -> TypeEnv -> Assignment -> Either String (Type, TypeEnv)
checkAssign funEnv env (Assign name expr) = do
    case Map.lookup name env of
        Nothing -> Left $ "Variable " ++ name ++ " not declared"
        Just varType -> do
            exprType <- checkExpr funEnv env expr
            if exprType == varType
                then return (UnitType, env)
                else Left $ "Type mismatch in assignment to " ++ name

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
    condType <- checkExpr funEnv env cond
    if condType /= BooleanType
        then Left "Condition must be boolean"
        else do
            _ <- checkCmds funEnv env thenBlock
            _ <- checkCmds funEnv env elseBlock
            return (UnitType, env)

-- Check while loops
checkWhile :: FunEnv -> TypeEnv -> Expr -> [Cmd] -> Either String (Type, TypeEnv)
checkWhile funEnv env cond body = do
    condType <- checkExpr funEnv env cond
    if condType /= BooleanType
        then Left "While condition must be boolean"
        else do
            _ <- checkCmds funEnv env body
            return (UnitType, env)