module SemanticAnalyzer where

import qualified Data.Map as Map
import AST

-- Types that our language supports
data Type = IntType 
          | BooleanType
          | UnitType
          | ErrorType String
          deriving (Eq, Show)

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
    Map.fromList [(name, (returnType, paramTypes)) | Main params _ <- functions,
                  let name = "main",
                      returnType = UnitType,
                      paramTypes = map (\(Param _ t) -> t) params]

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
    (_, env') <- checkCmd funEnv env cmd
    -- Check remaining commands with potentially updated environment
    checkCmds funEnv env' cmds

-- Check individual commands
checkCmd :: FunEnv -> TypeEnv -> Cmd -> Either String (Type, TypeEnv)
checkCmd funEnv env (DeclareCmd decl) = checkDeclaration funEnv env decl
checkCmd funEnv env (AssignCmd assign) = do
    checkAssignment funEnv env assign
    return (UnitType, env)
checkCmd funEnv env (IfCmd ifStmt) = do
    checkIf funEnv env ifStmt
    return (UnitType, env)
checkCmd funEnv env (WhileCmd cond body) = do
    condType <- checkExpr funEnv env cond
    unless (condType == BooleanType) $
        Left "While condition must be boolean"
    mapM_ (checkCmd funEnv env) body
    return (UnitType, env)
checkCmd funEnv env (ExprCmd expr) = do
    exprType <- checkExpr funEnv env expr
    return (exprType, env)
checkCmd funEnv env (ReturnCmd _) = 
    return (UnitType, env)

-- Check variable declarations
checkDeclaration :: FunEnv -> TypeEnv -> Declare -> Either String (Type, TypeEnv)
checkDeclaration funEnv env (ValDecl name declaredType expr) = do
    exprType <- checkExpr funEnv env expr
    if declaredType == UnitType || declaredType == exprType
        then return (UnitType, Map.insert name exprType env)
        else Left $ "Type mismatch in declaration of " ++ name
checkDeclaration funEnv env (VarDecl name declaredType expr) = do
    exprType <- checkExpr funEnv env expr
    if declaredType == UnitType || declaredType == exprType
        then return (UnitType, Map.insert name exprType env)
        else Left $ "Type mismatch in declaration of " ++ name
checkDeclaration _ env (VarDeclEmpty name declaredType) =
    return (UnitType, Map.insert name declaredType env)

-- Check assignments
checkAssignment :: FunEnv -> TypeEnv -> Assign -> Either String Type
checkAssignment funEnv env (Assign var expr) = do
    case Map.lookup var env of
        Nothing -> Left $ "Undefined variable: " ++ var
        Just varType -> do
            exprType <- checkExpr funEnv env expr
            if varType == exprType
                then Right UnitType
                else Left $ "Type mismatch in assignment to " ++ var

-- Check if statements
checkIf :: FunEnv -> TypeEnv -> If -> Either String Type
checkIf funEnv env (If cond thenBranch elseBranch) = do
    condType <- checkExpr funEnv env cond
    unless (condType == BooleanType) $
        Left "If condition must be boolean"
    mapM_ (checkCmd funEnv env) thenBranch
    mapM_ (checkCmd funEnv env) elseBranch
    return UnitType

-- Check expressions
checkExpr :: FunEnv -> TypeEnv -> Expr -> Either String Type
checkExpr _ _ (IntLit _) = Right IntType
checkExpr _ _ (BoolLit _) = Right BooleanType
checkExpr _ _ (StringLit _) = Right UnitType
checkExpr _ env (Id var) = 
    case Map.lookup var env of
        Nothing -> Left $ "Undefined variable: " ++ var
        Just t -> Right t
checkExpr funEnv env (BinOp e1 op e2) = do
    t1 <- checkExpr funEnv env e1
    t2 <- checkExpr funEnv env e2
    checkBinOp t1 op t2
checkExpr funEnv env (UnOp op e) = do
    t <- checkExpr funEnv env e
    checkUnOp op t
checkExpr funEnv env (Print e) = do
    _ <- checkExpr funEnv env e
    Right UnitType
checkExpr _ _ ReadLn = Right IntType

-- Check binary operations
checkBinOp :: Type -> BinOp -> Type -> Either String Type
checkBinOp IntType Add IntType = Right IntType
checkBinOp IntType Sub IntType = Right IntType
checkBinOp IntType Mul IntType = Right IntType
checkBinOp IntType Div IntType = Right IntType
checkBinOp IntType Mod IntType = Right IntType
checkBinOp IntType Lt IntType = Right BooleanType
checkBinOp IntType Lte IntType = Right BooleanType
checkBinOp IntType Gt IntType = Right BooleanType
checkBinOp IntType Gte IntType = Right BooleanType
checkBinOp t1 Eq t2 | t1 == t2 = Right BooleanType
checkBinOp t1 Neq t2 | t1 == t2 = Right BooleanType
checkBinOp BooleanType And BooleanType = Right BooleanType
checkBinOp BooleanType Or BooleanType = Right BooleanType
checkBinOp t1 op t2 = Left $ "Invalid types for operator: " ++ show t1 ++ " " ++ show op ++ " " ++ show t2

-- Check unary operations
checkUnOp :: UnOp -> Type -> Either String Type
checkUnOp Neg IntType = Right IntType
checkUnOp Not BooleanType = Right BooleanType
checkUnOp op t = Left $ "Invalid type for unary operator: " ++ show op ++ " " ++ show t

-- Helper function
unless :: Bool -> Either String () -> Either String ()
unless pred action = if pred then Right () else action