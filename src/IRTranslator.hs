module IRTranslator where

import qualified Data.Map as Map
import AST
import IR

type StringTemp = String

data TransState = TransState 
    { tempCount :: Int
    , labelCount :: Int
    , varEnv :: Map.Map String Temp
    , stringTemps :: Map.Map String StringTemp  -- Track string literals
    }

-- Initialize empty state
initialState :: TransState
initialState = TransState 
    { tempCount = 0
    , labelCount = 0
    , varEnv = Map.empty
    , stringTemps = Map.empty
    }

newStringTemp :: String -> TransState -> (StringTemp, TransState)
newStringTemp str state = 
    case Map.lookup str (stringTemps state) of
        Just temp -> (temp, state)  -- Reuse existing temp for same string
        Nothing -> 
            let count = tempCount state
                temp = "str_" ++ show count
                newState = state { 
                    tempCount = count + 1,
                    stringTemps = Map.insert str temp (stringTemps state)
                }
            in (temp, newState)

-- Generate new temporary
newTemp :: TransState -> (Temp, TransState)
newTemp state = 
    let count = tempCount state
        temp = "t" ++ show count
    in (temp, state { tempCount = count + 1 })

convert :: Int -> Temp
convert n = if even n then "$t8" else "$t9"

getReservedTemp :: TransState -> (Temp, TransState)
getReservedTemp state =
    let count = tempCount state
        temp = convert count
    in (temp, state { tempCount = count + 1 })

-- Generate new label
newLabel :: TransState -> (Label, TransState)
newLabel state = 
    let count = labelCount state
        label = "L" ++ show count
    in (label, state { labelCount = count + 1 })


-- Main translation function for the program
translateProgram :: AST -> IRProg
translateProgram (Program functions) = 
    let (prog, _) = translateFunctions functions initialState
    in prog

-- Translate list of functions
translateFunctions :: [Function] -> TransState -> (IRProg, TransState)
translateFunctions [] state = ([], state)
translateFunctions (f:fs) state =
    let (prog1, state1) = translateFunction f state
        (prog2, state2) = translateFunctions fs state1
    in (prog1 ++ prog2, state2)

-- Translate single function
translateFunction :: Function -> TransState -> (IRProg, TransState)
translateFunction (Main params body) state =
    let -- Create new environment with parameters mapped to temps
        (paramTemps, state1) = foldr (\(Param name _) (temps, st) ->
            let (temp, st') = newTemp st
            in (temp:temps, st')) ([], state { varEnv = Map.empty }) params
        newEnv = Map.fromList (zip (map (\(Param name _) -> name) params) paramTemps)
        state2 = state1 { varEnv = newEnv }
        -- Translate function body
        (bodyCode, state3) = translateCmds body state2
    in (bodyCode, state3)

-- Translate list of commands
translateCmds :: [Cmd] -> TransState -> (IRProg, TransState)
translateCmds [] state = ([], state)
translateCmds (cmd:cmds) state =
    let (prog1, state1) = translateCmd cmd state
        (prog2, state2) = translateCmds cmds state1
    in (prog1 ++ prog2, state2)

-- Translate single command
translateCmd :: Cmd -> TransState -> (IRProg, TransState)
translateCmd (DeclareCmd decl) state = translateDeclare decl state
translateCmd (AssignCmd assign) state = translateAssign assign state
translateCmd (IfCmd ifStmt) state = translateIf ifStmt state
translateCmd (WhileCmd cond body) state = translateWhile cond body state
translateCmd (ReturnCmd ReturnEmpty) state = 
    let (temp, state1) = newTemp state
    in ([CONST temp 0, RETURN temp], state1)
translateCmd (ExprCmd expr) state =
    let (temp, code, state1) = translateExpr expr state
    in (code, state1)

-- Translate declarations
translateDeclare :: Declare -> TransState -> (IRProg, TransState)
translateDeclare (ValDecl name _ expr) state =
    let (temp, code, state1) = translateExpr expr state
        newEnv = Map.insert name temp (varEnv state1)
    in (code, state1 { varEnv = newEnv })
translateDeclare (VarDecl name _ expr) state =
    translateDeclare (ValDecl name UnitType expr) state
translateDeclare (VarDeclEmpty name _) state =
    let (varTemp, state1) = newTemp state
        newEnv = Map.insert name varTemp (varEnv state1)
    in ([], state1 { varEnv = newEnv })

-- Translate assignments
translateAssign :: Assignment -> TransState -> (IRProg, TransState)
translateAssign (Assign name expr) state =
    case Map.lookup name (varEnv state) of
        Just varTemp ->
            let (exprTemp, code, state1) = translateExpr expr state
            in (code ++ [MOVE varTemp exprTemp], state1)
        Nothing -> error $ "Undefined variable: " ++ name

-- Translate if statements
translateIf :: If -> TransState -> (IRProg, TransState)
translateIf (If cond thenBlock elseBlock) state =
    let (condTemp, condCode, state1) = translateExpr cond state
        (thenLabel, state2) = newLabel state1
        (elseLabel, state3) = newLabel state2
        (endLabel, state4) = newLabel state3
        (thenCode, state5) = translateCmds thenBlock state4
        (elseCode, state6) = translateCmds elseBlock state5
        (trueTemp, state7) = getReservedTemp state6 
    in ( [CONST trueTemp 1] ++
         condCode ++
         [CJUMP Eq condTemp trueTemp thenLabel,
          JUMP elseLabel,
          LABEL thenLabel] ++
         thenCode ++
         [JUMP endLabel,
          LABEL elseLabel] ++
         elseCode ++
         [LABEL endLabel]
       , state7)

-- Translate while loops
translateWhile :: Expr -> [Cmd] -> TransState -> (IRProg, TransState)
translateWhile cond body state =
    let (startLabel, state1) = newLabel state
        (bodyLabel, state2) = newLabel state1
        (endLabel, state3) = newLabel state2
        (condTemp, condCode, state4) = translateExpr cond state3
        (bodyCode, state5) = translateCmds body state4
        (trueTemp, state6) = newTemp state5
    in ( [LABEL startLabel,
          CONST trueTemp 1] ++
         condCode ++
         [CJUMP Eq condTemp trueTemp bodyLabel,
          JUMP endLabel,
          LABEL bodyLabel] ++
         bodyCode ++
         [JUMP startLabel,
          LABEL endLabel]
       , state6)

-- Translate expressions
translateExpr :: Expr -> TransState -> (Temp, IRProg, TransState)
translateExpr (IntLit n) state =
    let (temp, state1) = newTemp state
    in (temp, [CONST temp n], state1)

translateExpr (StringLit s) state =
    let (temp, state1) = newTemp state
    in (temp, [STRINGCONST temp s], state1)

translateExpr (BoolLit b) state =
    let (temp, state1) = newTemp state
    in (temp, [CONST temp (if b then 1 else 0)], state1)

translateExpr (Id name) state =
    case Map.lookup name (varEnv state) of
        Just temp -> (temp, [], state)
        Nothing -> error $ "Undefined variable: " ++ name

translateExpr (BinOp e1 op e2) state =
    let (temp1, code1, state1) = translateExpr e1 state
        (temp2, code2, state2) = translateExpr e2 state1
        (resultTemp, state3) = newTemp state2
    in (resultTemp, code1 ++ code2 ++ [BINOP op resultTemp temp1 temp2], state3)

translateExpr (UnOp op e) state =
    let (temp, code, state1) = translateExpr e state
        (resultTemp, state2) = newTemp state1
    in (resultTemp, code ++ [UNOP op resultTemp temp], state2)

translateExpr (Print e) state =
    let (temp, code, state1) = translateExpr e state
        (resultTemp, state2) = newTemp state1
    in (resultTemp, code ++ [CALL resultTemp "print" [temp]], state2)

translateExpr ReadLn state =
    let (temp, state1) = newTemp state
    in (temp, [CALL temp "scan" []], state1)