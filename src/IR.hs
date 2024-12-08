module IR where

import AST (BinOperator(..), UnOperator(..))

-- Temporaries and labels are strings
type Temp = String   -- For temporary variables (t1, t2, etc)
type Label = String  -- For control flow labels (L1, L2, etc)

-- IR Instructions
data IRInstr 
    = MOVE Temp Temp                      -- Move between temps: t1 := t2 
    | CONST Temp Int                      -- Load constant: t1 := 5
    | STRINGCONST Temp String             -- Load string constant: t1 := "hello"
    | BINOP BinOperator Temp Temp Temp    -- Binary op: t1 := t2 op t3
    | UNOP UnOperator Temp Temp           -- Unary op: t1 := op t2
    | LABEL Label                         -- Define label: L1:
    | JUMP Label                          -- Unconditional jump: goto L1
    | CJUMP BinOperator Temp Temp Label   -- Conditional jump: if t1 op t2 goto L1
    | CALL Temp String [Temp]             -- Function call: t1 := f(t2,t3,...)
    | RETURN Temp                         -- Return value: return t1
    | STORE Temp Temp                     -- Store to memory: mem[t1] := t2
    | LOAD Temp Temp                      -- Load from memory: t1 := mem[t2]
    | NOP                                 -- No operation
    deriving (Show)

-- Program is a sequence of instructions
type IRProg = [IRInstr]