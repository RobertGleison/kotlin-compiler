module MIPSGenerator where

import IR
import AST (BinOperator(..), UnOperator(..))
import qualified Data.Map as Map

-- MIPS instructions as strings for easier generation
data MipsInstr
    = MipsLabel String                     -- Labels
    | MipsLi String Int                    -- Load immediate
    | MipsMove String String               -- Move between registers
    | MipsAdd String String String         -- Add
    | MipsSub String String String         -- Subtract
    | MipsMul String String String         -- Multiply
    | MipsDiv String String                -- Divide (result in lo)
    | MipsMflo String                      -- Move from lo
    | MipsLw String Int String             -- Load word
    | MipsSw String Int String             -- Store word
    | MipsJ String                         -- Jump
    | MipsBne String String String         -- Branch if not equal
    | MipsBeq String String String         -- Branch if equal
    | MipsBlt String String String         -- Branch if less than
    | MipsBgt String String String         -- Branch if greater than
    | MipsJal String                       -- Jump and link (function call)
    | MipsJr String                        -- Jump register (return)
    | MipsComment String                   -- Comments for readability
    | MipsAnd String String String         -- Logical AND
    | MipsOr String String String          -- Logical OR
    deriving Show

-- State for managing registers and memory
data CodeGenState = CodeGenState 
    { nextReg :: Int               -- Next available temporary register
    , regMap :: Map.Map Temp String -- Map IR temporaries to MIPS registers
    }

initialState :: CodeGenState
initialState = CodeGenState 0 Map.empty

-- Convert IR program to MIPS
generateMips :: IRProg -> [MipsInstr]
generateMips irProg = 
    let header = [ MipsComment "Program Start"
                , MipsJ "main"
                , MipsLabel "main"        -- Added this line to create main label
                ]
        code = concatMap translateInstr irProg
        footer = [MipsComment "Program End"]
    in header ++ code ++ footer ++ generateLibrary

-- Generate standard library functions (print_int, scan_int, etc)
generateLibrary :: [MipsInstr]
generateLibrary = 
    [ MipsLabel "print"
    , MipsLi "$v0" 1           -- syscall 1 = print integer
    , MipsLw "$a0" 0 "$sp"     -- load argument from stack
    , MipsComment "syscall to print"
    , MipsJr "$ra"             -- return
    
    , MipsLabel "scan"
    , MipsLi "$v0" 5           -- syscall 5 = read integer
    , MipsComment "syscall to read"
    , MipsJr "$ra"             -- return
    ]

-- Translate single IR instruction to MIPS
translateInstr :: IRInstr -> [MipsInstr]
translateInstr (MOVE dst src) = 
    [MipsComment $ "MOVE " ++ dst ++ " " ++ src,
     MipsMove (getReg dst) (getReg src)]

translateInstr (CONST temp val) =
    [MipsComment $ "CONST " ++ temp ++ " " ++ show val,
     MipsLi (getReg temp) val]

translateInstr (BINOP op dst src1 src2) =
    [MipsComment $ "BINOP " ++ show op] ++
    case op of
        Add -> [MipsAdd (getReg dst) (getReg src1) (getReg src2)]
        Sub -> [MipsSub (getReg dst) (getReg src1) (getReg src2)]
        Mul -> [MipsMul (getReg dst) (getReg src1) (getReg src2)]
        Div -> [MipsDiv (getReg src1) (getReg src2),
                MipsMflo (getReg dst)]
        And -> [ MipsAnd (getReg dst) (getReg src1) (getReg src2) ]
        Or  -> [ MipsOr (getReg dst) (getReg src1) (getReg src2) ]
        _ -> error $ "Unsupported binary operator: " ++ show op

translateInstr (UNOP op dst src) =
    [MipsComment $ "UNOP " ++ show op] ++
    case op of
        Neg -> [MipsSub (getReg dst) "$zero" (getReg src)]
        _ -> error $ "Unsupported unary operator: " ++ show op

translateInstr (LABEL lbl) = 
    [MipsLabel lbl]

translateInstr (JUMP lbl) =
    [MipsJ lbl]


translateInstr (CJUMP op src1 src2 lbl) =
    [MipsComment $ "CJUMP " ++ show op] ++
    case op of
        Eq  -> [MipsBne (getReg src1) (getReg src2) lbl]
        Neq -> [MipsBeq (getReg src1) (getReg src2) lbl]
        Lt  -> [MipsBlt (getReg src1) (getReg src2) lbl]
        Gt  -> [MipsBgt (getReg src1) (getReg src2) lbl]
        _ -> error $ "Unsupported comparison operator: " ++ show op

translateInstr (CALL dst fname args) =
    let saveArgs = zipWith (\arg pos -> 
            MipsSw (getReg arg) (pos * 4) "$sp") args [0..]
    in [MipsComment $ "CALL " ++ fname] ++
       saveArgs ++
       [MipsJal fname,
        MipsMove (getReg dst) "$v0"]

translateInstr (RETURN temp) =
    [MipsComment "RETURN",
     MipsMove "$v0" (getReg temp),
     MipsJr "$ra"]

translateInstr (STORE addr val) =
    [MipsComment "STORE",
     MipsSw (getReg val) 0 (getReg addr)]

translateInstr (LOAD dst addr) =
    [MipsComment "LOAD",
     MipsLw (getReg dst) 0 (getReg addr)]

translateInstr NOP = 
    [MipsComment "NOP"]

-- Helper function to map IR temporaries to MIPS registers
getReg :: Temp -> String
getReg temp = 
    if temp `elem` ["$v0", "$a0", "$ra", "$sp", "$fp", "$zero"]
    then temp  -- These are actual MIPS registers
    else case reads (tail temp) :: [(Int, String)] of
         [(n, "")] -> "$t" ++ show (n `mod` 8)  -- Convert t11 to $t3 (11 mod 8 = 3)
         _ -> temp  -- If parsing fails, return unchanged


-- Convert MIPS instructions to strings
mipsToString :: MipsInstr -> String
mipsToString (MipsLabel lbl) = lbl ++ ":"
mipsToString (MipsLi reg val) = "\tli " ++ reg ++ ", " ++ show val
mipsToString (MipsMove dst src) = "\tmove " ++ dst ++ ", " ++ src
mipsToString (MipsAdd dst src1 src2) = "\tadd " ++ dst ++ ", " ++ src1 ++ ", " ++ src2
mipsToString (MipsSub dst src1 src2) = "\tsub " ++ dst ++ ", " ++ src1 ++ ", " ++ src2
mipsToString (MipsMul dst src1 src2) = "\tmul " ++ dst ++ ", " ++ src1 ++ ", " ++ src2
mipsToString (MipsDiv src1 src2) = "\tdiv " ++ src1 ++ ", " ++ src2
mipsToString (MipsMflo dst) = "\tmflo " ++ dst
mipsToString (MipsLw dst offset src) = "\tlw " ++ dst ++ ", " ++ show offset ++ "(" ++ src ++ ")"
mipsToString (MipsSw src offset dst) = "\tsw " ++ src ++ ", " ++ show offset ++ "(" ++ dst ++ ")"
mipsToString (MipsJ lbl) = "\tj " ++ lbl
mipsToString (MipsBne src1 src2 lbl) = "\tbne " ++ src1 ++ ", " ++ src2 ++ ", " ++ lbl
mipsToString (MipsBeq src1 src2 lbl) = "\tbeq " ++ src1 ++ ", " ++ src2 ++ ", " ++ lbl
mipsToString (MipsBlt src1 src2 lbl) = "\tblt " ++ src1 ++ ", " ++ src2 ++ ", " ++ lbl
mipsToString (MipsBgt src1 src2 lbl) = "\tbgt " ++ src1 ++ ", " ++ src2 ++ ", " ++ lbl
mipsToString (MipsJal lbl) = "\tjal " ++ lbl
mipsToString (MipsJr reg) = "\tjr " ++ reg
mipsToString (MipsComment comment) = "\t# " ++ comment
mipsToString (MipsAnd dst src1 src2) = "\tand " ++ dst ++ ", " ++ src1 ++ ", " ++ src2
mipsToString (MipsOr dst src1 src2) = "\tor " ++ dst ++ ", " ++ src1 ++ ", " ++ src2

-- Generate final MIPS assembly string
generateAssembly :: IRProg -> String
generateAssembly irProg = 
    unlines $ [".text"] ++ 
    map mipsToString (generateMips irProg)