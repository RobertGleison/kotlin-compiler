{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use :" #-}
module MIPSGenerator where

import Data.List (isPrefixOf)
import Data.Char (ord)
import IR
import AST (BinOperator(..), UnOperator(..))
import qualified Data.Map as Map

-- MIPS instructions representation
data MipsInstr
    = MipsLabel String                     -- Labels
    | MipsLi String Int                    -- Load immediate
    | MipsMove String String               -- Move between registers
    | MipsAdd String String String         -- Add
    | MipsSub String String String         -- Subtract
    | MipsMul String String String         -- Multiply
    | MipsDiv String String                -- Divide (result in lo)
    | MipsAnd String String String         -- And
    | MipsOr String String String          -- Or
    | MipsXor String String String         -- Xor
    | MipsLt String String String          -- Lt
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
    | MipsMfhi String                      -- Move from hi register
    | MipsSyscall                          -- System call instruction
    | MipsLa String String                 -- Load address
    | MipsDataString String String         -- String in data section
    | MipsDataSpace String Int             -- Space allocation
    deriving Show

-- State management
data CodeGenState = CodeGenState 
    { nextReg :: Int               -- Next available temporary register
    , regMap :: Map.Map Temp String -- Map IR temporaries to MIPS registers
    }

-- State Initialization
-------------------------------------------------------------------------------
initialState :: CodeGenState
initialState = CodeGenState 0 Map.empty

-- Data Section Generation
-------------------------------------------------------------------------------
generateData :: IRProg -> [MipsInstr]
generateData prog =
    [ MipsDataString "newline" "\n"     -- Simplified newline
    , MipsDataSpace "buffer" 100
    ] ++
    concatMap extractStrings prog
    where
        extractStrings :: IRInstr -> [MipsInstr]
        extractStrings (STRINGCONST t str) = 
            [MipsDataString t ("\"" ++ str ++ "\"")]
        extractStrings _ = []

isStringTemp :: String -> Bool
isStringTemp t = "str_" `isPrefixOf` t


-- Library Function Generation
-------------------------------------------------------------------------------
generateLibrary :: [MipsInstr]
generateLibrary = 
    [ -- Print string function
      MipsLabel "print_string"
    , MipsLw "$a0" 0 "$sp"     -- load string address from stack
    , MipsLi "$v0" 4           -- syscall 4 = print string
    , MipsSyscall
    , MipsMove "$v0" "$zero"   -- Return 0
    , MipsJr "$ra"
    
    -- Print integer function
    , MipsLabel "print_int"
    , MipsLw "$a0" 0 "$sp"     -- load integer from stack
    , MipsLi "$v0" 1           -- syscall 1 = print integer
    , MipsSyscall
    , MipsMove "$v0" "$zero"   -- Return 0
    , MipsJr "$ra"
    
    -- Read integer function
    , MipsLabel "scan_int"
    , MipsLi "$v0" 5           -- syscall 5 = read integer
    , MipsSyscall              -- Result already in $v0
    , MipsJr "$ra"
    
    -- Read string function
    , MipsLabel "scan_string"
    , MipsLw "$a0" 0 "$sp"     -- load buffer address
    , MipsLw "$a1" 4 "$sp"     -- load buffer size
    , MipsLi "$v0" 8           -- syscall 8 = read string
    , MipsSyscall
    , MipsJr "$ra"
    ]

-- Instruction Translation
-------------------------------------------------------------------------------
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
        Eq  -> [MipsLt "$t8" (getReg src1) (getReg src2),
                MipsLt "$t9" (getReg src2) (getReg src1),
                MipsOr (getReg dst) "$t8" "$t9",
                MipsXor (getReg dst) (getReg dst) "1"]
        Neq -> [MipsLt "$t8" (getReg src1) (getReg src2),
                MipsLt "$t9" (getReg src2) (getReg src1),
                MipsOr (getReg dst) "$t8" "$t9"]
        Lt  -> [MipsLt (getReg dst) (getReg src1) (getReg src2)]
        Gt  -> [MipsLt (getReg dst) (getReg src2) (getReg src1)]
        
        Gte -> [MipsLt (getReg dst) (getReg src1) (getReg src2),
                MipsXor (getReg dst) (getReg dst) "1"]
                
        Lte -> [MipsLt (getReg dst) (getReg src2) (getReg src1),
                MipsXor (getReg dst) (getReg dst) "1"]
        And -> [MipsLt "$t8" "$zero" (getReg src1), 
                MipsLt "$t9" "$zero" (getReg src2),
                MipsAnd (getReg dst) "$t8" "$t9" ]
        Or  -> [MipsLt "$t8" "$zero" (getReg src1),
                MipsLt "$t9" "$zero" (getReg src2),
                MipsOr (getReg dst) "$t8" "$t9"]
        Add -> [MipsAdd (getReg dst) (getReg src1) (getReg src2)]
        Sub -> [MipsSub (getReg dst) (getReg src1) (getReg src2)]
        Mul -> [MipsMul (getReg dst) (getReg src1) (getReg src2)]
        Div -> [MipsDiv (getReg src1) (getReg src2),
                MipsMflo (getReg dst)]
        Mod -> [MipsDiv (getReg src1) (getReg src2),  -- Perform division
                MipsMfhi (getReg dst)] 
        _ -> error $ "Unsupported binary operator: " ++ show op

translateInstr (UNOP op dst src) =
    [MipsComment $ "UNOP " ++ show op] ++
    case op of
        Neg -> [MipsSub (getReg dst) "$zero" (getReg src)]
        Not -> [MipsXor (getReg dst) (getReg src) "1"]

translateInstr (LABEL lbl) = [MipsLabel lbl]
translateInstr (JUMP lbl) = [MipsJ lbl]

translateInstr (CJUMP op src1 src2 lbl) =
    [MipsComment $ "CJUMP " ++ show op] ++
    case op of
        Eq  -> [MipsBne (getReg src1) (getReg src2) lbl]
        Neq -> [MipsBeq (getReg src1) (getReg src2) lbl]
        Lt  -> [MipsBlt (getReg src1) (getReg src2) lbl]
        Gt  -> [MipsBgt (getReg src1) (getReg src2) lbl]
        _ -> error $ "Unsupported comparison operator: " ++ show op

translateInstr (CALL dst fname args) =
    let setupStack = if not (null args)
                    then [MipsSub "$sp" "$sp" (show (4 * length args))]
                    else []
        saveArgs = zipWith (\arg pos -> 
            MipsSw (getReg arg) (pos * 4) "$sp") args [0..]
        restoreStack = if not (null args)
                      then [MipsAdd "$sp" "$sp" (show (4 * length args))]
                      else []
        -- Check if any argument is in the data section (meaning it's a string)
        actualFname = case fname of
            "print" -> "print_string"  -- Since we can see string constants in data section
            other -> other
    in [MipsComment $ "CALL " ++ fname,
        MipsSw "$ra" (-4) "$sp"] ++    
        setupStack ++
        saveArgs ++
        [MipsJal actualFname] ++
        restoreStack ++
        [MipsLw "$ra" (-4) "$sp",     
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

translateInstr (STRINGCONST temp str) =
    [MipsComment $ "STRING CONST " ++ temp ++ " = " ++ show str,
     MipsLa (getReg temp) temp]

translateInstr NOP = [MipsComment "NOP"]

-- Helper Functions
-------------------------------------------------------------------------------
needsLibraryFunction :: IRInstr -> Bool
needsLibraryFunction (CALL _ fname _) = fname `elem` ["print", "print_string", "print_int", "scan"]
needsLibraryFunction _ = False

getReg :: Temp -> String
getReg temp = 
    if temp `elem` ["$v0", "$a0", "$ra", "$sp", "$fp", "$zero"]
    then temp
    else case reads (tail temp) :: [(Int, String)] of
         [(n, "")] -> "$t" ++ show (n `mod` 8)
         _ -> temp

-- MIPS Generation
-------------------------------------------------------------------------------
generateMips :: IRProg -> [MipsInstr]
generateMips irProg = 
    let header = [ MipsComment "Program Start"
                , MipsJ "main"
                , MipsLabel "main"
                ]
        code = concatMap translateInstr irProg
        footer = [ MipsComment "Program End"
                , MipsLi "$v0" 10      -- syscall 10 is exit
                , MipsSyscall
                ]
        needsLibrary = any needsLibraryFunction irProg
    in header ++ code ++ footer ++ (if needsLibrary then generateLibrary else [])

-- String Generation
-------------------------------------------------------------------------------
mipsToString :: MipsInstr -> String
mipsToString (MipsLabel lbl) = lbl ++ ":"
mipsToString (MipsLi reg val) = "\tli " ++ reg ++ ", " ++ show val
mipsToString (MipsMove dst src) = "\tmove " ++ dst ++ ", " ++ src
mipsToString (MipsAdd dst src1 src2) = "\tadd " ++ dst ++ ", " ++ src1 ++ ", " ++ src2
mipsToString (MipsSub dst src1 src2) = "\tsub " ++ dst ++ ", " ++ src1 ++ ", " ++ src2
mipsToString (MipsMul dst src1 src2) = "\tmul " ++ dst ++ ", " ++ src1 ++ ", " ++ src2
mipsToString (MipsDiv src1 src2) = "\tdiv " ++ src1 ++ ", " ++ src2
mipsToString (MipsAnd dst src1 src2) = "\tand " ++ dst ++ ", " ++ src1 ++ ", " ++ src2
mipsToString (MipsOr dst src1 src2) = "\tor " ++ dst ++ ", " ++ src1 ++ ", " ++ src2
mipsToString (MipsXor dst src1 src2) = "\txor " ++ dst ++ ", " ++ src1 ++ ", " ++ src2
mipsToString (MipsLt dst src1 src2) = "\tslt " ++ dst ++ ", " ++ src1 ++ ", " ++ src2
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
mipsToString (MipsMfhi dst) = "\tmfhi " ++ dst
mipsToString MipsSyscall = "\tsyscall"
mipsToString (MipsLa dst label) = "\tla " ++ dst ++ ", " ++ label
mipsToString (MipsDataString label str) = label ++ ": .asciiz " ++ str
mipsToString (MipsDataSpace label size) = label ++ ": .space " ++ show size

-- Main Assembly Generation
-------------------------------------------------------------------------------
generateAssembly :: IRProg -> String
generateAssembly irProg = 
    unlines $ 
    [".data"] ++
    map mipsToString (generateData irProg) ++
    [".text"] ++ 
    map mipsToString (generateMips irProg)