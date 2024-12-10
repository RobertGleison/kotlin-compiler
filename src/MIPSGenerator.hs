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
    | MipsSeq String String String         -- Set if equal
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
initialState :: CodeGenState
initialState = CodeGenState 0 Map.empty

-- Data Section Generation
generateData :: IRProg -> [MipsInstr]
generateData prog =
    [ MipsDataString "newline" "\"\\n\""     -- Properly escaped newline
    , MipsDataSpace "buffer" 100            -- Buffer for string input
    ] ++
    concatMap extractStrings prog
    where
        extractStrings :: IRInstr -> [MipsInstr]
        extractStrings (STRINGCONST t str) = 
            [MipsDataString t (processString str)]
        extractStrings _ = []


isStringTemp :: String -> [MipsInstr] -> Bool
isStringTemp temp dataSection = any (isDataString temp) dataSection
  where
    isDataString t (MipsDataString label _) = t == label
    isDataString _ _ = False

-- Library Function Generation
generateLibrary :: [MipsInstr]
generateLibrary = 
    [ -- Print string function
      MipsLabel "print_string"
    , MipsLw "$a0" 0 "$sp"
    , MipsLi "$v0" 4
    , MipsSyscall
    , MipsMove "$v0" "$zero"
    , MipsJr "$ra"
    
    -- Print integer function
    , MipsLabel "print_int"
    , MipsLw "$a0" 0 "$sp"
    , MipsLi "$v0" 1
    , MipsSyscall
    , MipsMove "$v0" "$zero"
    , MipsJr "$ra"
    
    -- Read string function
    , MipsLabel "scan_string"
    , MipsLw "$a0" 0 "$sp"      -- Load buffer address
    , MipsLw "$a1" 4 "$sp"      -- Load buffer size
    , MipsLi "$v0" 8            -- syscall 8 = read string
    , MipsSyscall
    , MipsMove "$v0" "$a0"      -- Return buffer address
    , MipsJr "$ra"
    
    -- Read integer function
    , MipsLabel "scan_int"
    , MipsLi "$v0" 5
    , MipsSyscall
    , MipsJr "$ra"
    ]

-- Base instruction translation
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
        Eq  -> [MipsSeq (getReg dst) (getReg src1) (getReg src2)]  -- Use $t9 instead of immediate 1
                
        -- Not Equal (!=): XOR them, then use slt to check if result is not 0
        Neq -> [MipsSeq (getReg dst) (getReg src1) (getReg src2),
                MipsXor (getReg dst) (getReg dst) "1"]
                
        -- Less Than (<): Direct slt
        Lt  -> [MipsLt (getReg dst) (getReg src1) (getReg src2)]
                
        -- Greater Than (>): Swap operands in slt
        Gt  -> [MipsLt (getReg dst) (getReg src2) (getReg src1)]
                
        -- Greater Than or Equal (>=): slt and invert result
        Gte -> [MipsLt (getReg dst) (getReg src1) (getReg src2),
                MipsXor (getReg dst) (getReg dst) "1"]
                
        -- Less Than or Equal (<=): Swap operands in slt and invert result
        Lte -> [MipsLt (getReg dst) (getReg src2) (getReg src1),
                MipsXor (getReg dst) (getReg dst) "1"]
        And -> [MipsAnd (getReg dst) (getReg src1) (getReg src2)]
        Or  -> [MipsOr (getReg dst) (getReg src1) (getReg src2)]
        Add -> [MipsAdd (getReg dst) (getReg src1) (getReg src2)]
        Sub -> [MipsSub (getReg dst) (getReg src1) (getReg src2)]
        Mul -> [MipsMul (getReg dst) (getReg src1) (getReg src2)]
        Div -> [MipsDiv (getReg src1) (getReg src2),
                MipsMflo (getReg dst)]
        Mod -> [MipsDiv (getReg src1) (getReg src2),
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
        Eq  -> [MipsBeq (getReg src1) (getReg src2) lbl]
        Neq -> [MipsBne (getReg src1) (getReg src2) lbl]
        Lt  -> [MipsBlt (getReg src1) (getReg src2) lbl]
        Gt  -> [MipsBeq (getReg src1) "1" lbl]
        _ -> error $ "Unsupported comparison operator: " ++ show op

translateInstr (STRINGCONST temp str) =
    [MipsComment $ "STRING CONST " ++ temp,
     MipsLa (getReg temp) temp]

translateInstr (STORE addr val) =
    [MipsComment "STORE",
     MipsSw (getReg val) 0 (getReg addr)]

translateInstr (LOAD dst addr) =
    [MipsComment "LOAD",
     MipsLw (getReg dst) 0 (getReg addr)]

translateInstr (RETURN temp) =
    [MipsComment "RETURN",
     MipsMove "$v0" (getReg temp),
     MipsJr "$ra"]

translateInstr NOP = [MipsComment "NOP"]


processString :: String -> String
processString s = "\"" ++ concatMap escapeChar s ++ "\""
  where
    escapeChar '\n' = "\\n"
    escapeChar '$' = "\\$"  -- Escape dollar signs
    escapeChar c = [c]


-- Instruction translation with data section context
translateInstrWithData :: [MipsInstr] -> IRInstr -> [MipsInstr]
translateInstrWithData dataSection (CALL dst fname args) =
    let setupStack = if not (null args)
                    then [MipsSub "$sp" "$sp" (show (4 * length args))]
                    else []
        saveArgs = zipWith (\arg pos -> 
            MipsSw (getReg arg) (pos * 4) "$sp") args [0..]
        restoreStack = if not (null args)
                      then [MipsAdd "$sp" "$sp" (show (4 * length args))]
                      else []
        
        -- Handle different function calls
        (actualFname, setupInstr, cleanup) = case fname of
            "print" -> case args of
                        (arg:_) -> if isStringTemp arg dataSection
                                  then ("print_string", [], restoreStack)
                                  else ("print_int", [], restoreStack)
                        _ -> ("print_string", [], restoreStack)
            "scan" -> ("scan_string",  -- Use scan_string for scan
                      [ MipsSub "$sp" "$sp" "8",     -- Make space for buffer and size
                        MipsLa "$t8" "buffer",      -- Load buffer address
                        MipsLi "$t9" 100,          -- Buffer size
                        MipsSw "$t8" 0 "$sp",      -- Store buffer address
                        MipsSw "$t9" 4 "$sp"       -- Store buffer size
                      ],
                      [MipsAdd "$sp" "$sp" "8"])     -- Cleanup extra space
            other -> (other, [], restoreStack)
            
    in [MipsComment $ "CALL " ++ fname] ++
       (if fname /= "scan" && not (null args) then setupStack else []) ++
       [MipsSw "$ra" (-4) "$sp"] ++
       setupInstr ++
       (if fname /= "scan" then saveArgs else []) ++
       [MipsJal actualFname] ++
       cleanup ++
       [MipsLw "$ra" (-4) "$sp",
        MipsMove (getReg dst) "$v0"]

translateInstrWithData _ instr = translateInstr instr

-- Helper Functions
needsLibraryFunction :: IRInstr -> Bool
needsLibraryFunction (CALL _ fname _) = 
    fname `elem` ["print", "print_string", "print_int", "scan", "scan_string", "scan_int"]
needsLibraryFunction _ = False

getReg :: Temp -> String
getReg temp = 
    if temp `elem` ["$v0", "$a0", "$ra", "$sp", "$fp", "$zero", "$t8", "$t9"]
    then temp
    else case reads (tail temp) :: [(Int, String)] of
         [(n, "")] -> "$t" ++ show (n `mod` 8)
         _ -> temp

-- MIPS Generation
generateMips :: IRProg -> [MipsInstr]
generateMips irProg = 
    let header = [ MipsComment "Program Start"
                , MipsJ "main"
                , MipsLabel "main"
                ]
        dataSection = generateData irProg
        code = concatMap (translateInstrWithData dataSection) irProg
        footer = [ MipsComment "Program End"
                , MipsLi "$v0" 10      -- syscall 10 is exit
                , MipsSyscall
                ]
        needsLibrary = any needsLibraryFunction irProg
    in header ++ code ++ footer ++ (if needsLibrary then generateLibrary else [])

-- String Generation
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
mipsToString (MipsSeq dst src1 src2) = "\tseq " ++ dst ++ ", " ++ src1 ++ ", " ++ src2
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
generateAssembly :: IRProg -> String
generateAssembly irProg =
    unlines $ 
    [".data"] ++
    map mipsToString (generateData irProg) ++
    [".text"] ++ 
    map mipsToString (generateMips irProg)