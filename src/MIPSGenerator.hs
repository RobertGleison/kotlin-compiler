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
    = MipsLabel String                     
    | MipsLi String Int                    
    | MipsMove String String               
    | MipsAdd String String String         
    | MipsSub String String String         
    | MipsMul String String String         
    | MipsDiv String String                
    | MipsAnd String String String         
    | MipsOr String String String          
    | MipsXor String String String         
    | MipsLt String String String          
    | MipsMflo String                      
    | MipsLw String Int String             
    | MipsSw String Int String             
    | MipsJ String                         
    | MipsSeq String String String         
    | MipsBne String String String         
    | MipsBeq String String String         
    | MipsBlt String String String         
    | MipsBgt String String String         
    | MipsJal String                       
    | MipsJr String                        
    | MipsComment String                   
    | MipsMfhi String                      
    | MipsSyscall                          
    | MipsLa String String                 
    | MipsDataString String String         
    | MipsDataSpace String Int             
    deriving Show

data CodeGenState = CodeGenState 
    { nextReg :: Int               
    , regMap :: Map.Map Temp String 
    }

initialState :: CodeGenState
initialState = CodeGenState 0 Map.empty

generateData :: IRProg -> [MipsInstr]
generateData prog =
    [ MipsDataString "newline" "\"\\n\"" 
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

generateLibrary :: [MipsInstr]
generateLibrary = 
    [ MipsLabel "print_string"
    , MipsLw "$a0" 0 "$sp"
    , MipsLi "$v0" 4
    , MipsSyscall
    , MipsMove "$v0" "$zero"
    , MipsJr "$ra"
    
    , MipsLabel "print_int"
    , MipsLw "$a0" 0 "$sp"
    , MipsLi "$v0" 1
    , MipsSyscall
    , MipsMove "$v0" "$zero"
    , MipsJr "$ra"
    
    , MipsLabel "scan_int"
    , MipsLi "$v0" 5
    , MipsSyscall
    , MipsJr "$ra"
    ]

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
        Eq  -> [MipsSeq (getReg dst) (getReg src1) (getReg src2)]
        Neq -> [MipsSeq (getReg dst) (getReg src1) (getReg src2),
                MipsXor (getReg dst) (getReg dst) "1"]
        Lt  -> [MipsLt (getReg dst) (getReg src1) (getReg src2)]
        Gt  -> [MipsLt (getReg dst) (getReg src2) (getReg src1)]
        Gte -> [MipsLt (getReg dst) (getReg src1) (getReg src2),
                MipsXor (getReg dst) (getReg dst) "1"]
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
    escapeChar '$' = "\\$"
    escapeChar c = [c]

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
        
        (actualFname, setupInstr, cleanup) = case fname of
            "print" -> case args of
                        (arg:_) -> if isStringTemp arg dataSection
                                  then ("print_string", [], restoreStack)
                                  else ("print_int", [], restoreStack)
                        _ -> ("print_string", [], restoreStack)
            "scan" -> ("scan_int", [], [])  -- Changed to only handle integers
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

needsLibraryFunction :: IRInstr -> Bool
needsLibraryFunction (CALL _ fname _) = 
    fname `elem` ["print", "print_string", "print_int", "scan", "scan_int"]
needsLibraryFunction _ = False

getReg :: Temp -> String
getReg temp = 
    if temp `elem` ["$v0", "$a0", "$ra", "$sp", "$fp", "$zero", "$t8", "$t9"]
    then temp
    else case reads (tail temp) :: [(Int, String)] of
         [(n, "")] -> "$t" ++ show (n `mod` 8)
         _ -> temp

generateMips :: IRProg -> [MipsInstr]
generateMips irProg = 
    let header = [ MipsComment "Program Start"
                , MipsJ "main"
                , MipsLabel "main"
                ]
        dataSection = generateData irProg
        code = concatMap (translateInstrWithData dataSection) irProg
        footer = [ MipsComment "Program End"
                , MipsLi "$v0" 10
                , MipsSyscall
                ]
        needsLibrary = any needsLibraryFunction irProg
    in header ++ code ++ footer ++ (if needsLibrary then generateLibrary else [])

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

generateAssembly :: IRProg -> String
generateAssembly irProg =
    unlines $ 
    [".data"] ++
    map mipsToString (generateData irProg) ++
    [".text"] ++ 
    map mipsToString (generateMips irProg)