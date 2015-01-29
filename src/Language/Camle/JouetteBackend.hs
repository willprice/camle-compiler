{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module Language.Camle.JouetteBackend where

-- Need to use same registers for variables otherwise fucks up while loops

import qualified Data.Map as M
import Data.List
import Data.Char
import Data.Generics
import Control.Monad.State

import qualified Language.Camle.Data.ThreeAddressCode as TAC
import Language.Camle.Data.JouetteAssembly

type MemoryTable = M.Map (Either String TAC.Var) Address
type RegisterTable = M.Map TAC.Temp Register
type LabelTable = M.Map TAC.Label Label
type VariableTable = M.Map TAC.Var TAC.Temp

newtype Code = Code [Instruction]
               deriving (Eq)
instance Show Code where
        show (Code []) = ""
        show (Code (instr:rest)) = show instr ++ "\n" ++ show (Code rest)

data TranslatorState = Translator { nextLabel :: Label
                                  , nextReg :: Register
                                  , nextFreeAddress :: Address
                                  , memory :: Memory
                                  , memoryTable :: MemoryTable
                                  , registerTable :: RegisterTable
                                  , variableTable :: VariableTable
                                  , labelTable :: LabelTable
                                  , code :: Code 
                                  }

type ASMGen = State TranslatorState

class Translateable src target where
        translate :: src -> ASMGen target

initialState :: TranslatorState
initialState = Translator { nextLabel = Label 0
                                    -- | Register 0 will contain the constant
                                    -- 1
                                    , nextReg = Register 1
                                    , nextFreeAddress = Address 0
                                    , memory = []
                                    , memoryTable = M.empty 
                                    , registerTable = M.empty
                                    , variableTable = M.empty
                                    , labelTable = M.empty
                                    , code = Code []
                                    }

-- |Top level compiler function, runs the compiler monad from the initial
-- state and pulls out the corresponding ASM code.
irToAsm :: TAC.Program -> Code
irToAsm program = fst $ runState (compile program) initialState

-- |Takes a three address code program and generates the corresponding ASM
-- code for a Jouette machine.
compile :: TAC.Program -> ASMGen Code
compile program = do
        initialiseReg0
        initialiseMemory program
        translate program :: ASMGen ()
        putInstruction HALT
        gets code

instance Translateable TAC.Program () where
        translate [] = return ()
        translate code@(instr:rest) = do
            translate instr :: ASMGen ()
            translate rest

instance Translateable TAC.Temp Register where
        translate = fetchOrAssignRegister

instance Translateable TAC.Label Label where
        translate = fetchOrAssignLabel

instance Translateable TAC.TAC () where 
        translate (TAC.AssignI temp n) = do
            result <- translate temp
            putInstruction $ ADDI result zeroReg n
        translate (TAC.AssignT to from) = do
            to' <- translate to
            from' <- translate from
            putInstruction $ ADD to' from' zeroReg
        translate (TAC.AssignV var temp) = do
            addr <- allocateVariable var
            assignVariableTemp var temp
            valReg <- translate temp
            addrReg <- mkTemp 
            loadConstant addrReg $ addressToInteger addr
            -- Initialise with 0
            putComment $ "Assign " ++ show var
            putInstruction $ STORE valReg addrReg 0
        translate (TAC.BinOp op result t1 t2) = do
            src1 <- translate t1
            src2 <- translate t2
            trgt <- translate result
            case op of 
                TAC.BOpAdd -> putInstruction $ ADD trgt src1 src2
                TAC.BOpSub -> putInstruction $ SUB trgt src1 src2
                TAC.BOpMul -> putInstruction $ MUL trgt src1 src2

                -- Input: t1 <= t2
                -- t2 >= t1
                -- t2 - t1 >= 0
                TAC.BOpLTE -> do
                    trueLabel <- mkLabel
                    endLabel <- mkLabel
                    comp <- mkTemp
                    putInstruction $ SUB comp src2 src1
                    putInstruction $ BGEZ comp trueLabel
                    putInstruction $ ADDI trgt zeroReg false
                    putInstruction $ JUMP endLabel
                    putLabel trueLabel
                    putInstruction $ ADDI trgt zeroReg true
                    putLabel endLabel

                -- Input: src1 = src2
                -- src1 - src2 = 0
                TAC.BOpEq -> do
                    trueLabel <- mkLabel
                    endLabel <- mkLabel
                    comp <- mkTemp

                    putInstruction $ SUB comp src2 src1
                    putInstruction $ BEQZ comp trueLabel
                    putInstruction $ ADDI trgt zeroReg false
                    putInstruction $ JUMP endLabel
                    putLabel trueLabel
                    putInstruction $ ADDI trgt zeroReg true
                    putLabel endLabel

                TAC.BOpAnd -> do
                    trueLabel <- mkLabel
                    endLabel <- mkLabel
                    comp <- mkTemp

                    putInstruction $ MUL comp src2 src1
                    putInstruction $ BNEZ comp trueLabel
                    putInstruction $ ADDI trgt zeroReg false
                    putInstruction $ JUMP endLabel
                    putLabel trueLabel
                    putInstruction $ ADDI trgt zeroReg true
                    putLabel endLabel

        translate (TAC.UnaryOp TAC.UOpMinus result temp) = do
            resultReg <- translate result
            argReg <- translate temp
            putInstruction $ SUB resultReg zeroReg argReg

        translate (TAC.UnaryOp TAC.UOpNegate result temp) = do
            setTrue <- mkLabel
            end <- mkLabel

            resultReg <- translate result
            argReg <- translate temp
            putInstruction $ BEQZ argReg setTrue
            loadConstant resultReg 0
            putInstruction $ JUMP end
            putLabel setTrue
            loadConstant resultReg 1
            putLabel end

        translate (TAC.Goto label) = do
            l <- fetchOrAssignLabel label
            putInstruction $ JUMP l
        translate (TAC.WriteI temp) = do
            reg <- translate temp
            putInstruction $ WR reg
        translate (TAC.WriteB bool) = do
            falseLabel <- mkLabel
            endLabel <- mkLabel 
            trueAddr <- fetchAddressOfString "true"
            falseAddr <- fetchAddressOfString "false"
            reg <- translate bool

            putInstruction $ BEQZ reg falseLabel
            putInstruction $ WRS trueAddr
            putInstruction $ JUMP endLabel
            putLabel falseLabel
            putInstruction $ WRS falseAddr
            putLabel endLabel
        translate (TAC.WriteS string) = do
            addr <- fetchAddressOfString string
            putInstruction $ WRS addr
        -- If the condition is true, then do nothing, otherwise jump to the
        -- label
        translate (TAC.Read temp) = do
            reg <- translate temp
            putInstruction $ RD reg
        translate (TAC.IfNot conditionResult label) = do
            ifFalse <- translate label
            cond <- translate conditionResult
            putInstruction $ BEQZ cond ifFalse
        translate (TAC.L label) = do
            l <- fetchOrAssignLabel label
            putLabel l
        translate (TAC.Noop) = putInstruction NOP
        translate tac = fail $ "Could not translate: " ++ show tac

translateArithmeticBinOp op = case op of
                        TAC.BOpAdd -> ADD
                        TAC.BOpSub -> SUB
                        TAC.BOpMul -> MUL
                        _ -> error "expected Binary operation"

-- GENERAL TRANSLATOR FUNCTIONS
mkTemp :: ASMGen Register
mkTemp = do
        reg <- getNextReg
        incrementRegister
        return reg

mkLabel :: ASMGen Label
mkLabel = do
        label <- getNextLabel
        incrementLabel
        return label

putComment :: String -> ASMGen ()
putComment comment = modifyCode (\(Code instructions) -> 
                                    Code $ instructions ++ [Comment comment])
putInstruction :: Instruction -> ASMGen ()
putInstruction instr = modifyCode (\(Code instructions) -> 
                                    Code $ instructions ++ [instr])

putLabel :: Label -> ASMGen ()
putLabel l = putInstruction $ LBL l

{- | If the symbol has already got an assigned register, then
-    that is returned, otherwise a register is allocated and 
-    assigned to the symbol
-}
fetchOrAssignRegister :: TAC.Temp -> ASMGen Register
fetchOrAssignRegister temp = do
        regTable <- gets registerTable
        case (M.lookup temp regTable) of
            Nothing -> do
                reg <- getNextReg
                incrementRegister
                modifyRegisterTable $ M.insert temp reg
                return reg
            (Just reg) -> return reg
fetchAddressOfVariable :: TAC.Var -> ASMGen Integer
fetchAddressOfVariable var = do
        memTable <- gets memoryTable
        case (M.lookup (Right var) memTable) of
            Nothing -> fail "Expected to find variable in memory"
            (Just addr) -> return . addressToInteger $ addr

fetchAddressOfString :: String -> ASMGen Integer
fetchAddressOfString str = do
        memTable <- gets memoryTable
        case (M.lookup (Left str) memTable) of
            Nothing -> fail "Expected to find string in memory"
            (Just addr) -> return . addressToInteger $ addr
fetchOrAssignLabel :: TAC.Label -> ASMGen Label
fetchOrAssignLabel l = do
        labels <- gets labelTable
        case (M.lookup l labels) of
            Nothing -> do
                l' <- mkLabel
                modifyLabelTable $ M.insert l l'
                return l'
            (Just l') -> return l'

allocateVariable :: TAC.Var -> ASMGen Address
allocateVariable var = do
        memTbl <- gets memoryTable
        case (M.lookup (Right var) memTbl) of
            Nothing -> do
                allocateSpaceForVariable
                addr <- getNextAddress
                incrementAddress
                return addr
            (Just addr) -> return addr

assignVariableTemp :: TAC.Var -> TAC.Temp -> ASMGen ()
assignVariableTemp var temp = do
        tbl <- gets variableTable
        modifyVariableTable $ M.insert var temp

allocateSpaceForVariable = do
        putInstruction $ DATA $ Byte 0
        putInstruction $ DATA $ Byte 0
        putInstruction $ DATA $ Byte 0
        putInstruction $ DATA $ Byte 0

getVariableRegister :: TAC.Var -> ASMGen Register
getVariableRegister var = do
        vars <- gets variableTable
        case (M.lookup var vars) of
            Nothing -> fail "Expected variable to be assigned to temp var"
            (Just temp) -> fetchOrAssignRegister temp



getNextAddress :: ASMGen Address
getNextAddress = gets nextFreeAddress

getNextReg :: ASMGen Register
getNextReg = gets nextReg

getNextLabel :: ASMGen Label
getNextLabel = gets nextLabel

-- MODIFY FIELD FUNCTIONS
modifyMemory :: (Memory -> Memory) -> ASMGen ()
modifyMemory fn = modify $ \s -> s { memory = fn $ memory s }

modifyMemoryTable :: (MemoryTable -> MemoryTable) -> ASMGen ()
modifyMemoryTable fn = modify $ \s -> s { memoryTable = fn $ memoryTable s }

modifyRegisterTable :: (RegisterTable -> RegisterTable) -> ASMGen ()
modifyRegisterTable fn = modify $ \s -> s { registerTable = fn $ registerTable s }

modifyLabelTable :: (LabelTable -> LabelTable) -> ASMGen ()
modifyLabelTable fn = modify $ \s -> s { labelTable = fn $ labelTable s }

modifyVariableTable :: (VariableTable -> VariableTable) -> ASMGen ()
modifyVariableTable fn = modify $ \s -> s { variableTable = fn $ variableTable s }

modifyCode :: (Code -> Code) -> ASMGen ()
modifyCode fn = modify $ \s -> s { code = fn $ code s }
--

incrementLabel :: ASMGen ()
incrementLabel = modify increment
    where increment s@Translator { nextLabel = (Label n) } =  s { nextLabel = Label (n + 1) }

incrementRegister :: ASMGen ()
incrementRegister = modify increment
    where increment s@Translator { nextReg = (Register n) } = s { nextReg = Register (n + 1) }

incrementAddress :: ASMGen ()
incrementAddress = modify increment
    where increment s@Translator { nextFreeAddress = (Address n) } = s { nextFreeAddress = Address (n + 4) }

updateAddress :: ASMGen ()
updateAddress = modify update
    where update s@Translator { nextFreeAddress = _
                              , memory = mem }
                 = s { nextFreeAddress = Address $ toInteger $ length mem }

loadConstant :: Register -> Integer -> ASMGen ()
loadConstant reg n = putInstruction $ ADDI reg zeroReg n

-- INITIALISATION FUNCTIONS
initialiseMemory :: TAC.Program -> ASMGen ()
initialiseMemory program = do
        let strings = ["false", "true"] ++ getStrings program
        mapM allocateString strings
        mem <- gets memory
        mapM (putInstruction . DATA) mem
        return ()

initialiseReg0 :: ASMGen ()
initialiseReg0 = putInstruction $ XOR r0 r0 r0
    where r0 = Register 0

-- STRING FUNCTIONS

getStrings :: TAC.Program -> [String]
getStrings = strings . nub . listify isString
    where flatten (TAC.WriteS s) = s
          strings = map flatten
isString :: TAC.TAC -> Bool
isString (TAC.WriteS _) = True
isString _ = False

allocateString :: String -> ASMGen Address
allocateString str = do
        addr <- insertStringIntoMemory str
        insertStringIntoMemoryTable str addr
        return addr

addressToInteger :: Address -> Integer
addressToInteger (Address n) = n

insertStringIntoMemory :: String -> ASMGen Address
insertStringIntoMemory str = do
        addr <- getNextAddress
        modifyMemory (\m -> m ++ (translateString str))
        updateAddress
        return addr

insertStringIntoMemoryTable :: String -> Address -> ASMGen ()
insertStringIntoMemoryTable str address = modifyMemoryTable $ M.insert (Left str) address

insertVariableIntoMemoryTable :: TAC.Var -> Address -> ASMGen ()
insertVariableIntoMemoryTable var address = modifyMemoryTable $ M.insert (Right var) address

translateString :: String -> [Byte]
translateString = padStringToWordLength . appendStopByte . stringToByteString

stringToByteString :: String -> [Byte]
stringToByteString = map charToByte

charToByte :: Char -> Byte
charToByte = Byte . toInteger . ord

appendStopByte :: [Byte] -> [Byte]
appendStopByte str = str ++ [Byte 0]

padStringToWordLength :: [Byte] -> [Byte]
padStringToWordLength byteStr = byteStr ++ (map Byte $ take (wordLength - num) [0,0..])
    where num = (length byteStr + 4) `mod` wordLength
