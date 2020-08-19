{-# LANGUAGE BinaryLiterals #-}

module LC3VM
  ( readVM,
    runVM,
  )
where

import Control.Monad (liftM)
import Data.Binary.Get
  ( Get,
    getWord16be,
    isEmpty,
    runGet,
  )
import Data.Bits
  ( complement,
    popCount,
    setBit,
    shiftL,
    shiftR,
    testBit,
    (.&.),
    (.|.),
  )
import qualified Data.ByteString.Lazy as B
import Data.Char
  ( chr,
    intToDigit,
    ord,
  )
import qualified Data.Vector as V
import Data.Word (Word16)
import Numeric (showIntAtBase)
import System.IO
  ( BufferMode (NoBuffering),
    hFlush,
    hSetBuffering,
    hSetEcho,
    stdin,
    stdout,
  )
import Text.Printf (printf)

type Memory = V.Vector Word16

data Register
  = R0
  | R1
  | R2
  | R3
  | R4
  | R5
  | R6
  | R7
  | PC
  | Cond
  | Count
  deriving (Show)

type Registers = V.Vector Word16

data OPCode
  = BR
  | ADD
  | LD --
  | ST
  | JSR --
  | AND
  | LDR --
  | STR --
  | RTI
  | NOT
  | LDI
  | STI
  | JMP
  | RET
  | LEA --
  | TRAP --

data Instruction
  = Add
      { addR0 :: Register,
        addR1 :: Register,
        addBy :: Either Word16 Register
      }
  | And
      { andR0 :: Register,
        andR1 :: Register,
        andWith :: Either Word16 Register
      }
  | Br
      { brCond :: Word16,
        brPcOffset :: Word16
      }
  | Jmp {jmpR0 :: Register}
  | Ret
  | Jsr {jsrPcOffset :: Word16}
  | Jsrr {jsrrR0 :: Register}
  | Ld
      { ldR0 :: Register,
        ldPcOffset :: Word16
      }
  | Ldi
      { ldiR0 :: Register,
        pcOffset :: Word16
      }
  | Ldr
      { ldiR0 :: Register,
        ldiR1 :: Register,
        offset :: Word16
      }
  | Lea
      { leaR0 :: Register,
        leaPcOffset :: Word16
      }
  | Not
      { notR0 :: Register,
        notR1 :: Register
      }
  | St
      { stR0 :: Register,
        stPcOffset :: Word16
      }
  | Sti
      { stR0 :: Register,
        stPcOffset :: Word16
      }
  | Str
      { strR0 :: Register,
        strR1 :: Register,
        strOffset :: Word16
      }
  | Trap {trapvect8 :: Word16}
  deriving (Show)

intToReg :: Integer -> Register
intToReg = wordToReg . fromIntegral

wordToReg :: Word16 -> Register
wordToReg w = case w of
  0 -> R0
  1 -> R1
  2 -> R2
  3 -> R3
  4 -> R4
  5 -> R5
  6 -> R6
  7 -> R7
  8 -> PC
  9 -> Cond
  10 -> Count

regToWord :: Register -> Word16
regToWord reg = case reg of
  R0 -> 0
  R1 -> 1
  R2 -> 2
  R3 -> 3
  R4 -> 4
  R5 -> 5
  R6 -> 6
  R7 -> 7
  PC -> 8
  Cond -> 9
  Count -> 10

readReg :: Register -> Registers -> Word16
readReg reg rs = rs V.! (fromIntegral $ regToWord reg)

updateReg :: Register -> Word16 -> Registers -> Registers
updateReg reg v rs = rs V.// [(fromIntegral $ regToWord reg, v)]

readMem :: Word16 -> Memory -> Word16
readMem p mem = mem V.! (fromIntegral p)

updateMem :: Word16 -> Word16 -> Memory -> Memory
updateMem p value mem = mem V.// [(fromIntegral p, value)]

opFromInstr :: Word16 -> OPCode
opFromInstr instr =
  let opCode = instr `shiftR` 12
   in case opCode of
        0b0001 -> ADD
        0b0101 -> AND
        0b0000 -> BR
        0b1100 -> JMP
        0b0100 -> JSR
        0b0010 -> LD
        0b1010 -> LDI
        0b0110 -> LDR
        0b1110 -> LEA
        0b1001 -> NOT
        0b0011 -> ST
        0b1011 -> STI
        0b0111 -> STR
        0b1111 -> TRAP
        otherwise -> error $ "Invalid instruction " ++ showWordAsBinary 4 opCode

showWord16AsBinary :: Word16 -> String
showWord16AsBinary = showWordAsBinary 16

showWordAsBinary :: Int -> Word16 -> String
showWordAsBinary n w =
  let s = showIntAtBase 2 intToDigit w ""
      sLen = length s
   in if sLen < n
        then take (n - sLen) (repeat '0') ++ s
        else s

data VM = VM
  { mem :: Memory,
    regs :: Registers
  }
  deriving (Show)

readVM :: FilePath -> IO VM
readVM file = do
  input <- B.readFile file
  let vm = runGet parseVM input
  return vm

parseVM :: Get VM
parseVM = do
  origin <- getWord16be
  words <- readWords
  let regs = updateReg PC 0x3000 $ V.fromList $ take 11 $ repeat 0
      mem = V.fromList ((take (fromIntegral origin) $ repeat 0) ++ words)
  return $ VM (padMem mem $ 2 ^ 16) regs

padMem :: Memory -> Int -> Memory
padMem m count =
  let missing = count - V.length m
   in if missing <= 0
        then m
        else m V.++ (V.fromList $ take missing $ repeat 0)

readWords :: Get [Word16]
readWords = do
  empty <- isEmpty
  if empty
    then return []
    else do
      word <- getWord16be
      words <- readWords
      return $ word : words

runVM :: VM -> IO ()
runVM vm = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  vm' <- stepVM vm
  hFlush stdout
  runVM vm'

stepVM :: VM -> IO VM
stepVM (VM mem regs) = do
  let pc = readReg PC regs
      pc' = pc + 1
      regs' = updateReg PC pc' regs
  (mem', instr) <- memRead mem pc
  -- printf "PC: 0x%x Instruction: %s\n" _pc (show $ parseInstruction instr)
  case parseInstruction instr of
    (Add r0 r1 (Left addBy)) -> do
      let regs'' = updateFlags (updateReg r0 (readReg r1 regs' + addBy) regs') r0
      return $ VM mem' regs''
    (Add r0 r1 (Right r2)) -> do
      let regs'' = updateFlags (updateReg r0 (readReg r1 regs' + readReg r2 regs') regs') r0
      return $ VM mem' regs''
    (And r0 r1 (Left andWith)) -> do
      let regs'' = updateFlags (updateReg r0 (readReg r1 regs' .&. andWith) regs') r0
      return $ VM mem' regs''
    (And r0 r1 (Right r2)) -> do
      let regs'' = updateFlags (updateReg r0 (readReg r1 regs' .&. readReg r2 regs') regs') r0
      return $ VM mem' regs''
    (Br cond pcOffset) ->
      let bits = cond .&. readReg Cond regs'
       in if bits /= 0
            then do
              let pc'' = pc' + pcOffset
                  regs'' = updateReg PC pc'' regs'
              return $ VM mem' regs''
            else return $ VM mem' regs'
    (Jmp r0) -> do
      let regs'' = updateReg PC (readReg r0 regs') regs'
      return $ VM mem' regs''
    Ret -> do
      let pc'' = readReg R7 regs'
          regs'' = updateReg PC pc'' regs'
      return $ VM mem' regs''
    (Jsr pcOffset) -> do
      let pc'' = pc' + pcOffset
          regs'' = updateReg PC pc'' (updateReg R7 pc' regs')
      return $ VM mem' regs''
    (Jsrr r0) -> do
      let regs'' = updateReg R7 pc' regs'
          pc'' = readReg r0 regs'
          regs''' = updateReg PC pc'' regs''
      return $ VM mem' regs'''
    (Ld r0 pcOffset) -> do
      (mem'', value) <- memRead mem' $ pc' + pcOffset
      let regs'' = updateFlags (updateReg r0 value regs') r0
      return $ VM mem'' regs''
    (Ldi r0 pcOffset) -> do
      (mem'', addr) <- memRead mem' (pc' + pcOffset)
      (mem''', value) <- memRead mem'' addr
      let regs'' = updateFlags (updateReg r0 value regs') r0
      return $ VM mem''' regs''
    (Ldr r0 r1 offset) -> do
      (mem'', value) <- memRead mem' $ (readReg r1 regs') + offset
      let regs'' = updateFlags (updateReg r0 value regs') r0
      return $ VM mem'' regs''
    (Lea r0 pcOffset) -> do
      let value = pc' + pcOffset
          regs'' = updateFlags (updateReg r0 value regs') r0
      return $ VM mem' regs''
    (Not r0 r1) -> do
      let value = complement $ readReg r1 regs'
          regs'' = updateFlags (updateReg r0 value regs') r0
      return $ VM mem' regs''
    (St r0 pcOffset) -> do
      let memAddr = pc' + pcOffset
          value = readReg r0 regs'
          mem'' = updateMem memAddr value mem'
      return $ VM mem'' regs'
    (Sti r0 pcOffset) -> do
      let memAddrAddr = pc' + pcOffset
          value = readReg r0 regs'
      (mem'', memAddr) <- memRead mem' memAddrAddr
      let mem''' = updateMem memAddr value mem''
      return $ VM mem''' regs'
    (Str r0 r1 offset) -> do
      let value = readReg r0 regs'
          memAddr = readReg r1 regs' + offset
          mem'' = updateMem memAddr value mem'
      return $ VM mem'' regs'
    t@(Trap vec) -> trap t $ VM mem' regs'

trap :: Instruction -> VM -> IO VM
trap (Trap vec) vm@(VM mem regs)
  | vec == trapGetc = do
    word <- getWord
    let regs' = updateReg R0 word regs
    return $ VM mem regs'
  | vec == trapOut = do
    putChar $ chr . fromIntegral $ (readReg R0 regs .&. oneBits 8)
    return vm
  | vec == trapIn = do
    putStr "Please input a character: "
    word <- getWord
    let regs' = updateReg R0 word regs
    return $ VM mem regs'
  | vec == trapPuts = do
    mem' <- putChars mem $ readReg R0 regs
    return $ VM mem' regs
  | vec == trapPutsp = do
    mem' <- putChars mem $ readReg R0 regs
    return $ VM mem' regs
  | vec == trapHalt = error "HALT"

putChars :: Memory -> Word16 -> IO (Memory)
putChars mem addr = do
  (mem', word) <- memRead mem addr
  if word == 0
    then return mem'
    else do
      putChar $ chr . fromIntegral $ word
      putChars mem' $ addr + 1

putCharsP :: Memory -> Word16 -> IO (Memory)
putCharsP mem addr = do
  (mem', word) <- memRead mem addr
  if word == 0
    then return mem'
    else do
      (mem'', word) <- memRead mem' addr
      putChar $ chr . fromIntegral $ wordRange 0 8 word
      putChar $ chr . fromIntegral $ wordRange 8 8 word
      putCharsP mem'' $ addr + 1

wordRange :: Int -> Int -> Word16 -> Word16
wordRange offset len w = w `shiftR` offset .&. oneBits len

signExtend :: Int -> Word16 -> Word16
signExtend count bits =
  if testBit (bits `shiftR` (count - 1)) 0
    then bits .|. (0xFFFF `shiftL` count)
    else bits

flPos, flZro, flNeg :: Word16
flPos = 1
flZro = 2
flNeg = 4

updateFlags :: Registers -> Register -> Registers
updateFlags regs r =
  updateReg Cond flag regs
  where
    value = readReg r regs
    flag
      | value == 0 = flZro
      | testBit (value `shiftR` 15) 0 = flNeg
      | otherwise = flPos

parseInstruction :: Word16 -> Instruction
parseInstruction instr =
  let op = opFromInstr instr
      imm5Flag = testBit (readWordFromInstr 5 1 instr) 0
   in case op of
        ADD -> Add r0 r1 addBy
          where
            r0 = r0FromInstr instr
            r1 = r1FromInstr instr
            addBy =
              if imm5Flag
                then Left $ imm5FromInstr instr
                else Right $ wordToReg $ readWordFromInstr 0 3 instr
        AND -> And r0 r1 andWith
          where
            r0 = r0FromInstr instr
            r1 = r1FromInstr instr
            andWith =
              if imm5Flag
                then Left $ imm5FromInstr instr
                else Right $ wordToReg $ readWordFromInstr 0 3 instr
        BR -> Br cond offset
          where
            cond = readWordFromInstr 9 3 instr
            offset = tailSignExtendedFromInstr 9 instr
        JMP -> Jmp $ r1FromInstr instr
        JSR ->
          let flag = testBit (readWordFromInstr 11 1 instr) 0
           in if flag
                then Jsr $ tailSignExtendedFromInstr 11 instr
                else Jsrr $ r1FromInstr instr
        LD -> Ld r0 offset
          where
            r0 = r0FromInstr instr
            offset = tailSignExtendedFromInstr 9 instr
        LDI -> Ldi r0 offset
          where
            r0 = r0FromInstr instr
            offset = imm9FromInstr instr
        LDR -> Ldr r0 r1 offset
          where
            r0 = r0FromInstr instr
            r1 = r1FromInstr instr
            offset = tailSignExtendedFromInstr 6 instr
        LEA -> Lea r0 offset
          where
            r0 = r0FromInstr instr
            offset = tailSignExtendedFromInstr 9 instr
        NOT -> Not r0 r1
          where
            r0 = r0FromInstr instr
            r1 = r1FromInstr instr
        ST -> St r0 offset
          where
            r0 = r0FromInstr instr
            offset = tailSignExtendedFromInstr 9 instr
        STI -> Sti r0 offset
          where
            r0 = r0FromInstr instr
            offset = tailSignExtendedFromInstr 9 instr
        STR -> Str r0 r1 offset
          where
            r0 = r0FromInstr instr
            r1 = r1FromInstr instr
            offset = tailSignExtendedFromInstr 6 instr
        TRAP -> Trap $ readWordFromInstr 0 8 instr

mrKBSR, mrKBDR :: Word16
mrKBSR = 0xFE00
mrKBDR = 0xFE02

memRead :: Memory -> Word16 -> IO (Memory, Word16)
memRead m addr
  | addr == mrKBSR = do
    key <- checkKey
    case key of
      Nothing -> return ((updateMem mrKBSR 0 m), 0)
      Just w ->
        return ((updateMem mrKBDR w $ updateMem mrKBSR (1 `shiftL` 15) m), (1 `shiftL` 15))
  | otherwise = return $ (m, readMem addr m)

checkKey :: IO (Maybe Word16)
checkKey = do
  result <- B.hGetNonBlocking stdin 1
  if B.null result
    then return Nothing
    else do
      let [l] = B.unpack result
      return $ Just $ fromIntegral l

getWord :: IO Word16
getWord = liftM (fromIntegral . ord) getChar

readWordFromInstr :: Int -> Int -> Word16 -> Word16
readWordFromInstr start length instr =
  (instr `shiftR` start) .&. oneBits length

r0FromInstr :: Word16 -> Register
r0FromInstr instr = wordToReg $ readWordFromInstr 9 3 instr

r1FromInstr :: Word16 -> Register
r1FromInstr instr = wordToReg $ readWordFromInstr 6 3 instr

signExtendedFromInstr :: Int -> Int -> Word16 -> Word16
signExtendedFromInstr offset len instr = signExtend len $ readWordFromInstr offset len instr

tailSignExtendedFromInstr :: Int -> Word16 -> Word16
tailSignExtendedFromInstr = signExtendedFromInstr 0

immFromInstr :: Int -> Word16 -> Word16
immFromInstr = tailSignExtendedFromInstr

imm5FromInstr :: Word16 -> Word16
imm5FromInstr = immFromInstr 5

imm9FromInstr :: Word16 -> Word16
imm9FromInstr = immFromInstr 9

trapGetc, trapOut, trapPuts, trapIn, trapPutsp, trapHalt :: Word16
trapGetc = 0x20
trapOut = 0x21
trapPuts = 0x22
trapIn = 0x23
trapPutsp = 0x24
trapHalt = 0x25

oneBits :: Int -> Word16
oneBits len = complement $ (complement 0) `shiftL` len
