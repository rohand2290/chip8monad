
module Cpu where

import Data.Word
import Data.Bits
import Data.List.Index (setAt)
import System.Random (StdGen, mkStdGen, randomR)
import Control.Monad.State
import Control.Monad (when)

data CPU = CPU
    {
        -- CHIP-8 memory can access up to 4096 bytes of ram, from 0x000 (0) to 0xFFF (4095).
        memory :: [Word8],

        -- Registers:
        registers :: [Word8],
        iReg :: Word16,
        pc :: Word16,
        sp :: Word8,
        delayTimer :: Word8,
        soundTimer :: Word8,
        stack :: [Word16],

        -- Display and keyboard
        display :: [[Bool]],
        keyboard :: [Bool],

        -- Random number generator
        gen :: StdGen
    }


-- Font sprites for hex digits 0-F, 5 bytes each (4x5 pixels), loaded at 0x000
fontSprites :: [Word8]
fontSprites =
    [ 0xF0, 0x90, 0x90, 0x90, 0xF0  -- 0
    , 0x20, 0x60, 0x20, 0x20, 0x70  -- 1
    , 0xF0, 0x10, 0xF0, 0x80, 0xF0  -- 2
    , 0xF0, 0x10, 0xF0, 0x10, 0xF0  -- 3
    , 0x90, 0x90, 0xF0, 0x10, 0x10  -- 4
    , 0xF0, 0x80, 0xF0, 0x10, 0xF0  -- 5
    , 0xF0, 0x80, 0xF0, 0x90, 0xF0  -- 6
    , 0xF0, 0x10, 0x20, 0x40, 0x40  -- 7
    , 0xF0, 0x90, 0xF0, 0x90, 0xF0  -- 8
    , 0xF0, 0x90, 0xF0, 0x10, 0xF0  -- 9
    , 0xF0, 0x90, 0xF0, 0x90, 0x90  -- A
    , 0xE0, 0x90, 0xE0, 0x90, 0xE0  -- B
    , 0xF0, 0x80, 0x80, 0x80, 0xF0  -- C
    , 0xE0, 0x90, 0x90, 0x90, 0xE0  -- D
    , 0xF0, 0x80, 0xF0, 0x80, 0xF0  -- E
    , 0xF0, 0x80, 0xF0, 0x80, 0x80  -- F
    ]

-- Load rom into memory
loadROM :: [Word8] -> CPU
loadROM rom = CPU
    { memory     = fontSprites ++ replicate (0x200 - length fontSprites) 0 ++ rom ++ replicate (4096 - 0x200 - length rom) 0
    , registers  = replicate 16 0
    , iReg       = 0
    , pc         = 0x200
    , sp         = 0
    , delayTimer = 0
    , soundTimer = 0
    , stack      = replicate 16 0
    , display    = replicate 32 (replicate 64 False)
    , keyboard   = replicate 16 False
    , gen        = mkStdGen 42
    }


-- Helpers

getReg :: CPU -> Word8 -> Word8
getReg cpu idx = registers cpu !! fromIntegral idx

setReg :: CPU -> Word8 -> Word8 -> CPU
setReg cpu idx val = cpu { registers = setAt (fromIntegral idx) val (registers cpu) }


-- Fetch, decode, execute


-- Reads 2 bytes from memory, combines into 16 bit opcode, and increments pc by 2
fetch :: State CPU Word16
fetch = do
    cpu <- get
    let high   = fromIntegral (memory cpu !! fromIntegral (pc cpu)) :: Word16
        low    = fromIntegral (memory cpu !! fromIntegral (pc cpu + 1)) :: Word16
        opcode = (high `shiftL` 8) .|. low
    modify $ \c -> c { pc = pc c + 2 }
    return opcode


-- 00E0: Clear the display
cls :: State CPU ()
cls = modify $ \cpu -> cpu { display = replicate 32 (replicate 64 False) }

-- 00EE: Return from subroutine
ret :: State CPU ()
ret = modify $ \cpu -> cpu { pc = stack cpu !! fromIntegral (sp cpu), sp = sp cpu - 1 }

-- 1NNN: Jump to address NNN
jp :: Word16 -> State CPU ()
jp nnn = modify $ \cpu -> cpu { pc = nnn }

-- 2NNN: Call subroutine at NNN
call :: Word16 -> State CPU ()
call nnn = modify $ \cpu -> cpu
    { sp    = sp cpu + 1
    , stack = setAt (fromIntegral (sp cpu + 1)) (pc cpu) (stack cpu)
    , pc    = nnn
    }

-- 3XKK: Skip next instruction if Vx == KK
seVxByte :: Word8 -> Word8 -> State CPU ()
seVxByte x kk = do
    vx <- gets (`getReg` x)
    when (vx == kk) $ modify $ \cpu -> cpu { pc = pc cpu + 2 }

-- 4XKK: Skip next instruction if Vx /= KK
sneVxByte :: Word8 -> Word8 -> State CPU ()
sneVxByte x kk = do
    vx <- gets (`getReg` x)
    when (vx /= kk) $ modify $ \cpu -> cpu { pc = pc cpu + 2 }

-- 5XY0: Skip next instruction if Vx == Vy
seVxVy :: Word8 -> Word8 -> State CPU ()
seVxVy x y = do
    vx <- gets (`getReg` x)
    vy <- gets (`getReg` y)
    when (vx == vy) $ modify $ \cpu -> cpu { pc = pc cpu + 2 }

-- 6XKK: Set Vx = KK
ldVxByte :: Word8 -> Word8 -> State CPU ()
ldVxByte x kk = modify $ \cpu -> setReg cpu x kk

-- 7XKK: Set Vx = Vx + KK (no carry)
addVxByte :: Word8 -> Word8 -> State CPU ()
addVxByte x kk = modify $ \cpu -> setReg cpu x (getReg cpu x + kk)

-- 8XY0: Set Vx = Vy
ldVxVy :: Word8 -> Word8 -> State CPU ()
ldVxVy x y = modify $ \cpu -> setReg cpu x (getReg cpu y)

-- 8XY1: Set Vx = Vx OR Vy
orVxVy :: Word8 -> Word8 -> State CPU ()
orVxVy x y = modify $ \cpu -> setReg cpu x (getReg cpu x .|. getReg cpu y)

-- 8XY2: Set Vx = Vx AND Vy
andVxVy :: Word8 -> Word8 -> State CPU ()
andVxVy x y = modify $ \cpu -> setReg cpu x (getReg cpu x .&. getReg cpu y)

-- 8XY3: Set Vx = Vx XOR Vy
xorVxVy :: Word8 -> Word8 -> State CPU ()
xorVxVy x y = modify $ \cpu -> setReg cpu x (getReg cpu x `xor` getReg cpu y)

-- 8XY4: Set Vx = Vx + Vy, set VF = carry
addVxVy :: Word8 -> Word8 -> State CPU ()
addVxVy x y = modify $ \cpu ->
    let sum'  = fromIntegral (getReg cpu x) + fromIntegral (getReg cpu y) :: Word16
        carry = if sum' > 0xFF then 1 else 0
    in setReg (setReg cpu x (fromIntegral (sum' .&. 0xFF))) 0xF carry

-- 8XY5: Set Vx = Vx - Vy, set VF = NOT borrow (1 if Vx > Vy)
subVxVy :: Word8 -> Word8 -> State CPU ()
subVxVy x y = modify $ \cpu ->
    let vx       = getReg cpu x
        vy       = getReg cpu y
        noBorrow = if vx > vy then 1 else 0
    in setReg (setReg cpu x (vx - vy)) 0xF noBorrow

-- 8XY6: Set Vx = Vx SHR 1, VF = least significant bit before shift
shrVx :: Word8 -> State CPU ()
shrVx x = modify $ \cpu ->
    let vx  = getReg cpu x
        lsb = vx .&. 0x1
    in setReg (setReg cpu x (vx `shiftR` 1)) 0xF lsb

-- 8XY7: Set Vx = Vy - Vx, set VF = NOT borrow (1 if Vy > Vx)
subnVxVy :: Word8 -> Word8 -> State CPU ()
subnVxVy x y = modify $ \cpu ->
    let vx       = getReg cpu x
        vy       = getReg cpu y
        noBorrow = if vy > vx then 1 else 0
    in setReg (setReg cpu x (vy - vx)) 0xF noBorrow

-- 8XYE: Set Vx = Vx SHL 1, VF = most significant bit before shift
shlVx :: Word8 -> State CPU ()
shlVx x = modify $ \cpu ->
    let vx  = getReg cpu x
        msb = (vx `shiftR` 7) .&. 0x1
    in setReg (setReg cpu x (vx `shiftL` 1)) 0xF msb

-- 9XY0: Skip next instruction if Vx /= Vy
sneVxVy :: Word8 -> Word8 -> State CPU ()
sneVxVy x y = do
    vx <- gets (`getReg` x)
    vy <- gets (`getReg` y)
    when (vx /= vy) $ modify $ \cpu -> cpu { pc = pc cpu + 2 }

-- ANNN: Set I = NNN
ldI :: Word16 -> State CPU ()
ldI nnn = modify $ \cpu -> cpu { iReg = nnn }

-- BNNN: Jump to address NNN + V0
jpV0 :: Word16 -> State CPU ()
jpV0 nnn = modify $ \cpu -> cpu { pc = nnn + fromIntegral (getReg cpu 0) }

-- CXKK: Set Vx = random byte AND KK
rnd :: Word8 -> Word8 -> State CPU ()
rnd x kk = modify $ \cpu ->
    let (randVal, newGen) = randomR (0 :: Int, 255) (gen cpu)
    in (setReg cpu x (fromIntegral randVal .&. kk)) { gen = newGen }

-- DXYN: Draw N-byte sprite at (Vx, Vy), set VF = 1 if collision
drw :: Word8 -> Word8 -> Word8 -> State CPU ()
drw x y n = modify $ \cpu ->
    let vx             = fromIntegral (getReg cpu x) :: Int
        vy             = fromIntegral (getReg cpu y) :: Int
        sprite         = take (fromIntegral n) $ drop (fromIntegral (iReg cpu)) (memory cpu)
        (newDisp, col) = drawSprite vx vy sprite (display cpu)
    in cpu { display = newDisp, registers = setAt 0xF (if col then 1 else 0) (registers cpu) }

drawSprite :: Int -> Int -> [Word8] -> [[Bool]] -> ([[Bool]], Bool)
drawSprite vx vy sprite disp = foldl drawRow (disp, False) (zip [0..] sprite)
  where
    drawRow (d, col) (row, byte) = foldl (drawBit row byte) (d, col) [0..7]
    drawBit row byte (d, col) bit =
        let spriteBit = (byte `shiftR` (7 - bit)) .&. 1 == 1
            px        = (vx + bit) `mod` 64
            py        = (vy + row) `mod` 32
            current   = (d !! py) !! px
            new       = current `xor` spriteBit
            newRow    = setAt px new (d !! py)
            newDisp   = setAt py newRow d
        in (newDisp, col || (current && spriteBit))

-- EX9E: Skip next instruction if key with value Vx is pressed
skp :: Word8 -> State CPU ()
skp x = do
    key     <- gets (`getReg` x)
    pressed <- gets (\cpu -> keyboard cpu !! fromIntegral key)
    when pressed $ modify $ \cpu -> cpu { pc = pc cpu + 2 }

-- EXA1: Skip next instruction if key with value Vx is not pressed
sknp :: Word8 -> State CPU ()
sknp x = do
    key     <- gets (`getReg` x)
    pressed <- gets (\cpu -> keyboard cpu !! fromIntegral key)
    when (not pressed) $ modify $ \cpu -> cpu { pc = pc cpu + 2 }

-- FX07: Set Vx = delay timer value
ldVxDT :: Word8 -> State CPU ()
ldVxDT x = modify $ \cpu -> setReg cpu x (delayTimer cpu)

-- FX0A: Wait for a key press, store its index in Vx
--       If no key is pressed, decrement pc to re-execute this instruction
ldVxK :: Word8 -> State CPU ()
ldVxK x = modify $ \cpu ->
    case filter (\(_, pressed) -> pressed) (zip [0..] (keyboard cpu)) of
        ((key, _) : _) -> setReg cpu x (fromIntegral (key :: Int))
        []             -> cpu { pc = pc cpu - 2 }

-- FX15: Set delay timer = Vx
ldDTVx :: Word8 -> State CPU ()
ldDTVx x = modify $ \cpu -> cpu { delayTimer = getReg cpu x }

-- FX18: Set sound timer = Vx
ldSTVx :: Word8 -> State CPU ()
ldSTVx x = modify $ \cpu -> cpu { soundTimer = getReg cpu x }

-- FX1E: Set I = I + Vx
addIVx :: Word8 -> State CPU ()
addIVx x = modify $ \cpu -> cpu { iReg = iReg cpu + fromIntegral (getReg cpu x) }

-- FX29: Set I = address of font sprite for digit Vx (each sprite is 5 bytes at 0x000)
ldFVx :: Word8 -> State CPU ()
ldFVx x = modify $ \cpu -> cpu { iReg = fromIntegral (getReg cpu x) * 5 }

-- FX33: Store BCD representation of Vx in memory at I, I+1, I+2
ldBVx :: Word8 -> State CPU ()
ldBVx x = modify $ \cpu ->
    let vx       = getReg cpu x
        hundreds = vx `div` 100
        tens     = (vx `div` 10) `mod` 10
        ones     = vx `mod` 10
        addr     = fromIntegral (iReg cpu)
        newMem   = setAt addr hundreds $ setAt (addr + 1) tens $ setAt (addr + 2) ones (memory cpu)
    in cpu { memory = newMem }

-- FX55: Store registers V0 through Vx in memory starting at I
storeRegs :: Word8 -> State CPU ()
storeRegs x = modify $ \cpu ->
    let regsToStore = take (fromIntegral x + 1) (registers cpu)
        addr        = fromIntegral (iReg cpu)
        newMem      = foldl (\mem (i, v) -> setAt (addr + i) v mem) (memory cpu) (zip [0..] regsToStore)
    in cpu { memory = newMem }

-- FX65: Read registers V0 through Vx from memory starting at I
loadRegs :: Word8 -> State CPU ()
loadRegs x = modify $ \cpu ->
    let addr    = fromIntegral (iReg cpu)
        vals    = take (fromIntegral x + 1) $ drop addr (memory cpu)
        newRegs = foldl (\regs (i, v) -> setAt i v regs) (registers cpu) (zip [0..] vals)
    in cpu { registers = newRegs }


data Instruction
    = Cls
    | Ret
    | Jp Word16
    | Call Word16
    | SeVxByte  Word8 Word8
    | SneVxByte Word8 Word8
    | SeVxVy    Word8 Word8
    | LdVxByte  Word8 Word8
    | AddVxByte Word8 Word8
    | LdVxVy    Word8 Word8
    | OrVxVy    Word8 Word8
    | AndVxVy   Word8 Word8
    | XorVxVy   Word8 Word8
    | AddVxVy   Word8 Word8
    | SubVxVy   Word8 Word8
    | ShrVx     Word8
    | SubnVxVy  Word8 Word8
    | ShlVx     Word8
    | SneVxVy   Word8 Word8
    | LdI       Word16
    | JpV0      Word16
    | Rnd       Word8 Word8
    | Drw       Word8 Word8 Word8
    | Skp       Word8
    | Sknp      Word8
    | LdVxDT    Word8
    | LdVxK     Word8
    | LdDTVx    Word8
    | LdSTVx    Word8
    | AddIVx    Word8
    | LdFVx     Word8
    | LdBVx     Word8
    | StoreRegs Word8
    | LoadRegs  Word8
    | Unknown
    deriving (Show)

decode :: Word16 -> Instruction
decode opcode =
    let nnn = opcode .&. 0x0FFF
        x   = fromIntegral ((opcode .&. 0x0F00) `shiftR` 8) :: Word8
        y   = fromIntegral ((opcode .&. 0x00F0) `shiftR` 4) :: Word8
        n   = fromIntegral (opcode .&. 0x000F) :: Word8
        kk  = fromIntegral (opcode .&. 0x00FF) :: Word8
        top = opcode .&. 0xF000
    in case opcode of
        0x00E0                        -> Cls
        0x00EE                        -> Ret
        _ | top == 0x1000             -> Jp nnn
        _ | top == 0x2000             -> Call nnn
        _ | top == 0x3000             -> SeVxByte x kk
        _ | top == 0x4000             -> SneVxByte x kk
        _ | top == 0x5000             -> SeVxVy x y
        _ | top == 0x6000             -> LdVxByte x kk
        _ | top == 0x7000             -> AddVxByte x kk
        _ | top == 0x8000, n == 0x0   -> LdVxVy x y
        _ | top == 0x8000, n == 0x1   -> OrVxVy x y
        _ | top == 0x8000, n == 0x2   -> AndVxVy x y
        _ | top == 0x8000, n == 0x3   -> XorVxVy x y
        _ | top == 0x8000, n == 0x4   -> AddVxVy x y
        _ | top == 0x8000, n == 0x5   -> SubVxVy x y
        _ | top == 0x8000, n == 0x6   -> ShrVx x
        _ | top == 0x8000, n == 0x7   -> SubnVxVy x y
        _ | top == 0x8000, n == 0xE   -> ShlVx x
        _ | top == 0x9000             -> SneVxVy x y
        _ | top == 0xA000             -> LdI nnn
        _ | top == 0xB000             -> JpV0 nnn
        _ | top == 0xC000             -> Rnd x kk
        _ | top == 0xD000             -> Drw x y n
        _ | top == 0xE000, kk == 0x9E -> Skp x
        _ | top == 0xE000, kk == 0xA1 -> Sknp x
        _ | top == 0xF000, kk == 0x07 -> LdVxDT x
        _ | top == 0xF000, kk == 0x0A -> LdVxK x
        _ | top == 0xF000, kk == 0x15 -> LdDTVx x
        _ | top == 0xF000, kk == 0x18 -> LdSTVx x
        _ | top == 0xF000, kk == 0x1E -> AddIVx x
        _ | top == 0xF000, kk == 0x29 -> LdFVx x
        _ | top == 0xF000, kk == 0x33 -> LdBVx x
        _ | top == 0xF000, kk == 0x55 -> StoreRegs x
        _ | top == 0xF000, kk == 0x65 -> LoadRegs x
        _                             -> Unknown

execute :: Instruction -> State CPU ()
execute instr = case instr of
    Cls             -> cls
    Ret             -> ret
    Jp nnn          -> jp nnn
    Call nnn        -> call nnn
    SeVxByte x kk   -> seVxByte x kk
    SneVxByte x kk  -> sneVxByte x kk
    SeVxVy x y      -> seVxVy x y
    LdVxByte x kk   -> ldVxByte x kk
    AddVxByte x kk  -> addVxByte x kk
    LdVxVy x y      -> ldVxVy x y
    OrVxVy x y      -> orVxVy x y
    AndVxVy x y     -> andVxVy x y
    XorVxVy x y     -> xorVxVy x y
    AddVxVy x y     -> addVxVy x y
    SubVxVy x y     -> subVxVy x y
    ShrVx x         -> shrVx x
    SubnVxVy x y    -> subnVxVy x y
    ShlVx x         -> shlVx x
    SneVxVy x y     -> sneVxVy x y
    LdI nnn         -> ldI nnn
    JpV0 nnn        -> jpV0 nnn
    Rnd x kk        -> rnd x kk
    Drw x y n       -> drw x y n
    Skp x           -> skp x
    Sknp x          -> sknp x
    LdVxDT x        -> ldVxDT x
    LdVxK x         -> ldVxK x
    LdDTVx x        -> ldDTVx x
    LdSTVx x        -> ldSTVx x
    AddIVx x        -> addIVx x
    LdFVx x         -> ldFVx x
    LdBVx x         -> ldBVx x
    StoreRegs x     -> storeRegs x
    LoadRegs x      -> loadRegs x
    Unknown         -> return ()


cycle :: State CPU ()
cycle = fetch >>= execute . decode

tickTimers :: State CPU ()
tickTimers = modify $ \cpu -> cpu
    { delayTimer = if delayTimer cpu > 0 then delayTimer cpu - 1 else 0
    , soundTimer = if soundTimer cpu > 0 then soundTimer cpu - 1 else 0
    }
