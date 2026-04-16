
module Cpu where

import Data.Word
import Data.Bits
import Data.List.Index (setAt)
import System.Random (StdGen, mkStdGen, randomR)

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
fetch :: CPU -> (Word16, CPU)
fetch prevCpu = (opcode, nextCpu) where
    highByte = fromIntegral (memory prevCpu !! fromIntegral (pc prevCpu)) :: Word16
    lowByte  = fromIntegral (memory prevCpu !! fromIntegral (pc prevCpu + 1)) :: Word16
    opcode   = (highByte `shiftL` 8) .|. lowByte
    nextCpu  = prevCpu { pc = pc prevCpu + 2 }


-- 00E0: Clear the display
cls :: CPU -> CPU
cls cpu = cpu { display = replicate 32 (replicate 64 False) }

-- 00EE: Return from subroutine
ret :: CPU -> CPU
ret cpu = cpu { pc = stack cpu !! fromIntegral (sp cpu), sp = sp cpu - 1 }

-- 1NNN: Jump to address NNN
jp :: Word16 -> CPU -> CPU
jp nnn cpu = cpu { pc = nnn }

-- 2NNN: Call subroutine at NNN
call :: Word16 -> CPU -> CPU
call nnn cpu = cpu
    { sp    = sp cpu + 1
    , stack = setAt (fromIntegral (sp cpu + 1)) (pc cpu) (stack cpu)
    , pc    = nnn
    }

-- 3XKK: Skip next instruction if Vx == KK
seVxByte :: Word8 -> Word8 -> CPU -> CPU
seVxByte x kk cpu
    | getReg cpu x == kk = cpu { pc = pc cpu + 2 }
    | otherwise          = cpu

-- 4XKK: Skip next instruction if Vx /= KK
sneVxByte :: Word8 -> Word8 -> CPU -> CPU
sneVxByte x kk cpu
    | getReg cpu x /= kk = cpu { pc = pc cpu + 2 }
    | otherwise          = cpu

-- 5XY0: Skip next instruction if Vx == Vy
seVxVy :: Word8 -> Word8 -> CPU -> CPU
seVxVy x y cpu
    | getReg cpu x == getReg cpu y = cpu { pc = pc cpu + 2 }
    | otherwise                    = cpu

-- 6XKK: Set Vx = KK
ldVxByte :: Word8 -> Word8 -> CPU -> CPU
ldVxByte x kk cpu = setReg cpu x kk

-- 7XKK: Set Vx = Vx + KK (no carry)
addVxByte :: Word8 -> Word8 -> CPU -> CPU
addVxByte x kk cpu = setReg cpu x (getReg cpu x + kk)

-- 8XY0: Set Vx = Vy
ldVxVy :: Word8 -> Word8 -> CPU -> CPU
ldVxVy x y cpu = setReg cpu x (getReg cpu y)

-- 8XY1: Set Vx = Vx OR Vy
orVxVy :: Word8 -> Word8 -> CPU -> CPU
orVxVy x y cpu = setReg cpu x (getReg cpu x .|. getReg cpu y)

-- 8XY2: Set Vx = Vx AND Vy
andVxVy :: Word8 -> Word8 -> CPU -> CPU
andVxVy x y cpu = setReg cpu x (getReg cpu x .&. getReg cpu y)

-- 8XY3: Set Vx = Vx XOR Vy
xorVxVy :: Word8 -> Word8 -> CPU -> CPU
xorVxVy x y cpu = setReg cpu x (getReg cpu x `xor` getReg cpu y)

-- 8XY4: Set Vx = Vx + Vy, set VF = carry
addVxVy :: Word8 -> Word8 -> CPU -> CPU
addVxVy x y cpu =
    let sum'  = fromIntegral (getReg cpu x) + fromIntegral (getReg cpu y) :: Word16
        carry = if sum' > 0xFF then 1 else 0
    in setReg (setReg cpu x (fromIntegral (sum' .&. 0xFF))) 0xF carry

-- 8XY5: Set Vx = Vx - Vy, set VF = NOT borrow (1 if Vx > Vy)
subVxVy :: Word8 -> Word8 -> CPU -> CPU
subVxVy x y cpu =
    let vx     = getReg cpu x
        vy     = getReg cpu y
        noBorrow = if vx > vy then 1 else 0
    in setReg (setReg cpu x (vx - vy)) 0xF noBorrow

-- 8XY6: Set Vx = Vx SHR 1, VF = least significant bit before shift
shrVx :: Word8 -> CPU -> CPU
shrVx x cpu =
    let vx  = getReg cpu x
        lsb = vx .&. 0x1
    in setReg (setReg cpu x (vx `shiftR` 1)) 0xF lsb

-- 8XY7: Set Vx = Vy - Vx, set VF = NOT borrow (1 if Vy > Vx)
subnVxVy :: Word8 -> Word8 -> CPU -> CPU
subnVxVy x y cpu =
    let vx       = getReg cpu x
        vy       = getReg cpu y
        noBorrow = if vy > vx then 1 else 0
    in setReg (setReg cpu x (vy - vx)) 0xF noBorrow

-- 8XYE: Set Vx = Vx SHL 1, VF = most significant bit before shift
shlVx :: Word8 -> CPU -> CPU
shlVx x cpu =
    let vx  = getReg cpu x
        msb = (vx `shiftR` 7) .&. 0x1
    in setReg (setReg cpu x (vx `shiftL` 1)) 0xF msb

-- 9XY0: Skip next instruction if Vx /= Vy
sneVxVy :: Word8 -> Word8 -> CPU -> CPU
sneVxVy x y cpu
    | getReg cpu x /= getReg cpu y = cpu { pc = pc cpu + 2 }
    | otherwise                    = cpu

-- ANNN: Set I = NNN
ldI :: Word16 -> CPU -> CPU
ldI nnn cpu = cpu { iReg = nnn }

-- BNNN: Jump to address NNN + V0
jpV0 :: Word16 -> CPU -> CPU
jpV0 nnn cpu = cpu { pc = nnn + fromIntegral (getReg cpu 0) }

-- CXKK: Set Vx = random byte AND KK
rnd :: Word8 -> Word8 -> CPU -> CPU
rnd x kk cpu =
    let (randVal, newGen) = randomR (0 :: Int, 255) (gen cpu)
    in (setReg cpu x (fromIntegral randVal .&. kk)) { gen = newGen }

-- DXYN: Draw N-byte sprite at (Vx, Vy), set VF = 1 if collision
drw :: Word8 -> Word8 -> Word8 -> CPU -> CPU
drw x y n cpu =
    let vx              = fromIntegral (getReg cpu x) :: Int
        vy              = fromIntegral (getReg cpu y) :: Int
        sprite          = take (fromIntegral n) $ drop (fromIntegral (iReg cpu)) (memory cpu)
        (newDisp, col)  = drawSprite vx vy sprite (display cpu)
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
skp :: Word8 -> CPU -> CPU
skp x cpu
    | keyboard cpu !! fromIntegral (getReg cpu x) = cpu { pc = pc cpu + 2 }
    | otherwise                                    = cpu

-- EXA1: Skip next instruction if key with value Vx is not pressed
sknp :: Word8 -> CPU -> CPU
sknp x cpu
    | not (keyboard cpu !! fromIntegral (getReg cpu x)) = cpu { pc = pc cpu + 2 }
    | otherwise                                          = cpu

-- FX07: Set Vx = delay timer value
ldVxDT :: Word8 -> CPU -> CPU
ldVxDT x cpu = setReg cpu x (delayTimer cpu)

-- FX0A: Wait for a key press, store its index in Vx
--       If no key is pressed, decrement pc to re-execute this instruction
ldVxK :: Word8 -> CPU -> CPU
ldVxK x cpu =
    case filter (\(_, pressed) -> pressed) (zip [0..] (keyboard cpu)) of
        ((key, _) : _) -> setReg cpu x (fromIntegral (key :: Int))
        []             -> cpu { pc = pc cpu - 2 }

-- FX15: Set delay timer = Vx
ldDTVx :: Word8 -> CPU -> CPU
ldDTVx x cpu = cpu { delayTimer = getReg cpu x }

-- FX18: Set sound timer = Vx
ldSTVx :: Word8 -> CPU -> CPU
ldSTVx x cpu = cpu { soundTimer = getReg cpu x }

-- FX1E: Set I = I + Vx
addIVx :: Word8 -> CPU -> CPU
addIVx x cpu = cpu { iReg = iReg cpu + fromIntegral (getReg cpu x) }

-- FX29: Set I = address of font sprite for digit Vx (each sprite is 5 bytes at 0x000)
ldFVx :: Word8 -> CPU -> CPU
ldFVx x cpu = cpu { iReg = fromIntegral (getReg cpu x) * 5 }

-- FX33: Store BCD representation of Vx in memory at I, I+1, I+2
ldBVx :: Word8 -> CPU -> CPU
ldBVx x cpu =
    let vx       = getReg cpu x
        hundreds = vx `div` 100
        tens     = (vx `div` 10) `mod` 10
        ones     = vx `mod` 10
        addr     = fromIntegral (iReg cpu)
        newMem   = setAt addr hundreds $ setAt (addr + 1) tens $ setAt (addr + 2) ones (memory cpu)
    in cpu { memory = newMem }

-- FX55: Store registers V0 through Vx in memory starting at I
storeRegs :: Word8 -> CPU -> CPU
storeRegs x cpu =
    let regsToStore = take (fromIntegral x + 1) (registers cpu)
        addr        = fromIntegral (iReg cpu)
        newMem      = foldl (\mem (i, v) -> setAt (addr + i) v mem) (memory cpu) (zip [0..] regsToStore)
    in cpu { memory = newMem }

-- FX65: Read registers V0 through Vx from memory starting at I
loadRegs :: Word8 -> CPU -> CPU
loadRegs x cpu =
    let addr    = fromIntegral (iReg cpu)
        vals    = take (fromIntegral x + 1) $ drop addr (memory cpu)
        newRegs = foldl (\regs (i, v) -> setAt i v regs) (registers cpu) (zip [0..] vals)
    in cpu { registers = newRegs }


-- Decodes opcode and dispatches to the appropriate handler
execute :: Word16 -> CPU -> CPU
execute opcode cpu =
    let nnn = opcode .&. 0x0FFF
        x   = fromIntegral ((opcode .&. 0x0F00) `shiftR` 8) :: Word8
        y   = fromIntegral ((opcode .&. 0x00F0) `shiftR` 4) :: Word8
        n   = fromIntegral (opcode .&. 0x000F) :: Word8
        kk  = fromIntegral (opcode .&. 0x00FF) :: Word8
    in case opcode of
        0x00E0                        -> cls cpu
        0x00EE                        -> ret cpu
        _ | top == 0x1000             -> jp nnn cpu
        _ | top == 0x2000             -> call nnn cpu
        _ | top == 0x3000             -> seVxByte x kk cpu
        _ | top == 0x4000             -> sneVxByte x kk cpu
        _ | top == 0x5000             -> seVxVy x y cpu
        _ | top == 0x6000             -> ldVxByte x kk cpu
        _ | top == 0x7000             -> addVxByte x kk cpu
        _ | top == 0x8000, n == 0x0   -> ldVxVy x y cpu
        _ | top == 0x8000, n == 0x1   -> orVxVy x y cpu
        _ | top == 0x8000, n == 0x2   -> andVxVy x y cpu
        _ | top == 0x8000, n == 0x3   -> xorVxVy x y cpu
        _ | top == 0x8000, n == 0x4   -> addVxVy x y cpu
        _ | top == 0x8000, n == 0x5   -> subVxVy x y cpu
        _ | top == 0x8000, n == 0x6   -> shrVx x cpu
        _ | top == 0x8000, n == 0x7   -> subnVxVy x y cpu
        _ | top == 0x8000, n == 0xE   -> shlVx x cpu
        _ | top == 0x9000             -> sneVxVy x y cpu
        _ | top == 0xA000             -> ldI nnn cpu
        _ | top == 0xB000             -> jpV0 nnn cpu
        _ | top == 0xC000             -> rnd x kk cpu
        _ | top == 0xD000             -> drw x y n cpu
        _ | top == 0xE000, kk == 0x9E -> skp x cpu
        _ | top == 0xE000, kk == 0xA1 -> sknp x cpu
        _ | top == 0xF000, kk == 0x07 -> ldVxDT x cpu
        _ | top == 0xF000, kk == 0x0A -> ldVxK x cpu
        _ | top == 0xF000, kk == 0x15 -> ldDTVx x cpu
        _ | top == 0xF000, kk == 0x18 -> ldSTVx x cpu
        _ | top == 0xF000, kk == 0x1E -> addIVx x cpu
        _ | top == 0xF000, kk == 0x29 -> ldFVx x cpu
        _ | top == 0xF000, kk == 0x33 -> ldBVx x cpu
        _ | top == 0xF000, kk == 0x55 -> storeRegs x cpu
        _ | top == 0xF000, kk == 0x65 -> loadRegs x cpu
        _                             -> cpu
  where
    top = opcode .&. 0xF000


cycle :: CPU -> CPU
cycle cpu = let (opcode, cpu') = fetch cpu in execute opcode cpu'

tickTimers :: CPU -> CPU
tickTimers cpu = cpu
    { delayTimer = if delayTimer cpu > 0 then delayTimer cpu - 1 else 0
    , soundTimer = if soundTimer cpu > 0 then soundTimer cpu - 1 else 0
    }
