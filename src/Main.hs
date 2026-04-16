module Main where
import qualified Data.ByteString as BS
import Data.Word
import Data.List.Index (setAt)
import System.Environment
import Graphics.Gloss hiding (display)
import Graphics.Gloss.Interface.Pure.Game hiding (display)
import Cpu

scale' :: Float
scale' = 10

render :: CPU -> Picture
render cpu = pictures
    [ translate (fromIntegral col * scale' - 320 + scale' / 2)
                (160 - fromIntegral row * scale' - scale' / 2)
                (color white (rectangleSolid scale' scale'))
    | (row, rowPixels) <- zip [0..] (display cpu)
    , (col, pixel)     <- zip [0..] rowPixels
    , pixel
    ]



-- CHIP-8 keypad mapping:
-- 1 2 3 C        1 2 3 4
-- 4 5 6 D   <-   Q W E R
-- 7 8 9 E        A S D F
-- A 0 B F        Z X C V
keyMap :: [(Key, Int)]
keyMap =
    [ (Char '1', 0x1), (Char '2', 0x2), (Char '3', 0x3), (Char '4', 0xC)
    , (Char 'q', 0x4), (Char 'w', 0x5), (Char 'e', 0x6), (Char 'r', 0xD)
    , (Char 'a', 0x7), (Char 's', 0x8), (Char 'd', 0x9), (Char 'f', 0xE)
    , (Char 'z', 0xA), (Char 'x', 0x0), (Char 'c', 0xB), (Char 'v', 0xF)
    ]

handleInput :: Event -> CPU -> CPU
handleInput (EventKey key state _ _) cpu =
    case lookup key keyMap of
        Just idx -> cpu { keyboard = setAt idx (state == Down) (keyboard cpu) }
        Nothing  -> cpu
handleInput _ cpu = cpu

cyclesPerFrame :: Int
cyclesPerFrame = 10

step :: Float -> CPU -> CPU
step _ cpu = tickTimers $ (!! cyclesPerFrame) $ iterate Cpu.cycle cpu

main :: IO ()
main = do
    args <- getArgs
    case args of
        [path] -> do
            romBytes <- BS.readFile path
            let cpu = loadROM (BS.unpack romBytes)
            play
                (InWindow "CHIP-8" (640, 320) (100, 100))
                black
                60
                cpu
                render
                handleInput
                step
        _ -> putStrLn "Usage: chip8monad <rom>"
