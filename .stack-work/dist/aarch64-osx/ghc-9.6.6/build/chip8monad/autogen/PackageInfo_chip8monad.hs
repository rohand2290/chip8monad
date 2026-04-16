{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_chip8monad (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "chip8monad"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "CHIP-8 emulator"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
