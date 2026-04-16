# chip8monad
A CHIP-8 emulator written in Haskell.

# Purpose
I wanted to write a CHIP-8 emulator since I heard it was a good platform to get started on due to less opcodes.
However, I also wanted to write it in Haskell.
Emulators are inherently very stateful, given you have to manage registers, memory, the stack, display, and other things.

## `State` monad
However, Haskell prides itself on being a stateless, pure language.
Therefore, it comes up with some abstractions like the `State` monad.
This monad abstracts the idea of state in a way which makes it compliant with Haskell's pure functional programming language paradigm. 

For example, take the `cls` command, which clears the memory of the system.
This can be annotated as such:

```Haskell
cls :: CPU -> CPU
```

We have to call `cls` with a CPU struct and the function returns another CPU struct, with the memory set to 0. 
This is Haskell's idea of state normally in a stateless programming language: the function returns another version of itself. 
The compiler does some optimizations behind the hood so that this isn't actually cloning the object every time, but for syntactical methods the `State` monad is used. 


## Running

**Prerequisites:** [GHC and Cabal](https://www.haskell.org/ghcup/)

```bash
# Build
cabal build

# Run with a ROM file
cabal run chip8monad -- path/to/rom.ch8
```

### Controls

The CHIP-8 keypad is mapped to your keyboard as follows:

```
CHIP-8   Keyboard
-------  --------
1 2 3 C  1 2 3 4
4 5 6 D  Q W E R
7 8 9 E  A S D F
A 0 B F  Z X C V
```

## License
MIT
