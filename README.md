brainiac
-----------
... If you're here and you're wondering what this is, it's a simple F# program that compiles [brainfuck](https://en.wikipedia.org/wiki/Brainfuck) to [.NET IL](https://en.wikipedia.org/wiki/Common_Intermediate_Language).

Some simple optimizations are also applied in the process, some of which are the same as in my [old brainfuck interpreter in C](https://github.com/profan/yet-another-brainfuck-interpreter), but this time the optimizations actually compose much better... and the optimization pass code is a lot easier to read because it's not written in C.

# General Details
* Uses a 65536 cell space by default.
* Uses (unsigned) byte cells, wrapping on overflow.
* Includes a special '#' instruction to dump the contents of all 65536 cells in a 16 column format, with a newline between each cell printed (as a number, not as a character).
* Accepts an unbounded input program size.

# Compilation Details
* Compiles the executed brainfuck to .NET IL, executing by invoking a method generated in the dynamic assembly.
* Optimizes a few common brainfuck patterns to more specific instructions before compiling to .NET IL, such as:
  * \>\>\>, ++++, etc.. optimizes continuous sequences of ptr mov/arithmetic into single operations
  * [-] (which is equal to set current cell to zero) to a direct set to zero
  * cleans up obviously no-op operations

# License
See attached LICENSE file.
