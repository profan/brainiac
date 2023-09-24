brainiac
-----------
... If you're here and you're wondering what this is, it's a simple F# program that compiles [brainfuck](https://en.wikipedia.org/wiki/Brainfuck) to [.NET IL](https://en.wikipedia.org/wiki/Common_Intermediate_Language).

Some simple optimizations are also applied in the process, some of which are the same as in my [old brainfuck interpreter in C](https://github.com/profan/yet-another-brainfuck-interpreter), but this time the optimizations actually compose much better... and the optimization pass code is a lot easier to read because it's not written in C.

# License
See attached LICENSE file.
