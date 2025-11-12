# Pile
An ANSI C compiler.

You can compile a subset of ANSI C into ARMv7-A assembly which can then be used with `as` or `gcc`.

The frontend consists of a lexer, parser, and generator. The lexer tokenizes the
program and the parser constructs an AST from the tokens. Finally, the
instruction generator translates the AST into an LLVM-like IR.

The backend consists of a selector, scheduler, allocator, optimizer, and
emitter. From the IR, the selector constructs a data-flow graph of the program
using ARMv7-A instructions with virtual operands. No data-flow analysis is done
and the scheduler just flattens the graph into a sequence of instructions. The
register allocator uses linear scan, which unfortunately takes a toll on code
quality due to constant spillage. Then, the optimizer does some peephole
optimizations before being finally emitted as ARMv7-A assembly by the emitter.

## Dependencies
- [Parsec](https://github.com/haskell/parsec)

## Installation
```bash
git clone https://github.com/jeremykaragania/pile.git
cd pile/pile
ghc -i details/*.hs -o pile Main.hs
```

## Usage
```bash
./pile filename...
```

## Examples
Compile [`factorial.c`](examples/factorial.c):
```bash
./pile ../examples/factorial.c
```
Create a file `main.c`:
```c
#include <stdio.h>

extern int factorial (int n);

int main() {
  printf("%d\n", factorial(5));
  return 0;
}
```
Compile `main.c` and `factorial.s` with `gcc`:
```bash
arm-none-linux-gnueabihf-gcc -o factorial main.c factorial.s
```
Run `factorial`:
```bash
qemu-arm factorial
```

## License
[MIT](LICENSE)
