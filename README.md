# Pile
An ANSI C compiler.

## Installation
```bash
git clone https://github.com/jeremykaragania/pile.git
cd pile
ghc -i pile/details/*.hs -o compiler pile/main.hs
```

## Usage
Compile [`examples/factorial.c`](examples/factorial.c).
```bash
./compiler examples/factorial.c
```
Create a file `main.c`.
```c
#include <stdio.h>

extern int factorial (int n);

int main() {
  printf("%d\n", factorial(5));
  return 0;
}
```
Compile `main.c`.
```bash
arm-none-linux-gnueabihf-gcc -o factorial main.c factorial.s
```
Execute `factorial`.
```bash
qemu-arm factorial
```

## License
[MIT](LICENSE)
