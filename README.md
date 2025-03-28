# Pile
An ANSI C compiler.

## Installation
```sh
$ git clone https://github.com/jeremykaragania/pile.git
$ cd pile/pile
$ ghc -i details/*.hs -o pile main.hs
```

## Usage
```sh
$ ./pile filename...
```

## Examples
Compile [`factorial.c`](examples/factorial.c).
```sh
$ ./pile ../examples/factorial.c
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
```sh
$ arm-none-linux-gnueabihf-gcc -o factorial main.c factorial.s
```
Execute `factorial`.
```sh
$ qemu-arm factorial
```

## License
[MIT](LICENSE)
