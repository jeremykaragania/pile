int factorial (int n) {
  int f = 1;

  while (n > 1) {
    n -= 1;
    f *= n;
  }

  return f;
}
