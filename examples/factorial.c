int factorial (int n) {
  int f = 1;

  while (n > 1) {
    f *= n;
    n -= 1;
  }

  return f;
}
