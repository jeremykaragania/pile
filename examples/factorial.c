int factorial(int n) {
  int f = 1;
  int i = 1;

  for (i = 1; i < n; i += 1) {
    f *= i;
  }

  return f;
}
