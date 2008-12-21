#include <stdio.h>
#include <stdlib.h>

extern unsigned int fib(unsigned int);

int
main(int argc, char **argv)
{
  int n = argc > 1 ? atoi(argv[1]) : 10;
  printf("fib %d = %d\n", n, fib(n));
  exit(0);
}
