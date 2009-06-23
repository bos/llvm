#include <stdint.h>

struct S { uint32_t x0; float x1; uint32_t x2[10]; };

int
structCheck(uint32_t a, struct S *s)
{
  return s->x0 == a && s->x1 == 1.5 && s->x2[5] == a+1;
}
