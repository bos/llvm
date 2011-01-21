#include <stdlib.h>
#include <stdint.h>

#ifdef DEBUG
#include <stdio.h>
#endif

#ifdef TEST
#include <stdio.h>
#endif


size_t gcd(size_t x, size_t y) {
  while (x!=0) {
    size_t tmp = y%x;
    y = x;
    x = tmp;
  }
  return y;
};

__inline__
size_t lcm(size_t x, size_t y) {
  return x*(y/gcd(x,y));
};

__inline__
size_t round_down_multiple(size_t x, size_t y) {
  return x - (x%y);
};

/*
This is the alignment that malloc always warrants.
If smaller alignments are requested, then we do not need to pad.

FIXME:
This was only tested on ix86-linux.
How to get the right number for every platform?
*/
const size_t default_align = 8;

/*
We have to waste a lot of memory,
since we need an aligned address
and before that space for a pointer.
Less memory can be wasted if 'free' also gets size and align information.
In this case we could omit padding in some cases
and in the other cases we could put the pointer after the memory chunk,
which allows us to use less padding.
*/
void *aligned_malloc(size_t size, size_t requested_align) {
  const size_t ptrsize = sizeof(void *);
  /*
  Ensure that alignment always allows to store a pointer
  (to the whole allocated block).
  */
  const size_t align = lcm(requested_align, ptrsize);
  const size_t pad = align;
  void *ptr = malloc(pad+ptrsize+size);
  if (ptr) {
    void **alignedptr = (void **) round_down_multiple((size_t)(ptr+pad+ptrsize), align);
    *(alignedptr-1) = ptr;
#ifdef DEBUG
    printf("allocated size %x with alignment %x at %08x %08x \n",
       size, align, (size_t) ptr, (size_t) alignedptr);
#endif
    return alignedptr;
  } else {
    return NULL;
  }
};

/* align must be a power of two */
void *power2_aligned_malloc(size_t size, size_t align) {
  const size_t ptrsize = sizeof(void *);
  size_t pad = align>=default_align ? align-default_align : 0;
  void *ptr = malloc(pad+ptrsize+size);
  if (ptr) {
    void **alignedptr = (void **)((size_t)(ptr+pad+ptrsize) & (-align));
    *(alignedptr-1) = ptr;
#ifdef DEBUG
    printf("allocated size 0x%x with alignment 0x%x at %08x %08x \n",
       size, align, (size_t) ptr, (size_t) alignedptr);
#endif
    return alignedptr;
  } else {
    return NULL;
  }
};

void aligned_free(void *alignedptr) {
  if (alignedptr) {
    void **sptr = (void **) alignedptr;
    void *ptr = *(sptr - 1);
#ifdef DEBUG
    printf("freed %08x %08x \n", (size_t) ptr, (size_t) alignedptr);
#endif
    free(ptr);
  } else {
    /*
    What shall we do about NULL pointers?
    Crash immediately? Make an official crash by 'free'?
    */
    free(alignedptr);
  }
};


/*
Abuse a pointer type as a size_t compatible type
and choose a name that will hopefully not clash
with names an llvm user already uses (such as 'malloc').
*/
void *aligned_malloc_sizeptr(void *size, void *align) {
  return aligned_malloc((size_t) size, (size_t) align);
}


const int
  prepadsize = 1024,
  postpadsize = 1024;

void *padded_aligned_malloc(size_t size, size_t align) {
  void *ptr = aligned_malloc(prepadsize+size+postpadsize, align);
  return ptr ? ptr+prepadsize : NULL;
};

void padded_aligned_free(void *ptr) {
  aligned_free(ptr ? ptr-prepadsize : NULL);
};


#ifdef TEST
void test_gcd (size_t x, size_t y) {
  printf("gcd(%d,%d) = %d\n", x, y, gcd (x,y));
}

void test_malloc (size_t size, size_t align) {
  uint8_t *ptr = aligned_malloc (size, align);
  if (ptr) {
    if (((size_t) ptr) % align) {
      printf ("ptr %08x not correctly aligned\n", (size_t) ptr);
    }
    size_t k;
    for (k = 0; k<size; k++) {
      ptr[k] = 0;
    }
    aligned_free (ptr);
  }
}

int main () {
  test_gcd (0,0);
  test_gcd (0,1);
  test_gcd (0,2);
  test_gcd (1,0);
  test_gcd (2,0);
  test_gcd (1,2);
  test_gcd (2,1);
  test_gcd (2,2);
  test_gcd (2,3);
  test_gcd (2,4);
  test_gcd (16,64);
  test_gcd (15,10);
  test_gcd (96,81);

  test_malloc (128, 1);
  test_malloc (128, 2);
  test_malloc (128, 3);
  test_malloc (128, 4);
  test_malloc (128, 5);
  test_malloc (128, 6);
  test_malloc (128, 8);
  test_malloc (128, 16);
  test_malloc (128, 32);
  test_malloc (128, 64);
  test_malloc (111, 1);
  test_malloc (111, 2);
  test_malloc (111, 3);
  test_malloc (111, 4);
  test_malloc (111, 5);
  test_malloc (111, 6);
  test_malloc (111, 8);
  test_malloc (111, 16);
  test_malloc (111, 32);
  test_malloc (111, 64);

  return 0;
}
#endif
