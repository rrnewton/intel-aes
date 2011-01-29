


#include <stdio.h>
#include <stdlib.h>

// Haskell can call this with a memory location.
// void blast_rands(int* ptr1, int* ptr2)
void blast_rands(int count, int* ptr)
{
  int i;
  for(i=0; i<count; i++)
  {
    *ptr = rand();
  }
}

void store_loop(int count, volatile int* ptr)
{
  int i;
  for(i=0; i<count; i++)
  {
    *ptr += 1;
  }
}


#if 0
// On a 3.33ghz desktop nehalem this produces 100M rands in 0.826 seconds.
int main() {

  int i, sum = 0; 
  for(i=0; i<100000000; i++) 
  {
    sum += rand();
  }

  printf("Sum of 100M randoms: %d\n", sum);

  return 0;
}
#endif
