
// On wasp this produces 100M rands in 0.826 seconds.

#include <stdio.h>

int main() {

  int i, sum = 0; 
  for(i=0; i<100000000; i++) 
  {
    sum += rand();
  }

  printf("Sum of 100M randoms: %d\n", sum);

  return 0;
}
