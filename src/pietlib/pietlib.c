#include <stdio.h>

#define PIET_STACK_SIZE 1000

int PietStack[PIET_STACK_SIZE];

extern void ColorBlock1(int * stackPtr, char dp, char cc);

void nextBlock(int * stackPtr,
               char dp,
               char cc,
               void (*funcTable[8])(int*, char, char),
               int combo,
               int oldHue,
               int oldLightness,
               int newHue,
               int newLightness,
               int curBlockNum) {
  printf("In block %d\n\tcombo = %d\n\toldHue = %d\n\toldLightness = %d"
         "\n\tnewHue = %d\n\tnewLightness = %d\n",
         curBlockNum, combo, oldHue, oldLightness, newHue, newLightness);

  int i;
  for (i=0; i<8; i++) {
    printf("\t-> Pointer %d = %p\n", i, funcTable[combo]);
  }

  funcTable[combo](stackPtr, dp, cc);
}

/*
void FooBlock(int * stackPtr, char dp, char cc) {

  void (*funcTable[8])(int*, char, char);
  funcTable[0] = ColorBlock1;
  funcTable[1] = ColorBlock1;

  nextBlock(stackPtr, dp, cc, funcTable, 6, 5, 4, 3, 2, 1);
}
*/


int main(int argc, char *argv[]) {
  int *stackPtr = &PietStack[0];
  ColorBlock1(stackPtr, 0, 0);

  printf("Hello");
  return 0;
}

