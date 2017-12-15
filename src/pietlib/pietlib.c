#include <stdio.h>

#define PIET_STACK_SIZE 1000

int PietStack[PIET_STACK_SIZE];

extern void ColorBlock1(int * stackPtr, char dp, char cc);

void nextBlock(int * stackPtr, char dp, char cc,
               void (*funcTable[8])(int*, char, char),
               int combo,
               int oldHue,
               int oldLightness,
               int newHue,
               int newLightness,
               int curBlockNum) {
  printf("In block %d\n\tcombo = %d\n\toldHue = %d\n\toldLightness = %d\n\tnewHue = %d\n\tnewwLightness = %d\n", curBlockNum, combo, oldHue, oldLightness, newHue, newLightness);
  funcTable[combo](stackPtr, dp, cc);
}


int main(int argc, char *argv[]) {
  int *stackPtr = &PietStack[0];
  ColorBlock1(stackPtr, 0, 0);

  printf("Hello");
  return 0;
}

