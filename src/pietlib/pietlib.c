#include <stdio.h>

#define PIET_STACK_SIZE 1000

int PietStack[PIET_STACK_SIZE];

extern void ColorBlock1(int * stackPtr, char dp, char cc);

void nextBlock(int * stackPtr,
               char dp,
               char cc,
               void (*funcTable[8])(int*, char, char),
               int oldHue,
               int oldLightness,
               int newHue,
               int newLightness,
               int curBlockNum) {

  int combo = 2*dp + cc;

  //
  // Print the lookup table.
  //
  printf("In block %d\n\tcombo = %d\n\toldHue = %d\n\toldLightness = %d"
         "\n\tnewHue = %d\n\tnewLightness = %d\n",
         curBlockNum, combo, oldHue, oldLightness, newHue, newLightness);

  int _dp, _cc;
  for (_dp=0; _dp<4; _dp++) {
    for (_cc=0; _cc<2; _cc++) {
      int _combo = _dp*2 + _cc;
      printf("\t%sCombo %d (DP %d, CC %d) -> Pointer %p\n",
             combo == _combo ? "(*)" : "   ",
             _combo, _dp, _cc, funcTable[_combo]);
    }
  }

  //
  // Jump to the next block.
  //
  funcTable[combo](stackPtr, dp, cc);
}

int main(int argc, char *argv[]) {
  int *stackPtr = &PietStack[0];
  ColorBlock1(stackPtr, 0, 0);

  printf("Hello");
  return 0;
}
