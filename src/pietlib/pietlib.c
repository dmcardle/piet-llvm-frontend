#include <stdio.h>
#include <stdlib.h>

#define COMBO(dp,cc) 2*dp + cc

const int PIET_STACK_SIZE = 1000;
const int NUM_STD_HUE = 6;
const int NUM_LIGHTNESS = 3;
const int NUM_DP_VALS = 4;
const int NUM_CC_VALS = 2;

/* Order of hues/lightnesses Must agree with order in ImageLoader.hs */
enum PietHue {P_RED, P_YELLOW, P_GREEN, P_CYAN, P_BLUE, P_MAGENTA, P_BLACK,
              P_WHITE};
enum PietLightness {P_LIGHT, P_NORMAL, P_DARK};

enum PietCommand {P_NOP, P_PUSH, P_POP, P_ADD, P_SUBTRACT, P_MULTIPLY, P_DIVIDE,
                  P_MOD, P_NOT, P_GREATER, P_POINTER, P_SWITCH, P_DUPLICATE,
                  P_ROLL, P_IN_NUM, P_IN_CHAR, P_OUT_NUM, P_OUT_CHAR};

int PietStack[PIET_STACK_SIZE];
int PietShadowStack[PIET_STACK_SIZE];

int lastBlockSize = -1;

extern int ColorBlock1(int * stackPtr, char dp, char cc, char oldHue, char oldLight);
static int computeHueChange(enum PietHue oldHue, enum PietHue newHue);
static int computeLightnessChange(enum PietLightness old, enum PietLightness new);
static enum PietCommand determineCommand(int hueChange, int lightChange);

int stackSafeToPop(int *stackPtr) {
  return stackPtr > PietStack;
}
int stackPop(int **stackPtr) {
  int val = **stackPtr;
  (*stackPtr)--;
  return val;
}
void stackPush(int **stackPtr, int val) {
  (*stackPtr)++;
  **stackPtr = val;
}
void stackPrint(int *stackPtr) {
  fprintf(stderr, "\tStack:");
  int * ptr;
  for (ptr = PietStack+1; ptr <= stackPtr; ptr++) {
    fprintf(stderr, " %d", *ptr);
  }
  fprintf(stderr, "\n");
}

void nextBlock(int * stackPtr,
               char dp,
               char cc,
               int (*funcTable[8])(int*, char, char, char, char),
               enum PietHue oldHue,
               enum PietLightness oldLightness,
               enum PietHue newHue,
               enum PietLightness newLightness,
               int curBlockSize,
               int curBlockNum) {

  int hueChange = computeHueChange(oldHue, newHue);
  int lightChange = computeLightnessChange(oldLightness, newLightness);

  //
  // Print debug trace info.
  //
  fprintf(stderr, "In block %d (size %d)\n\tcombo = %d\n"
          "\tHue:       %d -> %d\n"
          "\tLightness: %d -> %d\n",
          curBlockNum, curBlockSize, COMBO(dp,cc), oldHue, newHue, oldLightness,
          newLightness);

  fprintf(stderr, "\tdelta hue = %d\n", hueChange);
  fprintf(stderr, "\tdelta light = %d\n", lightChange);

  int _dp, _cc;
  for (_dp=0; _dp<NUM_DP_VALS; _dp++) {
    for (_cc=0; _cc<NUM_CC_VALS; _cc++) {
      int _combo = COMBO(_dp,_cc);
      fprintf(stderr, "\t%sCombo %d (DP %d, CC %d) -> Pointer %p\n",
              COMBO(dp,cc) == _combo ? "(*)" : "   ",
              _combo, _dp, _cc, funcTable[_combo]);
    }
  }

  stackPrint(stackPtr);

  int operand1, operand2;
  char charOperand;

  enum PietCommand command;

  /* "Sliding across a white block INTO a new color does not cause a command to
     be executed."
   */
  if (oldHue != P_BLACK && newHue != P_BLACK && oldHue != P_WHITE) {
    command = determineCommand(hueChange, lightChange);
  } else {
    command = P_NOP;
  }

  //
  // Perform the action
  //
  fprintf(stderr, "command = %d\n", command);
  switch(command) {
  case P_NOP: /* trivial */
    fprintf(stderr, "[NOP]\n");
    break;
  case P_PUSH:
    stackPush(&stackPtr, lastBlockSize);
    fprintf(stderr, "[PUSH %d]\n", lastBlockSize);
    break;
  case P_POP:
    fprintf(stderr, "[POP]\n");
    if (stackSafeToPop(stackPtr))
      stackPop(&stackPtr);
    break;
  case P_ADD:
    if (stackSafeToPop(stackPtr)) {
      operand1 = stackPop(&stackPtr);
      operand2 = stackPop(&stackPtr);
      stackPush(&stackPtr, operand2+operand1);
      fprintf(stderr, "[ADD]\n");
    }
    break;
  case P_SUBTRACT:
    if (stackSafeToPop(stackPtr)) {
      fprintf(stderr, "[SUBTRACT]\n");
      operand1 = stackPop(&stackPtr);
      operand2 = stackPop(&stackPtr);
      stackPush(&stackPtr, operand2-operand1);
    }
    break;
  case P_MULTIPLY:
    if (stackSafeToPop(stackPtr)) {
      operand1 = stackPop(&stackPtr);
      operand2 = stackPop(&stackPtr);
      fprintf(stderr, "[MULTIPLY %d %d]\n", operand1, operand2);
      stackPush(&stackPtr, operand2*operand1);
    }
    break;
  case P_DIVIDE:
    if (stackSafeToPop(stackPtr)) {
      fprintf(stderr, "[DIVIDE]\n");
      operand1 = stackPop(&stackPtr);
      operand2 = stackPop(&stackPtr);
      stackPush(&stackPtr, operand2/operand1);
    }
    break;
  case P_MOD:
    if (stackSafeToPop(stackPtr)) {
      fprintf(stderr, "[MOD]\n");
      operand1 = stackPop(&stackPtr);
      operand2 = stackPop(&stackPtr);
      stackPush(&stackPtr, operand2%operand1);
    }
    break;
  case P_NOT:
    if (stackSafeToPop(stackPtr)) {
      fprintf(stderr, "[NOT]\n");
      operand1 = stackPop(&stackPtr);
      operand1 = operand1==0 ? 1 : 0;
      stackPush(&stackPtr, operand1);
    }
    break;
  case P_GREATER:
    if (stackSafeToPop(stackPtr)) {
      fprintf(stderr, "[GREATER]\n]");
      operand1 = stackPop(&stackPtr);
      operand2 = stackPop(&stackPtr);
      stackPush(&stackPtr, operand2 > operand1);
    }
    break;
  case P_POINTER:
    if (stackSafeToPop(stackPtr)) {
      fprintf(stderr, "[POINTER]\n");
      operand1 = stackPop(&stackPtr);
      dp = (dp + operand1) % NUM_DP_VALS;
    }
    break;
  case P_SWITCH:
    if (stackSafeToPop(stackPtr)) {
      fprintf(stderr, "[SWITCH]\n");
      operand1 = stackPop(&stackPtr);
      cc = (cc + operand1) % NUM_CC_VALS;
    }
    break;
  case P_DUPLICATE:
    if (stackSafeToPop(stackPtr)) {
      fprintf(stderr, "[DUPLICATE]\n");
      operand1 = stackPop(&stackPtr);
      stackPush(&stackPtr, operand1);
      stackPush(&stackPtr, operand1);
    }
    break;
  case P_ROLL:
    if (stackSafeToPop(stackPtr)) {
      operand1 = stackPop(&stackPtr);
      operand2 = stackPop(&stackPtr);
      fprintf(stderr, "[ROLL %d %d]\n", operand1, operand2);
      {
        int *shadowStackPtr = PietShadowStack;
        int i,j;
        for (i=0; i<operand1; i++) {
          // Roll to depth=operand2
          // Bury top value on stack to depth
          int topVal = stackPop(&stackPtr);

          // Remove n=operand2 items
          for (j=0; j<<operand2; j++) {
            int p = stackPop(&stackPtr);
            stackPush(&shadowStackPtr, p);
          }

          stackPush(&stackPtr, topVal);

          // Restore original items
          for (j=0; j<operand2; j++) {
            int p = stackPop(&shadowStackPtr);
            stackPush(&stackPtr, p);
          }
        }
      }
    }
    break;
  case P_IN_NUM:
    fprintf(stderr, "[IN_NUM]\n");
    scanf("%c", &charOperand);
    operand1 = charOperand - '0';
    stackPush(&stackPtr, operand1);
    break;
  case P_IN_CHAR:
    fprintf(stderr, "[IN_CHAR]\n");
    scanf("%c", &charOperand);
    stackPush(&stackPtr, charOperand);
    break;
  case P_OUT_NUM:
    if (stackSafeToPop(stackPtr)) {
      fprintf(stderr, "[OUT_NUM]\n");
      operand1 = stackPop(&stackPtr);
      printf("*** %d\n", operand1);
    }
    break;
  case P_OUT_CHAR:
    if (stackSafeToPop(stackPtr)) {
      operand1 = stackPop(&stackPtr);
      fprintf(stderr, "[OUT_CHAR %d]\n", operand1);
      printf("*** %c\n", (char)operand1);
    }
    break;
  }

  //
  // Save this block's size in case the next block needs it.
  //
  lastBlockSize = curBlockSize;

  //
  // Jump to the next block.
  // Note that our new color is the next block's old color.
  //
  int r;
  int tries = 0;
  while (tries < NUM_DP_VALS) {
    fprintf(stderr, "<<< Block %d trying CC=%d DP=%d >>>\n", curBlockNum, cc, dp);
    r = funcTable[COMBO(dp,cc)](stackPtr, dp, cc, newHue, newLightness);
    if (r == 0)
      return;

    // If that didn't work, toggle CC.
    cc = (cc + 1) % 2;
    fprintf(stderr, "<<< Block %d trying CC=%d DP=%d >>>\n", curBlockNum, cc, dp);
    r = funcTable[COMBO(dp,cc)](stackPtr, dp, cc, newHue, newLightness);
    if (r == 0)
      return;

    // If that failed, move DP clockwise one step.
    dp = (dp + 1) % NUM_DP_VALS;
    tries++;
  }

  fprintf(stderr, "EXCEPTION: Block %d has nowhere to go\n", curBlockNum);
  exit(1);
}

static int computeHueChange(enum PietHue oldHue, enum PietHue newHue) {
  return  (NUM_STD_HUE + newHue - oldHue) % NUM_STD_HUE;
}

static int computeLightnessChange(enum PietLightness old, enum PietLightness new) {
  return (NUM_LIGHTNESS + new - old) % NUM_LIGHTNESS;
}

static enum PietCommand determineCommand(int hueChange, int lightChange) {
  switch (hueChange) {
  case 0:
    switch (lightChange) {
    case 0: return P_NOP;
    case 1: return P_PUSH;
    case 2: return P_POP;
    }
  case 1:
    switch (lightChange) {
    case 0: return P_ADD;
    case 1: return P_SUBTRACT;
    case 2: return P_MULTIPLY;
    }
  case 2:
    switch (lightChange) {
    case 0: return P_DIVIDE;
    case 1: return P_MOD;
    case 2: return P_NOT;
    }
  case 3:
    switch (lightChange) {
    case 0: return P_GREATER;
    case 1: return P_POINTER;
    case 2: return P_SWITCH;
    }
  case 4:
    switch (lightChange) {
    case 0: return P_DUPLICATE;
    case 1: return P_ROLL;
    case 2: return P_IN_NUM;
    }
  case 5:
    switch (lightChange) {
    case 0: return P_IN_CHAR;
    case 1: return P_OUT_NUM;
    case 2: return P_OUT_CHAR;
    }
  }

  fprintf(stderr, "EXCEPTION: no command for hueChange=%d and lightChange=%d\n",
         hueChange, lightChange);
  return P_NOP;
}

int main(int argc, char *argv[]) {
  int *stackPtr = &PietStack[0];
  ColorBlock1(stackPtr, 0, 0, P_BLACK, P_NORMAL);

  fprintf(stderr, "END OF EXECUTION\n");
  return 0;
}
