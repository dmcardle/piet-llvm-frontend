#define PIET_STACK_SIZE 1000

int PietStack[PIET_STACK_SIZE];

extern void colorBlock1(int * stackPtr, char dp, char cc);

int main(int argc, char *argv[]) {
  int *stackPtr = &PietStack[0];
  colorBlock1(stackPtr, 0, 0);
  return 0;
}
