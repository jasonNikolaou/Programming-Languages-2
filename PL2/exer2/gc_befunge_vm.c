#include <stdio.h>
#include <stdlib.h>

//---defines-----
#define ROWS 25
#define COLS 80
#define lli long long int

#define MOVE_ROW {\
  row = row + dir_row;\
  if (row >= ROWS) row = 0;\
  else if (row < 0) row = 24;\
}
#define MOVE_COL {\
  col = col + dir_col;\
  if (col >= COLS) col = 0;\
  else if (col < 0) col = 79;\
}
#define LOAD_OP \
  op = program[row][col];
#define MOVE_PC_AND_LOAD_OP {\
  if (dir_row) {\
    row = row + dir_row;\
    if (row >= ROWS) row = 0;\
    else if (row < 0) row = 24;\
  }\
  else { \
    col = col + dir_col;\
    if (col >= COLS) col = 0;\
    else if (col < 0) col = 79;\
  }\
  LOAD_OP; \
}
#define NEXT_INSTRUCTION \
  goto *labels[op]

//----stack----
#define STACK_SIZE 1048580 //1 << 20
lli stack[STACK_SIZE];
#define PUSH(VAL) *(top++) = VAL
#define POP (top == stack) ? 0 : *(--top)

//----heap-----
#define HEAP_SIZE 16777300//1 << 24
#define SIZE 2
#define OUT_OF_MEMORY_ERROR -42
#define null -1
lli heap[HEAP_SIZE];

//---heap functions---
lli BIT_64 = 1LL << 63; //dirty bit
lli NOT_BIT_64 = ~(1LL << 63);
lli BIT_63 = 1LL << 62; //mark bit
lli NOT_BIT_63 = ~(1LL << 62);
lli BIT_62 = 1LL << 61; //sign bit
lli NOT_BIT_62 = ~(1LL << 61);
lli BIT_64_63 = 3LL << 62; //dirty and mark bit.
lli NOT_BIT_64_63 = ~(3LL << 62);

//---dirty--------
#define GET_DIRTY(X) \
  (BIT_64 & X)
#define SET_DIRTY(X) \
  X = BIT_64 | X
#define CLEAR_DIRTY(X) \
  X = NOT_BIT_64 & X
//-----mark-------
#define GET_MARKED(X) \
  (BIT_63 & X)
#define SET_MARKED(X) \
  X = BIT_63 | X
#define CLEAR_MARKED(X) \
  X = NOT_BIT_63 & X
//-dirty and marked-
#define SET_DIRTY_AND_MARKED(X) \
  X = BIT_64_63 | X
#define CLEAR_DIRTY_AND_MARKED(X) \
  X = NOT_BIT_64_63 & X
//-----sign-------
#define GET_SIGN(X) \
  (BIT_62 & X)

void DFS(lli x) {
    SET_MARKED(heap[x]);
    register lli tmp = heap[x];
    if (GET_SIGN(tmp)) SET_DIRTY_AND_MARKED(tmp); //extend sign
    else CLEAR_DIRTY_AND_MARKED(tmp);
    if (tmp>=0 && tmp%2==0 && tmp<HEAP_SIZE && GET_DIRTY(heap[tmp]) && !GET_MARKED(heap[tmp]))
      DFS(tmp);//DFS(heap[x])
    tmp = heap[x+1];
    if (GET_SIGN(tmp)) SET_DIRTY_AND_MARKED(tmp); //extend sign
    else CLEAR_DIRTY_AND_MARKED(tmp);
    if (tmp>=0 && tmp%2==0 && tmp<HEAP_SIZE && GET_DIRTY(heap[tmp]) && !GET_MARKED(heap[tmp]))
      DFS(tmp);//DFS(heap[x+1]);

}

int main(int argc, char ** argv) {
  //---initialize heap--
  register lli freeStart = 0;
  for (register lli i=1; i<HEAP_SIZE; i+=2)
    heap[i] = i+1;
  heap[HEAP_SIZE-1] = -1; //end of freeList.
  //------------------
  //--initialize stack--
  register lli * top = stack;
  //-------------------
  int program[ROWS][COLS];
  void* labels[256];
  for (register int i=0; i<256; i++) {
    labels[i] = &&def; //default label.
  }
  labels['0'] = &&zero;
  labels['1'] = &&one;
  labels['2'] = &&two;
  labels['3'] = &&three;
  labels['4'] = &&four;
  labels['5'] = &&five;
  labels['6'] = &&six;
  labels['7'] = &&seven;
  labels['8'] = &&eight;
  labels['9'] = &&nine;
  labels['+'] = &&add;
  labels['-'] = &&subtract;
  labels['*'] = &&multiply;
  labels['/'] = &&divide;
  labels['%'] = &&modulo;
  labels['!'] = &&not;
  labels['`'] = &&greater;
  labels['>'] = &&right;
  labels['<'] = &&left;
  labels['^'] = &&up;
  labels['v'] = &&down;
  labels['?'] = &&random;
  labels['_'] = &&horizontal_if;
  labels['|'] = &&vertical_if;
  labels['\"'] = &&stringmode;
  labels[':'] = &&dup;
  labels['\\'] = &&swap;
  labels['$'] = &&pop;
  labels['.'] = &&output_int;
  labels[','] = &&output_char;
  labels['#'] = &&bridge;
  labels['g'] = &&get;
  labels['p'] = &&put;
  labels['&'] = &&input_int;
  labels['~'] = &&input_char;
  labels[' '] = &&space;
  labels['@'] = &&end;
  //-----extended befunge----
  labels['c'] = &&cons;
  labels['h'] = &&head;
  labels['t'] = &&tail;

  FILE * file = fopen(argv[1], "r");
  if (file == NULL) {
    printf("error: file does not exist\n");
    return 0;
  }

  //read program and fill labels array
  register lli row = 0;
  register lli col = 0;
  register char op;

  while ((op = fgetc(file)) != EOF) {
    if (col > COLS) {
      printf("error: cols x rows, should not exceed 80 x 25\n");
      printf("exceeded number of columns...\n");
      return 0;
    }
    if (op == '\n') {
      while (col < COLS) {
        program[row][col++] = ' ';
      }
      col = 0;
      row ++;
    }
    else {
      if (row >= ROWS) {
        printf("error: cols x rows, should not exceed 80 x 25\n");
        printf("exceeded number of rows...\n");
        return 0;
      }
      program[row][col] = op;
      col++;
    }
  }

  //run program
  register lli dir_row = 0;
  register lli dir_col = 1;
  row = 0;
  col = 0;
  int input;
  register lli val, val1, val2;
  register lli nextFree, * p, unused;
  while (1) {
    op = program[row][col];
    switch (op) {
      case '0':
      zero:
        MOVE_PC_AND_LOAD_OP;
        PUSH(0);
        NEXT_INSTRUCTION;
      case '1':
      one:
        MOVE_PC_AND_LOAD_OP;
        PUSH(1);
        NEXT_INSTRUCTION;
      case '2':
      two:
        MOVE_PC_AND_LOAD_OP;
        PUSH(2);
        NEXT_INSTRUCTION;
      case '3':
      three:
        MOVE_PC_AND_LOAD_OP;
        PUSH(3);
        NEXT_INSTRUCTION;
      case '4':
      four:
        MOVE_PC_AND_LOAD_OP;
        PUSH(4);
        NEXT_INSTRUCTION;
      case '5':
      five:
        MOVE_PC_AND_LOAD_OP;
        PUSH(5);
        NEXT_INSTRUCTION;
      case '6':
      six:
        MOVE_PC_AND_LOAD_OP;
        PUSH(6);
        NEXT_INSTRUCTION;
      case '7':
      seven:
        MOVE_PC_AND_LOAD_OP;
        PUSH(7);
        NEXT_INSTRUCTION;
      case '8':
      eight:
        MOVE_PC_AND_LOAD_OP;
        PUSH(8);
        NEXT_INSTRUCTION;
      case '9':
      nine:
        MOVE_PC_AND_LOAD_OP;
        PUSH(9);
        NEXT_INSTRUCTION;
      case '@':
      end:
        return 0;
      case ' ':
      space:
        MOVE_PC_AND_LOAD_OP;
        NEXT_INSTRUCTION;
      case '>':
      right:
        dir_col = 1;
        dir_row = 0;
        MOVE_COL;
        LOAD_OP;
        NEXT_INSTRUCTION;
      case '<':
      left:
        dir_col = -1;
        dir_row = 0;
        MOVE_COL;
        LOAD_OP;
        NEXT_INSTRUCTION;
      case '^':
      up:
        dir_row = -1;
        dir_col = 0;
        MOVE_ROW;
        LOAD_OP;
        NEXT_INSTRUCTION;
      case 'v':
      down:
        dir_row = 1;
        dir_col = 0;
        MOVE_ROW;
        LOAD_OP;
        NEXT_INSTRUCTION;
      case '?':
      random:
        val = rand() % 4;
        if (val == 1) { dir_row = 1; dir_col = 0; }
        else if (val == 2) { dir_row = -1; dir_col = 0; }
        else if (val == 3) { dir_row = 0; dir_col = 1; }
        else { dir_row = 0; dir_col = -1; }
        MOVE_PC_AND_LOAD_OP;
        NEXT_INSTRUCTION;
      case '_':
      horizontal_if:
        val1 = POP;
        if (val1) { //acts like <
          dir_row = 0;
          dir_col = -1;
        }
        else { //acts like >
          dir_row = 0;
          dir_col = 1;
        }
        MOVE_COL;
        LOAD_OP;
        NEXT_INSTRUCTION;
      case '|':
      vertical_if:
        val1 = POP;
        if (val1) { //acts like ^
          dir_row = -1;
          dir_col = 0;
        }
        else { //acts like v
          dir_row = 1;
          dir_col = 0;
        }
        MOVE_ROW;
        LOAD_OP;
        NEXT_INSTRUCTION;
      case '+':
      add:
        MOVE_PC_AND_LOAD_OP;
        val2 = POP;
        val1 = POP;
        PUSH(val1 + val2);
        NEXT_INSTRUCTION;
      case '-':
      subtract:
        MOVE_PC_AND_LOAD_OP;
        val2 = POP;
        val1 = POP;
        PUSH(val1 - val2);
        NEXT_INSTRUCTION;
      case '/':
      divide:
        MOVE_PC_AND_LOAD_OP;
        val2 = POP;
        val1 = POP;
        PUSH(val1 / val2);
        NEXT_INSTRUCTION;
      case '*':
      multiply:
        MOVE_PC_AND_LOAD_OP;
        val2 = POP;
        val1 = POP;
        PUSH(val1 * val2);
        NEXT_INSTRUCTION;
      case '%':
      modulo:
        MOVE_PC_AND_LOAD_OP;
        val2 = POP;
        val1 = POP;
        PUSH(val1 % val2);
        NEXT_INSTRUCTION;
      case '!':
      not:
        MOVE_PC_AND_LOAD_OP;
        val1 = (POP) ? 0 : 1;
        PUSH(val1);
        NEXT_INSTRUCTION;
      case '`':
      greater:
        MOVE_PC_AND_LOAD_OP;
        val2 = POP;
        val1 = POP;
        if (val1 > val2)
          PUSH(1);
        else
          PUSH(0);
        NEXT_INSTRUCTION;
      case '"':
      stringmode:
        MOVE_PC_AND_LOAD_OP;
        while (program[row][col] != '"') {
          PUSH(program[row][col]);
          MOVE_PC_AND_LOAD_OP;
        }
        MOVE_PC_AND_LOAD_OP;
        NEXT_INSTRUCTION;
      case '&':
      input_int:
        MOVE_PC_AND_LOAD_OP;
        scanf("%d", &input);
        PUSH(input);
        NEXT_INSTRUCTION;
      case '~':
      input_char:
        MOVE_PC_AND_LOAD_OP;
        char c;
        scanf("%c", &c);
        PUSH(c);
        NEXT_INSTRUCTION;
      case '.':
      output_int:
        MOVE_PC_AND_LOAD_OP;
        printf("%lld ", POP);
        NEXT_INSTRUCTION;
      case ',':
      output_char:
        MOVE_PC_AND_LOAD_OP;
        printf("%c", POP);
        NEXT_INSTRUCTION;
      case ':':
      dup:
        MOVE_PC_AND_LOAD_OP;
        val1 = POP;
        PUSH(val1);
        PUSH(val1);
        NEXT_INSTRUCTION;
      case '\\':
      swap:
        MOVE_PC_AND_LOAD_OP;
        val1 = POP;
        val2 = POP;
        PUSH(val1);
        PUSH(val2);
        NEXT_INSTRUCTION;
      case '$':
      pop:
        MOVE_PC_AND_LOAD_OP;
        POP;
        NEXT_INSTRUCTION;
      case '#':
      bridge: //skip next command
        MOVE_PC_AND_LOAD_OP; //faster? wtf?
        MOVE_PC_AND_LOAD_OP;
        NEXT_INSTRUCTION;
      case 'g':
      get:
        MOVE_PC_AND_LOAD_OP;
        val1 = POP; //y: row
        val2 = POP;  //x: column
        PUSH(program[val1][val2]);
        NEXT_INSTRUCTION;
      case 'p':
      put:
        MOVE_PC_AND_LOAD_OP;
        val1 = POP; //y
        val2 = POP; //x
        val = POP;   //val
        program[val1][val2] = val;
        NEXT_INSTRUCTION;
      //-----extended befunge----
      case 'c':
      cons:
        MOVE_PC_AND_LOAD_OP;
        if (freeStart == null) { //garbace collection
          //mark
          for (register lli * i=stack; i<top; i++) {
            val = *i;
            if (val>=0 && val%2== 0 && val<HEAP_SIZE && GET_DIRTY(heap[val]) && !GET_MARKED(heap[val]))
              DFS(val);
          }
          //sweep
          for(p=heap; p<heap+HEAP_SIZE; p+=SIZE) {
            if (GET_MARKED(*p)) {
              CLEAR_MARKED(*p);
            }
            else {
              *(p+1) = freeStart;
              freeStart = p - heap;
            }
          }
        }
        // if (freeStart == null) {
        //   printf("out of memory...\n");
        //   return 0;
        // }
        val2 = POP; //b
        val1 = POP; //a
        //heap allocate
        nextFree = heap[freeStart+1];
        //set dirty and clear marked:
        SET_DIRTY(val1);
        CLEAR_MARKED(val1);
        heap[freeStart] = val1;
        heap[freeStart+1] = val2;
        PUSH(freeStart); //push address in stack
        freeStart = nextFree;
        NEXT_INSTRUCTION;
      case 'h':
      head:
        MOVE_PC_AND_LOAD_OP;
        val = heap[POP];
        if (GET_SIGN(val)) SET_DIRTY_AND_MARKED(val);//extend sign
        else CLEAR_DIRTY_AND_MARKED(val);
        PUSH(val);
        NEXT_INSTRUCTION;
      case 't':
      tail:
        MOVE_PC_AND_LOAD_OP;
        val = POP+1;
        PUSH(heap[val]);
        NEXT_INSTRUCTION;
      default:
      def:
        printf("error: op does not exist");
        return 0;
    }
  }
  return 0;
}
