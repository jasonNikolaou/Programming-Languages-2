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
  LOAD_OP;\
}
#define NEXT_INSTRUCTION \
  goto *labels[op]

//----stack----
#define STACK_SIZE 1000000
lli stack[STACK_SIZE];
#define PUSH(VAL) *(top++) = VAL
#define POP (top == stack) ? 0 : *(--top)

int main(int argc, char ** argv) {
  //--initialize stack--
  register lli * top = stack;
  //-------------------
  char program[ROWS][COLS];
  void* labels[256];
  for (int i=0; i<256; i++) {
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

  FILE * file = fopen(argv[1], "r");
  if (file == NULL) {
    printf("error: file does not exist\n");
    return 0;
  }
  //read program
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
      //we get a warning, but it is faster :D
      printf("%c", POP);
      //uncomment 2 next lines and comment the above one to remove warning
      //val = POP;
      //printf("%c", (char)val);
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
    default:
    def:
      printf("error: op does not exist");
      return 0;
  }

  return 0;
}
