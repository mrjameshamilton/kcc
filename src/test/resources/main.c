int main(void) {
/*    for (int i = 0; i < 10; i = i + 1) {
        // case statements can only appear inside switch statements
        case 0: return 1;
    }

    int sum = 0;
    for (int i = 0; i < 10; i = i + 1) {
        switch(i % 2) {
            // make sure continue in switch in loop is permitted
            case 0: continue;
            default: sum = sum + 1;
        }
    }
    return sum;*/
/*

    int c = 0;
    switch (9) {
        case 9:
            c = 2;
        case 1:
            c = c + 4;
    }
    return c;

*/
    int a = 0;
    // a goto statement can jump to any point in a switch statement, including the middle of a case
    goto mid_case;
    switch (4) {
        case 4:
            a = 5;
        mid_case:
            a = a + 1;
            return a;
    }
    return 100;



/*int x = 1;
switch (3) {
  x = 2;
  case 1:
    // code block
  case 2:
    // code block
    break;
  case 4:
    // code block
    if (1) {
  //default:
  default:
  x=111;
  }
}
return x*/;
}
