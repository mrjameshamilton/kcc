int main(void) {
    int a = 3;
    switch(a + 1) {
        case 0: return 0;
        case a: return 1; // case statement values must be constant
        case 1: return 2;
    }

}

int foo(int x, int y) {
    return x + y;
}

int even_arguments(int a, int b, int c, int d, int e, int f, int g, int h);

int odd_arguments(int a, int b, int c, int d, int e, int f, int g, int h, int i);

