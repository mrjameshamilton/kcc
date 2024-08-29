// Test that we correctly infer type of bitshift expression;
// x << y has type of x, not common type of x and y

int x = 1L;

int main(void) {
    int x = 100;
    switch (x << 2l) {  // x << 2 == 400
        // these cases are duplicates b/c they'll both be converted to
        // the type of the switch expression - which is int, NOT long
        case 34359738768l:  // 2**35 + 400
            return 1;
        case 401:
            return 0;
    }
    long a = 10L;
    int b = a;
    return (long)10;
}

int foo(int x, long y) {
}