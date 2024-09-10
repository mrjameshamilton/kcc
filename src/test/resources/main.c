
// Make sure we evaluate the lhs of a compound expression only once

int i = 0;

int putchar(int c);
int *print_A(void) {
    putchar(65); // write A to stdout
    return &i;
}

int main2(void) {

    // we should print "A" to stdout only ONCE
    //int* tmp = print_A();

    //tmp = *tmp + 5;
    *print_A() += 5ul;

    if (i != 5) {
        return 1;
    }
    return 0; // success
}

int main3(void) {
    int i = -20;
/*
    int b = 2147483647;
    int c = -5000000;

*/
//return main2();
    /* This statement is evaluated as follows:
     * 1. sign-extend i to a long with value -20
     * 2. add this long to 2147483648, resulting in the long 2147483628,
     * 3. convert this to an int with value 2147483628 (this value
     * can be represented as an int)
     */
     //i = i + 12313213l;

        i += 2147483648l;

    // make sure we got the right answer and didn't clobber b
    if (i != 2147483628) {
        return 1;
    }

    return 0;
}
int main(void) {

    return main2();

    // lval is pointer
    double d = 5.0;
    double *d_ptr = &d;
    // convert 1000 to double
    *d_ptr *= 1000u;
    if (d != 5000.0) {
        return 1; // fail
    }
    int i = -50;
    int *i_ptr = &i;
    // convert i_ptr to unsigned, perform conversion, then convert back
    *i_ptr %= 4294967200U;
    if (*i_ptr != 46) {
        return 2; // fail
    }


    // rval is pointer
    unsigned int ui = 4294967295U; // 2^32 - 1
    ui /= *d_ptr;
    // convert ui to double, perform operation, and convert back
    if (ui != 858993u) {
        return 3; // fail
    }

    // both operands are pointers
    i = -10;
    unsigned long ul = 9223372036854775807ul; // 2^63 - 1
    unsigned long *ul_ptr = &ul;
    // convert i to common type (ul), perform operation, then
    // convert back to int
    *i_ptr -= *ul_ptr;
    if (i != -9) {
        return 4; // fail
    }

    // check neighbors
    if (ul != 9223372036854775807ul) {
        return 5; // fail
    }

    return 0;
}