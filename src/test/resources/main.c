long add(int a, int b) {
    return (long) -a + (long) b;
}

int main(void) {
    long a = 9223372036854775807lL;
    long a = add(2147483645, 2147483645);
    /* Test returning a long from a function call */
    if (a == 4294967290l) {
        return 1;
    }
    return 0;
}