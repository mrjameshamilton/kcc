static unsigned int u = 4294967299L;

int main(void) {

    static unsigned long x = 1u;
    /* only one "L" suffix is permitted on a long
     * Note: an "LL" suffix is standard-compliant and indicates
     * a long long constant, which our implementation doesn't support,
     * but an "lL" suffix is invalid
     */
    return 1 + 1;
}