static int x = 65;

extern int putchar(int c);

int foo(int a, int b, int c, int d, int e, int f, int g, int h, int i) {

    int putchar(int c);
    putchar(65);
   // putchar(a);
    //putchar(b);
    //putchar(c);
    //putchar(d);
    //putchar(e);
    //putchar(f);
    //putchar(g);


    return a + h;
}
int main1(void) {
    static int i = 2;
    static int j = 3;
    int cmp = i < j; // make sure rewrite cmpl j(%rip), i(%rip)

    for (static int i = 0; ; ) ;

    if (!cmp)
        return 1;
    return 0;
}

int main(void) {
    return foo(x, 66, 67, 68, 69, 70, 71, 7, 65);
    //putchar(65);
}
