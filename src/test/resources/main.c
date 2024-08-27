/*
 static local variables declared in different scopes
 * in the same function are distinct from each other.

int putchar (int ch);

int print_letters(void) {
    */
/* declare a static variable, initialize to ASCII 'A' *//*

    static int i = 65;
    */
/* print the ASCII character for its current value *//*

    putchar(i);
    {
        */
/* update the outer static 'i' variable *//*

        i = i + 1;

        */
/* declare another static variable, initialize to ASCII 'a' *//*

        static int i = 97;
        */
/* print the ASCII character for inner variable's current value *//*

        putchar(i);
        */
/* increment inner variable's value *//*

        i = i + 1;
    }
    */
/* print a newline *//*

    putchar(10);
    return 0;
}

int main(void) {
    //print uppercase and lowercase version of each letter in the alphabet
    for (int i = 0; i < 26; i = i + 1)
        print_letters();
}
*/
/* A variable with internal linkage will hide a variable with the same name
 * in a different file, even if the variable in the other file has external linkage.
 */


static int x = 1;

// read the value of this file's x variable
int read_internal_x(void);

// read the other file's x variable, which has external linkage
int read_x(void);

int main(void) {
    // This refers to the variable with internal linkage
    // defined above
    extern int x;
    if (x != 1)
        return 1;
    // update x, make sure its value is visible in another function
    x = 2;

    if (read_internal_x() != 2)
        return 1;

    // validate that other x was defined and initialized correctly
    if (read_x() != 10)
        return 1;
    return 0;
}

// this refers to the 'x' variable defines in this file with internal linkage
extern int x;

int read_internal_x(void) {
    return x;

}
