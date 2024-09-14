// Apply ++ and -- to subscript expressions, which are lvalues

// indices (static to prevent copy prop)
int i = 2;
int j = 1;
int k = 0;

int main(void) {
    int arr[3][2][2] = {
        {{1, 2}, {3, 4}}, {{5, 6}, {7, 8}}, {{9, 10}, {11, 12}}};

    arr[2]++;

    // also apply ++/-- to indices
    if (++arr[--i][j--][++k] *arr[1][1][1] != 9) {
        return 3;  // fail
    }

    return 0;  // success
}