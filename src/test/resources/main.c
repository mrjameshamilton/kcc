/* Test initializing nested arrays with automatic storage duration */

/* A partially initialized array of constants.
 * Elements that aren't explicitly initialized
 * (including nested arrays) should be zeroed out.
 * */
int test_partial(void) {

    // explicitly initialize only the first half of each array,
    // at each dimension
    int first_half_only[4][2][6] = {
        {{1, 2, 3}},
        {{4, 5, 6}}
    };

    int expected = 1;
    for (int i = 0; i < 4; i = i + 1) {
        for (int j = 0; j < 2; j = j + 1) {
            for (int k = 0; k < 6; k = k + 1) {
                int val = first_half_only[i][j][k];
                if (i > 1 || j > 0 || k > 2 ) {
                    // this wasn't explicitly initialized, should be zero
                    if (val) {
                        return 0;
                    }
                } else {
                    if (val != expected) {
                        return 0;
                    }
                    expected = expected + 1;
                }
            }
        }
    }

    return 1; // success
}

int main(void) {
    if (!test_partial()) {
        return 2;
    }

    return 0; // success
}
