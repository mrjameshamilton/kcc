// Combination of compound assignment and increment/decrement with subscript expressions
int main(void) {
    int arr[4] = {1, 2};
    int *ptr = arr;
    // arr[2] *= -3;
    // after expression, ptr points to arr[1] and idx is 3

    int result = (ptr++[0] *= 3);
    return result;
}