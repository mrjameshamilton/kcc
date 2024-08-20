int main(void) {
    int ans = 1;
    for (int i = 2; i < 20; i = i + 1)
        for (int j = 3; j < 10; j = j + 1)
                ans = ans + i;
    return ans;
}
