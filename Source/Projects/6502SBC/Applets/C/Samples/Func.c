int fibo(int n) {
    if (n <= 1) {
        return n;
    }
    return fibo(n-1) + fibo(n-2);
}

void main() {
    long start = millis();
    int f = 12;
    int total = fibo(f);
    long elapsed = millis() - start;
    printf("Fibo(%d)=%d\n", f, total);
    printf("Elapsed: %ld ms\n", elapsed);
}
