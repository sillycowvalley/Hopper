// Recursive Fibonacci benchmark
int fibo(int n) {
    if (n <= 1) {
        return n;
    }
    return fibo(n-1) + fibo(n-2);
}

void benchmark(char* name, int arg, int loops) {
    long start;
    int result;
    int count;
    long elapsed;
    long avgS;

    start = seconds();

    for (count = 0; count < loops; count++) {
        result = fibo(arg);
    }

    elapsed = seconds() - start;
    avgS = elapsed / loops;
    printf("%s(%d) = %d in %ld seconds average\n",
           name, arg, result, avgS);
}

void main() {
    benchmark("Fibo", 10, 5);
}
