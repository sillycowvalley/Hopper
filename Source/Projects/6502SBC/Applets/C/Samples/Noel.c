// Noel's RetroLab Benchmark
void main() {
    long s = 1000;
    long start = millis();
    int i; int j;
    for (i = 1; i <= 10; i++) {
        s = 0;
        for (j = 1; j <= 1000; j++) {
            s = s + j;
        }
        putchar('.');
    }
    
    printf("%ld\n", s);
    printf("%ld ms\n", millis() - start);
}
