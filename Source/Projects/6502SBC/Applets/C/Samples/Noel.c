void main() {
    long s;
    long start = millis();
    int  ss    = seconds();
    int i; int j;
    printf("Noel's RetroLab Benchmark\n");
    for (i = 1; i <= 10; i++) {
        s = 0;
        for (j = 1; j <= 1000; j++) {
            s = s + j;
        }
        putchar('.');
    }
    
    printf("\n%ld\n", s);
    printf("%ld ms\n", millis() - start);
    printf("%d seconds\n", seconds() - ss);
}
