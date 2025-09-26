void main() {
    long start = millis();
    int i;
    int j = 0;
    for (i = 1; i <= 10000; i++) {
        j = j + 1;
    }
    printf("\n%ld\n", j);
    printf("%ld ms\n", millis() - start);
}
