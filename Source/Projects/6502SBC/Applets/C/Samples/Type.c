void main(char* exe, char* arg) {
    FILE* fp = fopen(arg, "r");
    int c;
    while ((c = fgetc(fp)) != -1) {
        putchar(c);
    }
    fclose(fp);
}