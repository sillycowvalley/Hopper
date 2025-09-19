void main(char* exe, char* arg) {
    int c = 0;
    FILE* fp = fopen(arg, "r");
    while ((c = fgetc(fp)) != -1)
    {
        putchar(c);
    }
    fclose(fp);
}