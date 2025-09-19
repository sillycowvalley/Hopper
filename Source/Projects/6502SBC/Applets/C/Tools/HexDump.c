void main(char* exe, char* arg) {
    FILE* fp = fopen(arg, "r");
    char* buffer = malloc(16);
    int addr = 0;
    int n;
    int i;
    while ((n = fread(buffer, 1, 16, fp)) > 0) {
        printf("%04x: ", addr);
        for (i = 0; i < n; i++) {
            printf("%02x ", buffer[i]);
        }
        printf("\n");
        addr += 16;
    }

    free(buffer);
    fclose(fp);
}