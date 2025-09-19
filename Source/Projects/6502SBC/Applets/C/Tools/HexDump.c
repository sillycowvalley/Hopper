
const int bytesPerRow = 16;
void main(char* exe, char* arg) {
    FILE* fp = fopen(arg, "r");
    char* buffer = malloc(bytesPerRow);
    int addr = 0;
    int n;
    int i;
    int count;
    while ((n = fread(buffer, 1, bytesPerRow, fp)) > 0) {
        printf("%04x:", addr);
        count = 0;
        for (i = 0; i < n; i++) {
            if (i % 8 == 0)
            {
                putchar(' ');
            }
            printf("%02x ", buffer[i]);
            count++;
        }
        for (i = count; i < bytesPerRow; i++)
        {
            printf("    ");
        }
        for (i = 0; i < n; i++) {
            if (i % 8 == 0)
            {
                putchar(' ');
            }
            if (buffer[i] < 32)
            {
                putchar('.');
            } else if (buffer[i] > 127)
            {
                putchar('.');
            }
            else
            {
                printf("%c", buffer[i]);
            }
        }
        printf("\n");
        addr += bytesPerRow;
    }
    free(buffer);
    fclose(fp);
}
