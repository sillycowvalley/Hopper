const int rowSize = 16;
char * addr;
void main() {
    addr = 0;
    int i;
    while (addr < 256) {
        printf("%04x: ", addr);
        for (i = 0; i < rowSize; i++) {
            printf("%02x ", addr[i]);
        }
        printf("\n");
        addr += rowSize;
    }
}