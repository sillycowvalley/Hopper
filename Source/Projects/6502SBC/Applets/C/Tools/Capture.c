// CAPTURE.C - Capture console input to file with counter
const int CtrlC = 0x03;
const int BS    = 0x08;
const int CtrlZ = 0x1A;
const int EOL   = 0x0A;
const int CR    = 0x0D;

void main(char* exe, char* filename) {
    FILE* fp;
    int c;
    int count;
    if (filename == null) {
        printf("Usage: %s <filename>\n", exe);
        return;
    }
    
    fp = fopen(filename, "w");
    if (fp == null) {
        printf("Cannot create %s\n", filename);
        return;
    }
    
    printf("Paste text, Ctrl+Z to save, Ctrl+C to abort\n");
    printf("Capturing:     ");
    
    count = 0;
    while ((c = getch()) != CtrlZ) {
        if (c == CtrlC) {
            printf("\nAborted.\n");
            fclose(fp);
            return;
        }
        if (c == CR) c = EOL;
        fputc(c, fp);
        count++;
        if (count % 10 == 0)
        {
            printf("\b\b\b\b%04x", count);
        }
    }
    
    fclose(fp);
    printf("\b\b\b\b%04x", count);
    printf("\nSaved %d bytes to %s\n", count, filename);
}