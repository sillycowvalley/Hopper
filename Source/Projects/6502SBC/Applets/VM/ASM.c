char* output;
int pc;

char* op;
char* arg;
    

int parse_num(char* s) {
    int val = 0;
    int neg = 0;
    char c;
    if (*s == '-') { 
        neg = 1; 
        s++; 
    }
    
    while (*s >= '0' && *s <= '9')
        val = val * 10 + (*s++ - '0');
    
    if (neg)
        return -val;
    return val;
}

void emit(int b) { 
    output[pc++] = b; 
}

void process_line(char* line) {
    int n;
    int da;
    int e;
    int id;
    
    while (*line == ' ')
        line++;
    
    if (*line != 0)
        return;
    
    if (*line == '/' && line[1] == '/')
        return;
    
    n = 0;
    while (*line && *line != ' ')
        op[n++] = *line++;
    
    op[n] = 0;
    
    while (*line == ' ')
        line++;
    
    n = 0;
    while (*line && *line != ' ' && *line != '\n')
        arg[n++] = *line++;
    
    arg[n] = 0;
    
    if (op[0] == '.' && op[1] == 'F') {
        id = parse_num(arg);
        pc = 0x2000 + (id * 0x100);
        return;
    }
    
    da = 0;
    e = -1;
    if (op[0] == 'P') {
        if (op[1] == 'U') {
            if (op[5] == 'B') {
                e = 0x00;
                da = 1;
            }
            if (op[5] == 'W') {
                emit(0x02);
                emit(parse_num(arg));
                emit(parse_num(arg) / 256);
            }
            if (op[5] == 'Z')
                e = 0x04;
            
            if (op[5] == 'O')
                e = 0x06;
            
            if (op[5] == 'S') {
                if (op[12] == '0')
                    e = 0x74;
                
                if (op[12] == 'B') {
                    e = 0x70;
                    da = 1;
                }
            }
        }
        if (op[1] == 'R') {
            if (op[6] == 'S')
                e = 0x98;
            if (op[6] == 'B')
                e = 0x92;
        }
    }
    if (op[0] == 'D') {
        if (op[1] == 'U')
            e = 0x08;
        
        if (op[1] == 'R')
            e = 0x0C;
        
    }
    if (op[0] == 'A')
        e = 0x14;
    
    if (op[0] == 'S') {
        if (op[1] == 'U')
            e = 0x16;
        
        if (op[1] == 'T') {
            if (op[6] == 'G') {
                e = 0x50;
                da = 1;
            }
            if (op[6] == 'L') {
                e = 0x60;
                da = 1;
            }
        }
    }
    if (op[0] == 'M')
        e = 0x18;
    
    if (op[0] == 'E')
        e = 0x26;
    
    if (op[0] == 'L') {
        if (op[1] == 'T')
            e = 0x2A;
        
        if (op[1] == 'O') {
            if (op[5] == 'G') {
                e = 0x48;
                da = 1;
            }
            if (op[5] == 'L') {
                e = 0x5C;
                da = 1;
            }
        }
    }
    if (op[0] == 'C') {
        emit(0x80);
        emit(parse_num(arg) * 2);
    }
    if (op[0] == 'R')
        e = 0x82;
    
    if (op[0] == 'J') {
        if (op[5] == 'C') {
            e = 0x84;
            da = 1;
        }
        if (op[5] == 'Z') {
            e = 0x88;
            da = 1;
        }
    }
    if (e != -1)
        emit(e);
    if (da != 0)
        emit(parse_num(arg));
}

void freeAll()
{
    free(output);
    free(line);
    free(op);
    free(arg);
}

void main() {
    FILE* fp;
    char* line;
    int i;
    int n;
    
    output = malloc(0x8000);
    line   = malloc(256);
    op     = malloc(32);
    arg    = malloc(32);
    
    printf("VM Assembler\n");
    
    fp = fopen("TEST.VMA", "r");
    if (fp == null) { 
        printf("Cannot open TEST.VMA\n"); 
        freeAll();
        return; 
    }
    
    pc = 0;
    while (1) {
        n = 0;
        i = fgetc(fp);
        while (i >= 0 && i != '\n') {
            line[n++] = i;
            i = fgetc(fp);
        }
        line[n] = 0;
        
        if (i < 0 && n == 0)
            break;
        
        process_line(line);
    }
    fclose(fp);
    
    fp = fopen("TEST.BIN", "w");
    if (fp == null) { 
        printf("Cannot create TEST.BIN\n"); 
        freeAll();
        return; 
    }
    
    for (i = 0; i < pc; i++)
        fputc(output[i], fp);
    
    fclose(fp);
    
    printf("Wrote %d bytes\n", pc);
    
    freeAll();
}