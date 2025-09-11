// tinyc.c - Minimal C compiler for 6502 Hopper BIOS
// Self-hosting compiler with char (8-bit) and int (16-bit) support
// Hex numbers only - no decimal support

// Common values as named constants for readability
const int ZERO = 0x00;
const int ONE = 0x01;
const int TWO = 0x02;
const int FOUR = 0x04;
const int EIGHT = 0x08;
const int TEN = 0x0A;
const int SIXTEEN = 0x10;
const int THIRTY_ONE = 0x1F;
const int BYTE_MASK = 0xFF;
const int BYTE_SHIFT = 0x08;

// Token types
const int TOKEN_EOF = 0x00;
const int TOKEN_NUM = 0x80;
const int TOKEN_IDENT = 0x81;
const int TOKEN_IF = 0x82;
const int TOKEN_FOR = 0x83;
const int TOKEN_RETURN = 0x84;
const int TOKEN_BREAK = 0x85;
const int TOKEN_CONTINUE = 0x86;
const int TOKEN_INT = 0x87;
const int TOKEN_CHAR = 0x88;
const int TOKEN_CONST = 0x89;
const int TOKEN_EQ = 0x90;     // ==
const int TOKEN_NE = 0x91;     // !=
const int TOKEN_LE = 0x92;     // <=
const int TOKEN_GE = 0x93;     // >=
const int TOKEN_SHL = 0x94;     // 
const int TOKEN_SHR = 0x95;     // >>
const int TOKEN_AND = 0x96;     // &
const int TOKEN_OR = 0x97;      // |
const int TOKEN_XOR = 0x98;     // ^

// 6502 Opcodes
const int LDA_IMM = 0xA9;
const int LDX_IMM = 0xA2;
const int LDY_IMM = 0xA0;
const int STA_ZP = 0x85;
const int STX_ZP = 0x86;
const int STY_ZP = 0x84;
const int STA_ABS = 0x8D;
const int LDA_ZP = 0xA5;
const int LDA_ABS = 0xAD;
const int JSR = 0x20;
const int RTS = 0x60;
const int PHA = 0x48;
const int PLA = 0x68;
const int PHX = 0xDA;
const int PLX = 0xFA;
const int PHY = 0x5A;
const int PLY = 0x7A;
const int TSX = 0xBA;
const int TXS = 0x9A;
const int JMP_ABS = 0x4C;
const int BEQ = 0xF0;
const int BNE = 0xD0;
const int BMI = 0x30;
const int BPL = 0x10;
const int BCC = 0x90;
const int BCS = 0xB0;
const int CMP_IMM = 0xC9;
const int CMP_ZP = 0xC5;
const int CPX_IMM = 0xE0;
const int CPY_IMM = 0xC0;
const int CLC = 0x18;
const int SEC = 0x38;
const int ADC_IMM = 0x69;
const int ADC_ZP = 0x65;
const int SBC_IMM = 0xE9;
const int SBC_ZP = 0xE5;
const int INC_ZP = 0xE6;
const int DEC_ZP = 0xC6;
const int INX = 0xE8;
const int DEX = 0xCA;
const int INY = 0xC8;
const int DEY = 0x88;
const int TAX = 0xAA;
const int TXA = 0x8A;
const int TAY = 0xA8;
const int TYA = 0x98;
const int ORA_ZP = 0x05;
const int AND_IMM = 0x29;
const int AND_ZP = 0x25;
const int EOR_IMM = 0x49;
const int EOR_ZP = 0x45;
const int ASL_A = 0x0A;
const int LSR_A = 0x4A;
const int ROL_A = 0x2A;
const int ROR_A = 0x6A;
const int STZ_ZP = 0x64;
const int BRK = 0x00;

// Zero Page locations for runtime
const int ZP_BP = 0x50;
const int ZP_TEMP = 0x51;
const int ZP_TEMP2 = 0x52;

// ASCII codes
const int CHAR_SPACE = 0x20;
const int CHAR_TAB = 0x09;
const int CHAR_NEWLINE = 0x0A;
const int CHAR_QUOTE = 0x27;
const int CHAR_LPAREN = 0x28;
const int CHAR_RPAREN = 0x29;
const int CHAR_PLUS = 0x2B;
const int CHAR_COMMA = 0x2C;
const int CHAR_MINUS = 0x2D;
const int CHAR_ZERO = 0x30;
const int CHAR_NINE = 0x39;
const int CHAR_SEMICOLON = 0x3B;
const int CHAR_LT = 0x3C;
const int CHAR_EQUALS = 0x3D;
const int CHAR_GT = 0x3E;
const int CHAR_LBRACE = 0x7B;
const int CHAR_RBRACE = 0x7D;
const int CHAR_BANG = 0x21;
const int CHAR_AMP = 0x26;
const int CHAR_PIPE = 0x7C;
const int CHAR_CARET = 0x5E;
const int CHAR_UPPER_A = 0x41;
const int CHAR_UPPER_F = 0x46;
const int CHAR_UPPER_Z = 0x5A;
const int CHAR_LOWER_A = 0x61;
const int CHAR_LOWER_F = 0x66;
const int CHAR_LOWER_X = 0x78;
const int CHAR_LOWER_Z = 0x7A;

// File handles
const int STDIN = 0x00;
const int STDOUT = 0x01;
const int STDERR = 0x02;

// Limits
const int MAX_SYMBOLS = 0x64;    // 100
const int MAX_FORWARDS = 0x64;   // 100
const int MAX_BREAKS = 0x64;     // 100
const int MAX_NESTING = 0x0A;    // 10
const int BUFFER_SIZE = 0x2000;  // 8KB
const int NAME_LENGTH = 0x20;    // 32
const int STRING_POOL_SIZE = 0x400; // 1KB

// Type identifiers
const int TYPE_CHAR = 0x01;
const int TYPE_INT = 0x02;
const int TYPE_CONST_CHAR = 0x11;
const int TYPE_CONST_INT = 0x12;

// Compiler state
int current_char;
int token_type;
int token_value;
int token_name[NAME_LENGTH];
int token_length;
int line_number;
int program_counter;
int error_code;

// Symbol table
int symbol_count;
int symbol_names[MAX_SYMBOLS];
int symbol_types[MAX_SYMBOLS];
int symbol_values[MAX_SYMBOLS];
int symbol_scope[MAX_SYMBOLS];

// String pool for names
char string_pool[STRING_POOL_SIZE];
int string_pool_size;

// Forward references
int forward_count;
int forward_locations[MAX_FORWARDS];
int forward_symbols[MAX_FORWARDS];

// Break/continue tracking
int loop_depth;
int loop_starts[MAX_NESTING];
int break_count;
int break_locations[MAX_BREAKS];

// Output buffer
char output_buffer[BUFFER_SIZE];
int output_size;

// File handles
int source_file;
int output_file;

// Console output for errors
int console(int c) {
    fputc(c, STDERR);
    return ZERO;
}

// Error reporting
int error(int code) {
    console(CHAR_BANG);
    console(CHAR_ZERO + code);
    console(CHAR_NEWLINE);
    exit(code);
    return ZERO;
}

// Memory operations (would use BIOS Memory.Allocate)
int malloc(int size) {
    return ZERO;  // Simplified
}

int free(int ptr) {
    return ZERO;  // Simplified
}

// File operations (would use BIOS File operations)
int fopen(int path) {
    return ZERO;  // Simplified
}

int fclose(int fd) {
    return ZERO;  // Simplified
}

int fgetc(int fd) {
    return ZERO;  // Simplified
}

int fputc(int byte, int fd) {
    return ZERO;  // Simplified
}

// Built-in functions that map to BIOS
int getchar() {
    return fgetc(source_file);
}

int putchar(int c) {
    return fputc(c, output_file);
}

int exit(int code) {
    return ZERO;  // Would call BIOS E()
}

// String operations
int strlen(char *s) {
    int len = ZERO;
    for (; s[len] != ZERO; len = len + ONE) { }
    return len;
}

int strcmp(char *s1, char *s2) {
    int i;
    for (i = ZERO; s1[i] != ZERO; i = i + ONE) {
        if (s1[i] != s2[i]) {
            return s1[i] - s2[i];
        }
    }
    return s1[i] - s2[i];
}

int strcpy(char *dest, char *src) {
    int i;
    for (i = ZERO; src[i] != ZERO; i = i + ONE) {
        dest[i] = src[i];
    }
    dest[i] = ZERO;
    return ZERO;
}

// Read next character from source
int next_char() {
    current_char = fgetc(source_file);
    if (current_char == CHAR_NEWLINE) {
        line_number = line_number + ONE;
    }
    return current_char;
}

// Write byte to output buffer
int emit_byte(int byte) {
    if (output_size >= BUFFER_SIZE) {
        error(ONE);  // Buffer overflow
    }
    output_buffer[output_size] = byte;
    output_size = output_size + ONE;
    program_counter = program_counter + ONE;
    return ZERO;
}

// Write 16-bit word to output buffer
int emit_word(int word) {
    emit_byte(word & BYTE_MASK);        // Low byte
    emit_byte(word >> BYTE_SHIFT);      // High byte
    return ZERO;
}

// Patch byte in output buffer
int patch_byte(int location, int value) {
    output_buffer[location - 0x2000] = value;
    return ZERO;
}

// Patch word in output buffer
int patch_word(int location, int value) {
    patch_byte(location, value & BYTE_MASK);
    patch_byte(location + ONE, value >> BYTE_SHIFT);
    return ZERO;
}

// Check if character is letter
int is_letter(int ch) {
    if (ch >= CHAR_UPPER_A) {
        if (ch <= CHAR_UPPER_Z) {
            return ONE;
        }
    }
    if (ch >= CHAR_LOWER_A) {
        if (ch <= CHAR_LOWER_Z) {
            return ONE;
        }
    }
    return ZERO;
}

// Check if character is hex digit
int is_hex_digit(int ch) {
    if (ch >= CHAR_ZERO) {
        if (ch <= CHAR_NINE) {
            return ONE;
        }
    }
    if (ch >= CHAR_UPPER_A) {
        if (ch <= CHAR_UPPER_F) {
            return ONE;
        }
    }
    if (ch >= CHAR_LOWER_A) {
        if (ch <= CHAR_LOWER_F) {
            return ONE;
        }
    }
    return ZERO;
}

// Parse hex digit
int hex_digit() {
    if (current_char >= CHAR_ZERO) {
        if (current_char <= CHAR_NINE) {
            return current_char - CHAR_ZERO;
        }
    }
    if (current_char >= CHAR_UPPER_A) {
        if (current_char <= CHAR_UPPER_F) {
            return current_char - CHAR_UPPER_A + TEN;
        }
    }
    if (current_char >= CHAR_LOWER_A) {
        if (current_char <= CHAR_LOWER_F) {
            return current_char - CHAR_LOWER_A + TEN;
        }
    }
    return -ONE;  // Not a hex digit
}

// Skip whitespace
int skip_whitespace() {
    for (;;) {
        if (current_char == CHAR_SPACE) { next_char(); continue; }
        if (current_char == CHAR_TAB) { next_char(); continue; }
        if (current_char == CHAR_NEWLINE) { next_char(); continue; }
        break;
    }
    return ZERO;
}

// Store identifier in string pool
int store_identifier() {
    int start = string_pool_size;
    int i;
    
    for (i = ZERO; i < token_length; i = i + ONE) {
        string_pool[string_pool_size] = token_name[i];
        string_pool_size = string_pool_size + ONE;
    }
    string_pool[string_pool_size] = ZERO;
    string_pool_size = string_pool_size + ONE;
    
    return start;
}

// Get next token
int next_token() {
    skip_whitespace();
    
    // EOF
    if (current_char == ZERO) {
        token_type = TOKEN_EOF;
        return TOKEN_EOF;
    }
    
    // Hex number 0xNN or 0xNNNN
    if (current_char == CHAR_ZERO) {
        next_char();
        if (current_char == CHAR_LOWER_X) {
            next_char();
            token_value = ZERO;
            for (;;) {
                int d = hex_digit();
                if (d < ZERO) { break; }
                token_value = (token_value << FOUR) + d;  // Shift left 4 bits
                next_char();
            }
            token_type = TOKEN_NUM;
            return TOKEN_NUM;
        }
        // Just zero
        token_value = ZERO;
        token_type = TOKEN_NUM;
        return TOKEN_NUM;
    }
    
    // Character literal
    if (current_char == CHAR_QUOTE) {
        next_char();
        token_value = current_char;
        next_char();
        if (current_char != CHAR_QUOTE) {
            error(TWO);  // Missing closing quote
        }
        next_char();
        token_type = TOKEN_NUM;
        return TOKEN_NUM;
    }
    
    // Identifier or keyword
    if (is_letter(current_char)) {
        token_length = ZERO;
        
        for (; is_letter(current_char) || is_hex_digit(current_char); next_char()) {
            if (token_length < THIRTY_ONE) {
                token_name[token_length] = current_char;
                token_length = token_length + ONE;
            }
        }
        token_name[token_length] = ZERO;
        
        // Check for keywords
        if (strcmp(token_name, "int") == ZERO) {
            token_type = TOKEN_INT;
            return TOKEN_INT;
        }
        if (strcmp(token_name, "char") == ZERO) {
            token_type = TOKEN_CHAR;
            return TOKEN_CHAR;
        }
        if (strcmp(token_name, "const") == ZERO) {
            token_type = TOKEN_CONST;
            return TOKEN_CONST;
        }
        if (strcmp(token_name, "if") == ZERO) {
            token_type = TOKEN_IF;
            return TOKEN_IF;
        }
        if (strcmp(token_name, "for") == ZERO) {
            token_type = TOKEN_FOR;
            return TOKEN_FOR;
        }
        if (strcmp(token_name, "return") == ZERO) {
            token_type = TOKEN_RETURN;
            return TOKEN_RETURN;
        }
        if (strcmp(token_name, "break") == ZERO) {
            token_type = TOKEN_BREAK;
            return TOKEN_BREAK;
        }
        if (strcmp(token_name, "continue") == ZERO) {
            token_type = TOKEN_CONTINUE;
            return TOKEN_CONTINUE;
        }
        
        // Regular identifier
        token_type = TOKEN_IDENT;
        return TOKEN_IDENT;
    }
    
    // Two-character operators
    if (current_char == CHAR_EQUALS) {
        next_char();
        if (current_char == CHAR_EQUALS) {
            next_char();
            token_type = TOKEN_EQ;
            return TOKEN_EQ;
        }
        token_type = CHAR_EQUALS;
        return CHAR_EQUALS;
    }
    
    if (current_char == CHAR_BANG) {
        next_char();
        if (current_char == CHAR_EQUALS) {
            next_char();
            token_type = TOKEN_NE;
            return TOKEN_NE;
        }
        error(FOUR);  // Unexpected !
    }
    
    if (current_char == CHAR_LT) {
        next_char();
        if (current_char == CHAR_LT) {
            next_char();
            token_type = TOKEN_SHL;
            return TOKEN_SHL;
        }
        if (current_char == CHAR_EQUALS) {
            next_char();
            token_type = TOKEN_LE;
            return TOKEN_LE;
        }
        token_type = CHAR_LT;
        return CHAR_LT;
    }
    
    if (current_char == CHAR_GT) {
        next_char();
        if (current_char == CHAR_GT) {
            next_char();
            token_type = TOKEN_SHR;
            return TOKEN_SHR;
        }
        if (current_char == CHAR_EQUALS) {
            next_char();
            token_type = TOKEN_GE;
            return TOKEN_GE;
        }
        token_type = CHAR_GT;
        return CHAR_GT;
    }
    
    // Single character tokens
    if (current_char == CHAR_PLUS) { next_char(); token_type = CHAR_PLUS; return CHAR_PLUS; }
    if (current_char == CHAR_MINUS) { next_char(); token_type = CHAR_MINUS; return CHAR_MINUS; }
    if (current_char == CHAR_AMP) { next_char(); token_type = TOKEN_AND; return TOKEN_AND; }
    if (current_char == CHAR_PIPE) { next_char(); token_type = TOKEN_OR; return TOKEN_OR; }
    if (current_char == CHAR_CARET) { next_char(); token_type = TOKEN_XOR; return TOKEN_XOR; }
    if (current_char == CHAR_LPAREN) { next_char(); token_type = CHAR_LPAREN; return CHAR_LPAREN; }
    if (current_char == CHAR_RPAREN) { next_char(); token_type = CHAR_RPAREN; return CHAR_RPAREN; }
    if (current_char == CHAR_LBRACE) { next_char(); token_type = CHAR_LBRACE; return CHAR_LBRACE; }
    if (current_char == CHAR_RBRACE) { next_char(); token_type = CHAR_RBRACE; return CHAR_RBRACE; }
    if (current_char == CHAR_SEMICOLON) { next_char(); token_type = CHAR_SEMICOLON; return CHAR_SEMICOLON; }
    if (current_char == CHAR_COMMA) { next_char(); token_type = CHAR_COMMA; return CHAR_COMMA; }
    
    error(EIGHT);  // Unknown character
    return ZERO;
}

// Expect specific token
int expect(int expected) {
    if (token_type != expected) {
        error(TEN);  // Unexpected token
    }
    next_token();
    return ZERO;
}

// Find symbol in table
int find_symbol(char *name) {
    int i;
    for (i = ZERO; i < symbol_count; i = i + ONE) {
        if (strcmp(&string_pool[symbol_names[i]], name) == ZERO) {
            return i;
        }
    }
    return -ONE;
}

// Add symbol to table
int add_symbol(char *name, int type, int value) {
    if (symbol_count >= MAX_SYMBOLS) {
        error(SIXTEEN);  // Too many symbols
    }
    
    symbol_names[symbol_count] = store_identifier();
    symbol_types[symbol_count] = type;
    symbol_values[symbol_count] = value;
    symbol_scope[symbol_count] = ZERO;  // Global for now
    symbol_count = symbol_count + ONE;
    
    return symbol_count - ONE;
}

// Forward declarations
int parse_expression();
int parse_statement();

// Get size of type
int sizeof_type(int type) {
    if (type == TYPE_CHAR) { return ONE; }
    if (type == TYPE_CONST_CHAR) { return ONE; }
    if (type == TYPE_INT) { return TWO; }
    if (type == TYPE_CONST_INT) { return TWO; }
    return TWO;  // Default to int
}

// Parse primary expression
int parse_primary() {
    int sym;
    
    // Number literal
    if (token_type == TOKEN_NUM) {
        if (token_value <= BYTE_MASK) {
            emit_byte(LDA_IMM);
            emit_byte(token_value);
        } else {
            emit_byte(LDA_IMM);
            emit_byte(token_value & BYTE_MASK);
            emit_byte(LDX_IMM);
            emit_byte(token_value >> BYTE_SHIFT);
        }
        next_token();
        return (token_value <= BYTE_MASK) ? TYPE_CHAR : TYPE_INT;
    }
    
    // Variable or function call
    if (token_type == TOKEN_IDENT) {
        sym = find_symbol(token_name);
        if (sym < ZERO) {
            error(0x20);  // Undefined symbol
        }
        
        // Check if function call
        if (next_token() == CHAR_LPAREN) {
            int arg_count = ZERO;
            
            next_token();
            
            // Arguments
            if (token_type != CHAR_RPAREN) {
                parse_expression();
                emit_byte(PHA);
                arg_count = arg_count + ONE;
                
                for (; token_type == CHAR_COMMA; ) {
                    next_token();
                    parse_expression();
                    emit_byte(PHA);
                    arg_count = arg_count + ONE;
                }
            }
            
            expect(CHAR_RPAREN);
            
            // Call function
            emit_byte(JSR);
            if (symbol_values[sym] == ZERO) {
                // Forward reference
                forward_locations[forward_count] = program_counter;
                forward_symbols[forward_count] = sym;
                forward_count = forward_count + ONE;
                emit_word(ZERO);
            } else {
                emit_word(symbol_values[sym]);
            }
            
            // Clean up arguments
            for (; arg_count > ZERO; arg_count = arg_count - ONE) {
                emit_byte(PLA);
            }
            
            return TYPE_INT;
        }
        
        // Variable access
        if (symbol_types[sym] == TYPE_CHAR) {
            emit_byte(LDA_ZP);
            emit_byte(symbol_values[sym]);
        } else {
            emit_byte(LDA_ZP);
            emit_byte(symbol_values[sym]);
            emit_byte(LDX_ZP);
            emit_byte(symbol_values[sym] + ONE);
        }
        
        return symbol_types[sym];
    }
    
    // Parenthesized expression
    if (token_type == CHAR_LPAREN) {
        int type;
        next_token();
        type = parse_expression();
        expect(CHAR_RPAREN);
        return type;
    }
    
    error(0x40);  // Expected expression
    return TYPE_INT;
}

// Parse expression with all operators
int parse_expression() {
    int left_type = parse_primary();
    int right_type;
    
    for (;;) {
        if (token_type == CHAR_PLUS) {
            next_token();
            emit_byte(PHA);
            right_type = parse_primary();
            emit_byte(CLC);
            emit_byte(ADC_ZP);
            emit_byte(ZP_TEMP);
        }
        else if (token_type == CHAR_MINUS) {
            next_token();
            emit_byte(PHA);
            right_type = parse_primary();
            emit_byte(SEC);
            emit_byte(SBC_ZP);
            emit_byte(ZP_TEMP);
        }
        else if (token_type == TOKEN_AND) {
            next_token();
            emit_byte(PHA);
            right_type = parse_primary();
            emit_byte(PLA);
            emit_byte(AND_ZP);
            emit_byte(ZP_TEMP);
        }
        else if (token_type == TOKEN_OR) {
            next_token();
            emit_byte(PHA);
            right_type = parse_primary();
            emit_byte(PLA);
            emit_byte(ORA_ZP);
            emit_byte(ZP_TEMP);
        }
        else if (token_type == TOKEN_XOR) {
            next_token();
            emit_byte(PHA);
            right_type = parse_primary();
            emit_byte(PLA);
            emit_byte(EOR_ZP);
            emit_byte(ZP_TEMP);
        }
        else if (token_type == TOKEN_SHL) {
            next_token();
            right_type = parse_primary();
            // For now, just emit single shift
            emit_byte(ASL_A);
        }
        else if (token_type == TOKEN_SHR) {
            next_token();
            right_type = parse_primary();
            // For now, just emit single shift
            emit_byte(LSR_A);
        }
        else if (token_type == CHAR_EQUALS) {
            // Assignment
            next_token();
            right_type = parse_expression();
            // Store code would go here
            return right_type;
        }
        else if (token_type == TOKEN_EQ) {
            next_token();
            emit_byte(PHA);
            right_type = parse_primary();
            emit_byte(CMP_ZP);
            emit_byte(ZP_TEMP);
            return TYPE_CHAR;
        }
        else if (token_type == TOKEN_NE) {
            next_token();
            emit_byte(PHA);
            right_type = parse_primary();
            emit_byte(CMP_ZP);
            emit_byte(ZP_TEMP);
            return TYPE_CHAR;
        }
        else if (token_type == CHAR_LT) {
            next_token();
            emit_byte(PHA);
            right_type = parse_primary();
            emit_byte(CMP_ZP);
            emit_byte(ZP_TEMP);
            return TYPE_CHAR;
        }
        else if (token_type == CHAR_GT) {
            next_token();
            emit_byte(PHA);
            right_type = parse_primary();
            emit_byte(CMP_ZP);
            emit_byte(ZP_TEMP);
            return TYPE_CHAR;
        }
        else if (token_type == TOKEN_LE) {
            next_token();
            emit_byte(PHA);
            right_type = parse_primary();
            emit_byte(CMP_ZP);
            emit_byte(ZP_TEMP);
            return TYPE_CHAR;
        }
        else if (token_type == TOKEN_GE) {
            next_token();
            emit_byte(PHA);
            right_type = parse_primary();
            emit_byte(CMP_ZP);
            emit_byte(ZP_TEMP);
            return TYPE_CHAR;
        }
        else {
            break;
        }
    }
    
    return left_type;
}

// Parse statement
int parse_statement() {
    int i;
    int patch_location;
    int old_break_count;
    
    // if statement
    if (token_type == TOKEN_IF) {
        next_token();
        expect(CHAR_LPAREN);
        parse_expression();
        expect(CHAR_RPAREN);
        
        emit_byte(CMP_IMM);
        emit_byte(ZERO);
        emit_byte(BEQ);
        patch_location = program_counter;
        emit_byte(ZERO);
        
        parse_statement();
        
        patch_byte(patch_location, program_counter - patch_location - ONE);
        
        return ZERO;
    }
    
    // for loop
    if (token_type == TOKEN_FOR) {
        next_token();
        expect(CHAR_LPAREN);
        expect(CHAR_SEMICOLON);
        expect(CHAR_SEMICOLON);
        expect(CHAR_RPAREN);
        
        loop_starts[loop_depth] = program_counter;
        old_break_count = break_count;
        loop_depth = loop_depth + ONE;
        
        parse_statement();
        
        emit_byte(JMP_ABS);
        emit_word(loop_starts[loop_depth - ONE]);
        
        // Patch all breaks from this loop
        for (i = old_break_count; i < break_count; i = i + ONE) {
            patch_word(break_locations[i], program_counter);
        }
        
        break_count = old_break_count;
        loop_depth = loop_depth - ONE;
        
        return ZERO;
    }
    
    // return statement
    if (token_type == TOKEN_RETURN) {
        next_token();
        parse_expression();
        expect(CHAR_SEMICOLON);
        
        emit_byte(LDX_ZP);
        emit_byte(ZP_BP);
        emit_byte(TXS);
        emit_byte(PLX);
        emit_byte(STX_ZP);
        emit_byte(ZP_BP);
        emit_byte(RTS);
        
        return ZERO;
    }
    
    // break statement
    if (token_type == TOKEN_BREAK) {
        next_token();
        expect(CHAR_SEMICOLON);
        
        emit_byte(JMP_ABS);
        break_locations[break_count] = program_counter;
        emit_word(ZERO);
        break_count = break_count + ONE;
        
        return ZERO;
    }
    
    // continue statement
    if (token_type == TOKEN_CONTINUE) {
        next_token();
        expect(CHAR_SEMICOLON);
        
        emit_byte(JMP_ABS);
        emit_word(loop_starts[loop_depth - ONE]);
        
        return ZERO;
    }
    
    // Block statement
    if (token_type == CHAR_LBRACE) {
        next_token();
        for (; token_type != CHAR_RBRACE; ) {
            parse_statement();
        }
        next_token();
        return ZERO;
    }
    
    // Expression statement
    parse_expression();
    expect(CHAR_SEMICOLON);
    
    return ZERO;
}

// Parse type specifier
int parse_type() {
    int type = TYPE_INT;
    int is_const = ZERO;
    
    if (token_type == TOKEN_CONST) {
        is_const = ONE;
        next_token();
    }
    
    if (token_type == TOKEN_CHAR) {
        type = TYPE_CHAR;
        next_token();
    } else if (token_type == TOKEN_INT) {
        type = TYPE_INT;
        next_token();
    } else {
        error(0x80);  // Expected type
    }
    
    if (is_const) {
        if (type == TYPE_CHAR) {
            type = TYPE_CONST_CHAR;
        } else {
            type = TYPE_CONST_INT;
        }
    }
    
    return type;
}

// Parse function definition
int parse_function() {
    int type;
    int sym;
    int param_count = ZERO;
    
    type = parse_type();
    
    if (token_type != TOKEN_IDENT) {
        error(0xA0);  // Expected function name
    }
    
    sym = add_symbol(token_name, type, program_counter);
    
    next_token();
    expect(CHAR_LPAREN);
    
    // Parameters
    for (; token_type != CHAR_RPAREN; ) {
        int param_type = parse_type();
        if (token_type != TOKEN_IDENT) {
            error(0xC0);  // Expected parameter name
        }
        add_symbol(token_name, param_type, param_count);
        param_count = param_count + ONE;
        next_token();
        
        if (token_type == CHAR_COMMA) {
            next_token();
        }
    }
    
    expect(CHAR_RPAREN);
    expect(CHAR_LBRACE);
    
    // Function prologue
    emit_byte(PHX);
    emit_byte(TSX);
    emit_byte(STX_ZP);
    emit_byte(ZP_BP);
    
    // Function body
    for (; token_type != CHAR_RBRACE; ) {
        parse_statement();
    }
    
    expect(CHAR_RBRACE);
    
    // Default epilogue
    emit_byte(LDX_ZP);
    emit_byte(ZP_BP);
    emit_byte(TXS);
    emit_byte(PLX);
    emit_byte(STX_ZP);
    emit_byte(ZP_BP);
    emit_byte(RTS);
    
    return ZERO;
}

// Main compiler
int main() {
    int i;
    int zp_alloc = 0x58;
    
    // Initialize
    current_char = ZERO;
    token_type = ZERO;
    token_value = ZERO;
    token_length = ZERO;
    line_number = ONE;
    program_counter = 0x2000;
    error_code = ZERO;
    symbol_count = ZERO;
    string_pool_size = ZERO;
    forward_count = ZERO;
    loop_depth = ZERO;
    break_count = ZERO;
    output_size = ZERO;
    
    // Open files
    source_file = fopen(ZERO);
    if (source_file < ZERO) {
        error(0xE0);  // Cannot open source
    }
    
    // Start parsing
    next_char();
    next_token();
    
    // Parse global declarations and functions
    for (;;) {
        int type;
        int is_const = ZERO;
        
        if (token_type == TOKEN_CONST) {
            is_const = ONE;
            next_token();
        }
        
        if (token_type != TOKEN_INT) {
            if (token_type != TOKEN_CHAR) {
                if (token_type == TOKEN_EOF) {
                    break;
                }
                error(0xF0);  // Expected declaration
            }
        }
        
        type = (token_type == TOKEN_CHAR) ? TYPE_CHAR : TYPE_INT;
        if (is_const) {
            type = (type == TYPE_CHAR) ? TYPE_CONST_CHAR : TYPE_CONST_INT;
        }
        
        next_token();
        
        if (token_type != TOKEN_IDENT) {
            error(0xF8);  // Expected identifier
        }
        
        strcpy(token_name, token_name);
        
        if (next_token() == CHAR_LPAREN) {
            // Function
            parse_function();
        } else {
            // Global variable(s)
            for (;;) {
                int size = sizeof_type(type);
                add_symbol(token_name, type, zp_alloc);
                zp_alloc = zp_alloc + size;
                
                if (token_type == CHAR_COMMA) {
                    next_token();
                    if (token_type != TOKEN_IDENT) {
                        error(0xFC);
                    }
                    next_token();
                } else {
                    break;
                }
            }
            expect(CHAR_SEMICOLON);
        }
    }
    
    // Patch forward references
    for (i = ZERO; i < forward_count; i = i + ONE) {
        int sym = forward_symbols[i];
        if (symbol_values[sym] == ZERO) {
            error(0xFE);  // Undefined function
        }
        patch_word(forward_locations[i], symbol_values[sym]);
    }
    
    // Write output
    output_file = fopen(ONE);
    if (output_file < ZERO) {
        error(0xFF);  // Cannot create output
    }
    
    for (i = ZERO; i < output_size; i = i + ONE) {
        fputc(output_buffer[i], output_file);
    }
    
    fclose(output_file);
    fclose(source_file);
    
    return ZERO;
}