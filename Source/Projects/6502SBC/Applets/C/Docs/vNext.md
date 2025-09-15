# C Compiler Enhancement Roadmap

## Core Language Features Required

### Pointer Support
- **char\* type recognition** - Parse `char*` in declarations
- **Pointer dereference** - `*ptr` for reading values
- **Pointer assignment** - `*ptr = value` for writing through pointers  
- **Pointer arithmetic** - `ptr + offset` (scaled by 1 for char*)
- **NULL constant** - Define as 0, recognize in comparisons
- **Pointer comparison** - `ptr == NULL`, `ptr1 == ptr2`

### Memory Management
- **malloc(int size)** - Returns char*, wraps MemAllocate syscall
- **free(char* ptr)** - Wraps MemFree syscall

### File I/O
- **FILE structure** - Singleton in zero page:
  ```c
  mode         // 1 byte: 'r' or 'w'
  buffer_pos   // 2 bytes: position in FileDataBuffer  
  buffer_len   // 2 bytes: valid bytes in buffer
  eof_flag     // 1 byte: EOF indicator
  ```

- **Core functions**:
  - `FILE* fopen(char* filename, char* mode)`
  - `int fclose(FILE* fp)`
  - `int fgetc(FILE* fp)`
  - `int fputc(int c, FILE* fp)`
  - `char* fgets(char* str, int n, FILE* fp)`
  - `int fputs(char* str, FILE* fp)`
  - `int feof(FILE* fp)`

## Phase 1: Read-Only Implementation

### Goals
Build useful utilities to validate the infrastructure:

1. **TYPE command** - Display text file contents
   ```c
   void main(int argc, char* argv[]) {
       FILE* fp = fopen(argv[1], "r");
       int c;
       while ((c = fgetc(fp)) != -1) {
           putchar(c);
       }
       fclose(fp);
   }
   ```

2. **HEXDUMP utility** - Display file in hex
   ```c
   void main(int argc, char* argv[]) {
       FILE* fp = fopen(argv[1], "r");
       char* buffer = malloc(16);
       int addr = 0;
       int n;
       while ((n = fread(buffer, 1, 16, fp)) > 0) {
           printf("%04x: ", addr);
           for (int i = 0; i < n; i++) {
               printf("%02x ", buffer[i]);
           }
           printf("\n");
           addr += 16;
       }
       free(buffer);
       fclose(fp);
   }
   ```

3. **6502 Disassembler** - More complex pointer manipulation

### Implementation Order - Phase 1
1. Add `char*` type parsing
2. Implement malloc/free
3. Add pointer dereference for reading (`*ptr`)
4. Implement basic fopen/fgetc/fclose for read mode
5. Test with TYPE utility
6. Add fread for HEXDUMP
7. Expand to full read API

## Phase 2: Write Functionality

### Goals
- Complete file write API
- Begin self-hosting compiler

### Additional Features Needed
1. Pointer dereference for writing (`*ptr = value`)
2. File write mode in fopen
3. fputc/fputs/fwrite implementation  
4. Buffer flushing logic

### Bootstrap Compiler Requirements
Start new compiler `CC2.C` that can compile simple C:
- Initially just a lexer that reads source and writes tokens
- Gradually add parser, AST builder
- Eventually replace current compiler

## Implementation Notes

### Parser Changes
- Recognize `*` after `char` for type
- Parse `*` prefix for dereference in expressions
- Handle pointer arithmetic in binary operators

### Code Generation
- Dereference generates indirect load: `LDA [ptr],Y`
- Pointer arithmetic is simple addition (no scaling for char*)
- NULL is just zero in NEXT0-3

### Type System
- Track CharPtr as distinct from Char
- Ensure pointer operations only on pointer types
- Allow NULL assignment to any pointer

### Constraints to Document
- Single file open at a time
- No append mode
- Sequential access only
- FILE* always returns same pointer
- Maximum malloc size: ~60KB (limited by heap)

This roadmap provides a clear path from current state to a self-hosting compiler while building useful tools along the way.