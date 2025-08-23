# Hopper BASIC Disassembly & TRON Reference

## Stack Dump Format
```
evaluation_stack|frame_variables global_variables
```

**Type Prefixes:**
- `i:` INT, `w:` WORD, `b:` BYTE, `c:` CHAR, `s:` STRING, `B:` BIT, `v:` VOID, `l:` LONG
- `!:` or `?:` BAD TYPE (invalid/corrupted type pushed to stack)
- `{type:value}` = VAR containing type
- `[type:value]` = ARRAY of type  
- `|` = BP boundary (stack frame)
- `v:value` = VOID function return value slot

**Stack Direction:** Left→Right = Bottom→Top

## Opcode Encoding (6+2 bits)
- **0x00-0x3F**: No operands
- **0x40-0x7F**: One byte operand  
- **0x80-0xBF**: Two byte operands
- **0xC0-0xFF**: Three byte operands

## Critical Opcodes

### Stack Management
- `18 ENTER` - Create function frame, push BP
- `49 RETURN n` - Return (n=0: no value, n=1: pop return value)
- `4F DECSP n` - Discard n stack values

### Variables
- `43 PUSHLOCAL off` - Push local var at BP+offset
- `44 POPLOCAL off` - Pop to local var at BP+offset  
- `4B PUSHGLOBAL idx` - Push global variable
- `4C POPGLOBAL idx` - Pop to global variable
- `4D INCGLOBAL idx` - Increment global (shortcut)

### Literals
- `14 PUSH0`, `15 PUSH1`, `16 PUSHVOID` - Common constants
- `40 PUSHBIT val`, `41 PUSHBYTE val`, `42 PUSHCHAR val`
- `80 PUSHINT lo hi`, `81 PUSHWORD lo hi`
- `1A PUSHEMPTYVAR` - Create VAR containing INT:0

### Control Flow
- `88 JUMPZW lo hi` - Jump if zero (16-bit offset)
- `89 JUMPNZW lo hi` - Jump if non-zero (16-bit offset)
- `87 JUMPW lo hi` - Unconditional jump (16-bit offset)

### FOR Loops (3-byte opcodes)
- `C0 FORCHK iter fwd_lo fwd_hi` - Initial range check, jump forward if done
- `C1 FORIT iter back_lo back_hi` - Increment by step, jump back if continuing  
- `C2 FORITF iter back_lo back_hi` - Fast +1 increment (positive ranges only)

### Function Calls
- `83 CALL name_lo name_hi` - Call by name (slow, resolves)
- `84 CALLF func_lo func_hi` - Call fast (resolved address)

### System Calls
- `48 SYSCALL id` - Built-in function call
  - `09` = PRINTVALUE, `11` = PRINTCHAR

## Common Patterns

### Function Entry/Exit
```
18 ENTER                    ; Create frame
...function body...
49 RETURN n                 ; Return (n=0/1)
4F DECSP 1                  ; Caller cleans up return value
```

### Variable Assignment
```
[expression compilation]     ; Leaves value on stack
44 POPLOCAL 00              ; Store to local
4C POPGLOBAL idx            ; Store to global
```

### FOR Loop Structure
```
1A PUSHEMPTYVAR             ; Create iterator VAR
[start_expr] 44 POPLOCAL 00 ; Initialize iterator
[end_expr] [step_expr]      ; Loop bounds on stack
C0 FORCHK 00 exit_offset    ; Check range, jump if done
...loop body...
C1 FORIT 00 loop_offset     ; Increment & jump back
```

### Optimized FOR (FORITF)
```
1A PUSHEMPTYVAR             ; Create iterator  
15 PUSH1 44 POPLOCAL 00     ; Iterator = 1
41 0A 15                    ; Push end(10), Push step(1)
...loop body...
C2 FORITF 00 loop_offset    ; Fast +1, jump back
```

## Debugging Tips

1. **Stack Underflow**: Watch for empty stack before operations
2. **Type Mismatches**: Check type prefixes in stack dumps  
3. **Frame Corruption**: Verify `|` appears/disappears correctly
4. **Loop Bounds**: FORCHK should jump forward when iterator > limit
5. **VAR Evolution**: Track `{type:value}` changes in containers
6. **Global State**: Right side of `|` shows persistent state

**Key Insight**: Each instruction shows complete machine state - stack contents, local variables, globals, and types. Perfect for isolating exactly where things go wrong.