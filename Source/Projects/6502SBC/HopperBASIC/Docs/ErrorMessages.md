# Classic BASIC Error Messages Reference
**Document Type: Reference Documentation**

## Overview

This document catalogs error messages from historic BASIC implementations to inform the refactoring of error messages in HopperBASIC. The goal is to maintain consistency with classic BASIC while improving clarity and accessibility.

## Current HopperBASIC Error Structure

Your current implementation in `Error.asm` includes:
- **Terse mode**: Error codes (0x0001-0x0026)  
- **Verbose mode**: Descriptive error strings
- **26 defined error types** covering syntax, runtime, and system errors

---

## Historic BASIC Error Messages by Implementation

### Microsoft BASIC Family (1975-1990s)

| Error Code | Microsoft BASIC-80 | GW-BASIC | QuickBASIC | BASICA | HopperBASIC Equivalent |
|------------|-------------------|----------|-------------|---------|------------------------|
| 2 | SYNTAX ERROR | SYNTAX ERROR | Syntax error | SYNTAX ERROR | ✅ syntaxError |
| 3 | RETURN WITHOUT GOSUB | RETURN WITHOUT GOSUB | RETURN without GOSUB | RETURN WITHOUT GOSUB | ❌ *Not needed (no GOSUB)* |
| 4 | OUT OF DATA | OUT OF DATA | Out of DATA | OUT OF DATA | ❌ *Not implemented* |
| 5 | ILLEGAL FUNCTION CALL | ILLEGAL FUNCTION CALL | Illegal function call | ILLEGAL FUNCTION CALL | ✅ undefinedIdentifier |
| 6 | OVERFLOW | OVERFLOW | Overflow | OVERFLOW | ✅ numericOverflow |
| 7 | OUT OF MEMORY | OUT OF MEMORY | Out of memory | OUT OF MEMORY | ✅ outOfMemory |
| 8 | UNDEFINED LINE NUMBER | UNDEFINED LINE NUMBER | Label not defined | UNDEFINED LINE NUMBER | ❌ *Not applicable* |
| 9 | SUBSCRIPT OUT OF RANGE | SUBSCRIPT OUT OF RANGE | Subscript out of range | SUBSCRIPT OUT OF RANGE | ✅ badIndex |
| 10 | DUPLICATE DEFINITION | DUPLICATE DEFINITION | Duplicate definition | DUPLICATE DEFINITION | ✅ functionExists, constantExists |
| 11 | DIVISION BY ZERO | DIVISION BY ZERO | Division by zero | DIVISION BY ZERO | ✅ divisionByZero |
| 12 | ILLEGAL DIRECT | ILLEGAL DIRECT | Illegal in direct mode | ILLEGAL DIRECT | ✅ illegalInFunctionMode |
| 13 | TYPE MISMATCH | TYPE MISMATCH | Type mismatch | TYPE MISMATCH | ✅ typeMismatch |
| 14 | OUT OF STRING SPACE | OUT OF STRING SPACE | Out of string space | OUT OF STRING SPACE | ✅ stringTooLong |
| 15 | STRING TOO LONG | STRING TOO LONG | String too long | STRING TOO LONG | ✅ stringTooLong |
| 16 | STRING FORMULA TOO COMPLEX | STRING FORMULA TOO COMPLEX | String formula too complex | STRING FORMULA TOO COMPLEX | ❌ *Not applicable* |
| 17 | CAN'T CONTINUE | CAN'T CONTINUE | Can't continue | CAN'T CONTINUE | ❌ *Not applicable* |
| 18 | UNDEFINED USER FUNCTION | UNDEFINED USER FUNCTION | Function not defined | UNDEFINED USER FUNCTION | ✅ undefinedIdentifier |
| 19 | NO RESUME | NO RESUME | No RESUME | NO RESUME | ❌ *Not applicable* |
| 20 | RESUME WITHOUT ERROR | RESUME WITHOUT ERROR | RESUME without error | RESUME WITHOUT ERROR | ❌ *Not applicable* |
| 21 | UNPRINTABLE ERROR | UNPRINTABLE ERROR | Unprintable error | UNPRINTABLE ERROR | ❌ *Internal use* |
| 22 | MISSING OPERAND | MISSING OPERAND | Missing operand | MISSING OPERAND | ✅ expectedExpression |
| 23 | LINE BUFFER OVERFLOW | LINE BUFFER OVERFLOW | Line buffer overflow | LINE BUFFER OVERFLOW | ✅ bufferOverflow |
| 24 | DEVICE TIMEOUT | DEVICE TIMEOUT | Device timeout | DEVICE TIMEOUT | ❌ *Hardware specific* |
| 25 | DEVICE FAULT | DEVICE FAULT | Device fault | DEVICE FAULT | ❌ *Hardware specific* |
| 26 | FOR WITHOUT NEXT | FOR WITHOUT NEXT | FOR without NEXT | FOR WITHOUT NEXT | ✅ missingNext |
| 27 | OUT OF PAPER | OUT OF PAPER | Out of paper | OUT OF PAPER | ❌ *Hardware specific* |
| 28 | (various) | UNPRINTABLE ERROR | Unprintable error | UNPRINTABLE ERROR | ❌ *Internal use* |
| 29 | WHILE WITHOUT WEND | WHILE WITHOUT WEND | WHILE without WEND | WHILE WITHOUT WEND | ❌ *To be implemented* |
| 30 | WEND WITHOUT WHILE | WEND WITHOUT WHILE | WEND without WHILE | WEND WITHOUT WHILE | ❌ *To be implemented* |

### Commodore BASIC Family (1977-1992)

| Error Code | PET BASIC 2.0 | VIC-20 BASIC 2.0 | C64 BASIC 2.0 | C128 BASIC 7.0 | HopperBASIC Equivalent |
|------------|---------------|------------------|---------------|----------------|------------------------|
| ?SYNTAX ERROR | ?SYNTAX ERROR | ?SYNTAX ERROR | ?SYNTAX ERROR | ?SYNTAX ERROR | ✅ syntaxError |
| ?TYPE MISMATCH ERROR | ?TYPE MISMATCH ERROR | ?TYPE MISMATCH ERROR | ?TYPE MISMATCH ERROR | ?TYPE MISMATCH ERROR | ✅ typeMismatch |
| ?OUT OF MEMORY ERROR | ?OUT OF MEMORY ERROR | ?OUT OF MEMORY ERROR | ?OUT OF MEMORY ERROR | ?OUT OF MEMORY ERROR | ✅ outOfMemory |
| ?UNDEF'D STATEMENT ERROR | ?UNDEF'D STATEMENT ERROR | ?UNDEF'D STATEMENT ERROR | ?UNDEF'D STATEMENT ERROR | ?UNDEF'D STATEMENT ERROR | ✅ undefinedIdentifier |
| ?ILLEGAL QUANTITY ERROR | ?ILLEGAL QUANTITY ERROR | ?ILLEGAL QUANTITY ERROR | ?ILLEGAL QUANTITY ERROR | ?ILLEGAL QUANTITY ERROR | ✅ rangeError |
| ?OVERFLOW ERROR | ?OVERFLOW ERROR | ?OVERFLOW ERROR | ?OVERFLOW ERROR | ?OVERFLOW ERROR | ✅ numericOverflow |
| ?OUT OF DATA ERROR | ?OUT OF DATA ERROR | ?OUT OF DATA ERROR | ?OUT OF DATA ERROR | ?OUT OF DATA ERROR | ❌ *Not implemented* |
| ?DIVISION BY ZERO ERROR | ?DIVISION BY ZERO ERROR | ?DIVISION BY ZERO ERROR | ?DIVISION BY ZERO ERROR | ?DIVISION BY ZERO ERROR | ✅ divisionByZero |
| ?STRING TOO LONG ERROR | ?STRING TOO LONG ERROR | ?STRING TOO LONG ERROR | ?STRING TOO LONG ERROR | ?STRING TOO LONG ERROR | ✅ stringTooLong |
| ?FILE NOT FOUND ERROR | ?FILE NOT FOUND ERROR | ?FILE NOT FOUND ERROR | ?FILE NOT FOUND ERROR | ?FILE NOT FOUND ERROR | ✅ fileNotFound |
| ?DEVICE NOT PRESENT ERROR | ?DEVICE NOT PRESENT ERROR | ?DEVICE NOT PRESENT ERROR | ?DEVICE NOT PRESENT ERROR | ?DEVICE NOT PRESENT ERROR | ❌ *Hardware specific* |
| ?NOT INPUT FILE ERROR | ?NOT INPUT FILE ERROR | ?NOT INPUT FILE ERROR | ?NOT INPUT FILE ERROR | ?NOT INPUT FILE ERROR | ❌ *File I/O specific* |
| ?NOT OUTPUT FILE ERROR | ?NOT OUTPUT FILE ERROR | ?NOT OUTPUT FILE ERROR | ?NOT OUTPUT FILE ERROR | ?NOT OUTPUT FILE ERROR | ❌ *File I/O specific* |
| ?FILE NOT OPEN ERROR | ?FILE NOT OPEN ERROR | ?FILE NOT OPEN ERROR | ?FILE NOT OPEN ERROR | ?FILE NOT OPEN ERROR | ❌ *File I/O specific* |
| ?FORMULA TOO COMPLEX ERROR | - | - | - | ?FORMULA TOO COMPLEX ERROR | ❌ *Not applicable* |
| ?CAN'T CONTINUE ERROR | - | - | - | ?CAN'T CONTINUE ERROR | ❌ *Not applicable* |
| ?REDIM'D ARRAY ERROR | - | - | - | ?REDIM'D ARRAY ERROR | ❌ *Not applicable* |

### Atari BASIC (1979-1985)

| Error Code | Atari BASIC | Atari BASIC XL | Atari BASIC XE | HopperBASIC Equivalent |
|------------|-------------|----------------|----------------|------------------------|
| 2 | SYNTAX ERROR | SYNTAX ERROR | SYNTAX ERROR | ✅ syntaxError |
| 3 | VALUE ERROR | VALUE ERROR | VALUE ERROR | ✅ rangeError |
| 4 | TOO MANY VARIABLES | TOO MANY VARIABLES | TOO MANY VARIABLES | ✅ outOfMemory |
| 5 | STRING LENGTH ERROR | STRING LENGTH ERROR | STRING LENGTH ERROR | ✅ stringTooLong |
| 6 | OUT OF MEMORY | OUT OF MEMORY | OUT OF MEMORY | ✅ outOfMemory |
| 7 | SUBSCRIPT ERROR | SUBSCRIPT ERROR | SUBSCRIPT ERROR | ✅ badIndex |
| 8 | FOR-NEXT ERROR | FOR-NEXT ERROR | FOR-NEXT ERROR | ✅ nextWithoutFor, missingNext |
| 9 | ARRAY ERROR | ARRAY ERROR | ARRAY ERROR | ✅ badIndex |
| 10 | DIM ERROR | DIM ERROR | DIM ERROR | ✅ variableExists |
| 11 | DIVISION BY ZERO | DIVISION BY ZERO | DIVISION BY ZERO | ✅ divisionByZero |
| 12 | ILLEGAL FUNCTION | ILLEGAL FUNCTION | ILLEGAL FUNCTION | ✅ undefinedIdentifier |
| 13 | NO MATCHING FOR | NO MATCHING FOR | NO MATCHING FOR | ✅ nextWithoutFor |
| 14 | STRING-NUMBER ERROR | STRING-NUMBER ERROR | STRING-NUMBER ERROR | ✅ typeMismatch |
| 15 | NUMBER TOO BIG | NUMBER TOO BIG | NUMBER TOO BIG | ✅ numericOverflow |
| 21 | FUNCTION ERROR | FUNCTION ERROR | FUNCTION ERROR | ✅ undefinedIdentifier |

### Apple BASIC Family (1977-1987)

| Error Code | Apple II Integer BASIC | Applesoft BASIC | Apple IIgs BASIC | HopperBASIC Equivalent |
|------------|------------------------|-----------------|------------------|------------------------|
| SYNTAX ERR | SYNTAX ERROR | ?SYNTAX ERROR | Syntax error | ✅ syntaxError |
| MEM FULL ERR | OUT OF MEMORY | ?OUT OF MEMORY ERROR | Out of memory | ✅ outOfMemory |
| TOO LONG ERR | STRING TOO LONG | ?STRING TOO LONG ERROR | String too long | ✅ stringTooLong |
| RANGE ERR | ILLEGAL QUANTITY | ?ILLEGAL QUANTITY ERROR | Value out of range | ✅ rangeError |
| DIM ERR | BAD SUBSCRIPT | ?BAD SUBSCRIPT ERROR | Subscript out of range | ✅ badIndex |
| TYPE ERR | TYPE MISMATCH | ?TYPE MISMATCH ERROR | Type mismatch | ✅ typeMismatch |
| /0 ERR | DIVISION BY ZERO | ?DIVISION BY ZERO ERROR | Division by zero | ✅ divisionByZero |
| - | OVERFLOW | ?OVERFLOW ERROR | Numeric overflow | ✅ numericOverflow |
| - | UNDEF'D FUNCTION | ?UNDEF'D FUNCTION ERROR | Function not defined | ✅ undefinedIdentifier |
| - | FOR WITHOUT NEXT | ?FOR WITHOUT NEXT ERROR | FOR without NEXT | ✅ missingNext |
| - | NEXT WITHOUT FOR | ?NEXT WITHOUT FOR ERROR | NEXT without FOR | ✅ nextWithoutFor |

### TRS-80 Model I/III BASIC (1977-1980)

| Error Code | Level I BASIC | Level II BASIC | HopperBASIC Equivalent |
|------------|---------------|----------------|------------------------|
| SN | SYNTAX ERROR | SYNTAX ERROR | ✅ syntaxError |
| NF | NEXT WITHOUT FOR | NEXT WITHOUT FOR | ✅ nextWithoutFor |
| RG | RETURN WITHOUT GOSUB | RETURN WITHOUT GOSUB | ❌ *Not applicable* |
| OD | OUT OF DATA | OUT OF DATA | ❌ *Not implemented* |
| FC | ILLEGAL FUNCTION CALL | ILLEGAL FUNCTION CALL | ✅ undefinedIdentifier |
| OV | OVERFLOW | OVERFLOW | ✅ numericOverflow |
| OM | OUT OF MEMORY | OUT OF MEMORY | ✅ outOfMemory |
| UL | UNDEFINED LINE | UNDEFINED LINE | ❌ *Not applicable* |
| BS | BAD SUBSCRIPT | BAD SUBSCRIPT | ✅ badIndex |
| DD | DUPLICATE DEFINITION | DUPLICATE DEFINITION | ✅ functionExists |
| /0 | DIVISION BY ZERO | DIVISION BY ZERO | ✅ divisionByZero |
| ID | ILLEGAL DIRECT | ILLEGAL DIRECT | ✅ illegalInFunctionMode |
| TM | TYPE MISMATCH | TYPE MISMATCH | ✅ typeMismatch |
| OS | OUT OF STRING SPACE | OUT OF STRING SPACE | ✅ stringTooLong |
| LS | STRING TOO LONG | STRING TOO LONG | ✅ stringTooLong |
| ST | STRING FORMULA TOO COMPLEX | STRING FORMULA TOO COMPLEX | ❌ *Not applicable* |

### Sinclair BASIC Family (1980-1987)

| Error Code | ZX80 BASIC | ZX81 BASIC | ZX Spectrum BASIC | HopperBASIC Equivalent |
|------------|------------|------------|-------------------|------------------------|
| 0 | - | - | OK | ✅ (Messages.OK) |
| 1 | NEXT WITHOUT FOR | NEXT WITHOUT FOR | NEXT without FOR | ✅ nextWithoutFor |
| 2 | VARIABLE NOT FOUND | VARIABLE NOT FOUND | Variable not found | ✅ undefinedIdentifier |
| 3 | SUBSCRIPT WRONG | SUBSCRIPT WRONG | Subscript wrong | ✅ badIndex |
| 4 | OUT OF MEMORY | OUT OF MEMORY | Out of memory | ✅ outOfMemory |
| 5 | OUT OF SCREEN | OUT OF SCREEN | Out of screen | ❌ *Hardware specific* |
| 6 | NUMBER TOO BIG | NUMBER TOO BIG | Number too big | ✅ numericOverflow |
| 7 | RETURN WITHOUT GOSUB | RETURN WITHOUT GOSUB | RETURN without GOSUB | ❌ *Not applicable* |
| 8 | END OF FILE | END OF FILE | End of file | ❌ *File I/O specific* |
| 9 | STOP STATEMENT | STOP STATEMENT | STOP statement | ❌ *Not implemented* |
| A | INVALID ARGUMENT | INVALID ARGUMENT | Invalid argument | ✅ rangeError |
| B | INTEGER OUT OF RANGE | INTEGER OUT OF RANGE | Integer out of range | ✅ rangeError |
| C | NONSENSE IN BASIC | NONSENSE IN BASIC | Nonsense in BASIC | ✅ syntaxError |
| D | BREAK - CONT REPEATS | BREAK - CONT REPEATS | BREAK - CONT repeats | ✅ ctrlC |
| E | OUT OF DATA | OUT OF DATA | Out of DATA | ❌ *Not implemented* |
| F | INVALID FILE NAME | INVALID FILE NAME | Invalid file name | ✅ fileNotFound |



## Error Message Analysis

### Common Patterns Across All BASIC Variants

1. **SYNTAX ERROR** - Universal across all implementations
2. **OUT OF MEMORY** - Critical system error, present in all variants
3. **DIVISION BY ZERO** - Mathematical error, consistent naming
4. **TYPE MISMATCH** - Data type conflict, standard terminology
5. **STRING TOO LONG** - String handling limitation
6. **SUBSCRIPT/ARRAY ERROR** - Array bounds checking
7. **FOR/NEXT MISMATCH** - Control structure integrity
8. **OVERFLOW** - Numeric range exceeded
9. **UNDEFINED IDENTIFIER** - Variable/function not found

### Unique Error Categories by Era

**Early Systems (PET, TRS-80, Apple II)**:
- Shorter error messages due to memory constraints
- Two-letter error codes common (SN, NF, RG, etc.)
- Focus on basic language constructs

**Mid-Era Systems (Commodore 64, Atari, BBC)**:
- More descriptive error messages
- Better categorization of error types
- Introduction of procedure/function errors

**Later Systems (GW-BASIC, QuickBASIC)**:
- Most comprehensive error sets
- Detailed error descriptions
- Hardware and file I/O error handling

### Recommendations for HopperBASIC Error Refactoring

Based on this analysis, consider these improvements:

#### 1. **Missing Classic Errors to Add**
```hopper
// Control structure errors
const string whileWithoutWend = "WHILE WITHOUT WEND";
const string wendWithoutWhile = "WEND WITHOUT WHILE";

// Enhanced syntax errors  
const string missingComma = "MISSING ,";
const string missingQuote = "MISSING \"";

// Mathematical errors
const string negativeRoot = "NEGATIVE ROOT";
const string logRange = "LOG RANGE";
```

#### 2. **Consider Error Categories**
```hopper
// Group errors by category for better organization
namespace SyntaxErrors {
    const string syntax = "SYNTAX ERROR";
    const string missingParen = ") EXPECTED";
    // etc.
}

namespace RuntimeErrors {
    const string divisionByZero = "DIVISION BY ZERO";
    const string outOfMemory = "OUT OF MEMORY";
    // etc.
}
```

#### 3. **Accessibility Improvements**
Keep your current descriptive style rather than adopting cryptic two-letter codes. Your current messages like "NEXT WITHOUT FOR" are clearer than "NF".

#### 4. **Maintain Compatibility**
Your current error messages align well with classic BASIC conventions while being more descriptive than early implementations.

---

## Conclusion

Your current HopperBASIC error system is well-designed and covers most essential error conditions found in classic BASIC implementations. The comprehensive error strings provide better user experience than the terse codes used in early systems, while maintaining the familiar terminology that BASIC programmers expect.

The main areas for expansion would be adding WHILE/WEND errors if you implement those constructs, and potentially adding more specific mathematical range errors for advanced functions.