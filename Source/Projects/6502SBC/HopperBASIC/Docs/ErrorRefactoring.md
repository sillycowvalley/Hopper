# HopperBASIC Error Message Refactoring Plan
**Document Type: Technical Specification**

## Dictionary Compression Strategy

### Available Keywords for Reuse
Your current keyword table contains these reusable strings:

**Type Keywords**: `INT`, `WORD`, `BIT`, `BYTE`, `CHAR`, `STRING`, `VAR`, `ARRAY`, `CONST`  
**Logic Keywords**: `TRUE`, `FALSE`, `AND`, `OR`, `NOT`  
**Control Keywords**: `IF`, `THEN`, `ELSE`, `ENDIF`, `FOR`, `NEXT`, `WHILE`, `WEND`, `DO`, `UNTIL`, `FUNC`, `ENDFUNC`  
**Other Keywords**: `EXPECTED`, `EXPRESSION` (from existing "EXPRESSION EXPECTED")

### Current Error Messages vs. Optimized Versions

| Current Error | Optimized Using Keywords | Bytes Saved |
|---------------|--------------------------|-------------|
| `"TYPE MISMATCH"` | `INT + " " + "EXPECTED"` | Reuses 2 keywords |
| `"FUNCTION EXISTS"` | `FUNC + " " + "EXISTS"` | Reuses 1 keyword |
| `"CONSTANT EXISTS"` | `CONST + " " + "EXISTS"` | Reuses 1 keyword |
| `"NEXT WITHOUT FOR"` | `NEXT + " WITHOUT " + FOR` | Reuses 2 keywords |
| `"SYNTAX ERROR"` | Keep as-is (universal term) | - |
| `"DIVISION BY ZERO"` | Keep as-is (classic BASIC) | - |

## Specific Type-Aware Error Messages

Instead of generic `"TYPE MISMATCH"`, use specific messages:

### Current vs. Proposed Type Error Messages

| Context | Current | Proposed (Using Keywords) |
|---------|---------|---------------------------|
| INT expected | `"TYPE MISMATCH"` | `INT + " " + EXPECTED` |
| WORD expected | `"TYPE MISMATCH"` | `WORD + " " + EXPECTED` |
| BIT expected | `"TYPE MISMATCH"` | `BIT + " " + EXPECTED` |
| BYTE expected | `"TYPE MISMATCH"` | `BYTE + " " + EXPECTED` |
| CHAR expected | `"TYPE MISMATCH"` | `CHAR + " " + EXPECTED` |
| STRING expected | `"TYPE MISMATCH"` | `STRING + " " + EXPECTED` |
| Array expected | `"TYPE MISMATCH"` | `ARRAY + " " + EXPECTED` |
| Expression expected | `"EXPRESSION EXPECTED"` | `EXPRESSION + " " + EXPECTED` ✅ |

### Enhanced Expression Error Messages

| Context | Current | Proposed (Using Keywords) |
|---------|---------|---------------------------|
| BIT expression needed | `"EXPRESSION EXPECTED"` | `BIT + " " + EXPRESSION + " " + EXPECTED` |
| INT expression needed | `"EXPRESSION EXPECTED"` | `INT + " " + EXPRESSION + " " + EXPECTED` |
| Numeric expression needed | `"EXPRESSION EXPECTED"` | `"NUMERIC " + EXPRESSION + " " + EXPECTED` |

## Control Flow Error Optimization

| Current | Proposed (Using Keywords) | Keywords Reused |
|---------|---------------------------|-----------------|
| `"NEXT WITHOUT FOR"` | `NEXT + " WITHOUT " + FOR` | 2 |
| `"FOR WITHOUT NEXT"` *(missing)* | `FOR + " WITHOUT " + NEXT` | 2 |
| `"WHILE WITHOUT WEND"` *(missing)* | `WHILE + " WITHOUT " + WEND` | 2 |
| `"WEND WITHOUT WHILE"` *(missing)* | `WEND + " WITHOUT " + WHILE` | 2 |
| `"IF WITHOUT ENDIF"` *(missing)* | `IF + " WITHOUT " + ENDIF` | 2 |
| `"ENDIF WITHOUT IF"` *(missing)* | `ENDIF + " WITHOUT " + IF` | 2 |
| `"ELSE WITHOUT IF"` *(missing)* | `ELSE + " WITHOUT " + IF` | 2 |

## Function and Declaration Errors

| Current | Proposed (Using Keywords) | Keywords Reused |
|---------|---------------------------|-----------------|
| `"FUNCTION EXISTS"` | `FUNC + " EXISTS"` | 1 |
| `"CONSTANT EXISTS"` | `CONST + " EXISTS"` | 1 |
| `"UNDEFINED IDENTIFIER"` | `"UNDEFINED " + IDENTIFIER` | 1 |
| `"CONSTANT EXPECTED"` | `CONST + " " + EXPECTED` | 2 |

## Logical Expression Errors

| Current | Proposed (Using Keywords) | Keywords Reused |
|---------|---------------------------|-----------------|
| `"INVALID BIT VALUE"` | `BIT + " " + EXPECTED` | 2 |
| Generic type error | `TRUE + " OR " + FALSE + " " + EXPECTED` | 4 |

## Implementation Strategy

### Phase 1: Core Error Helper Functions
Create helper functions that build error messages from keyword combinations:

```hopper
// Helper to build "[TYPE] EXPECTED" messages
// Input: A = BASICType enum value
// Output: Error message built and stored in ZP.LastError
TypeExpectedError()
{
    // Convert BASICType to Token, then print keyword + " EXPECTED"
    BASICTypes.ToToken();  // Convert enum to token
    Tokens.PrintKeyword(); // Reuse existing keyword printing
    // Append " EXPECTED"
}

// Helper to build "[KEYWORD1] WITHOUT [KEYWORD2]" messages  
// Input: A = first token, X = second token
// Output: Error message built
WithoutError()
{
    // Print first keyword + " WITHOUT " + second keyword
}
```

### Phase 2: Migrate Existing Errors
Replace current string constants with keyword-building functions:

```hopper
// Before:
const string nextWithoutFor = "NEXT WITHOUT FOR";

// After:
NextWithoutForError()
{
    LDA #Token.NEXT
    LDX #Token.FOR  
    WithoutError();
}
```

### Phase 3: Enhanced Type Checking
Use specific type error messages throughout the compiler:

```hopper
// In expression compiler when INT expected:
// Before:
Error.TypeMismatch();

// After:  
LDA #BASICType.INT
Error.TypeExpectedError();
```

## String Constant Optimization

### Keep These Universal Constants
Some error strings should remain as constants because they're not easily built from keywords:

```hopper
// Universal BASIC terms - keep as-is
const string syntaxError = "SYNTAX ERROR";
const string outOfMemory = "OUT OF MEMORY"; 
const string divisionByZero = "DIVISION BY ZERO";
const string numericOverflow = "NUMERIC OVERFLOW";

// New short constants for common connectors
const string without = " WITHOUT ";
const string exists = " EXISTS";
const string undefined = "UNDEFINED ";
```

### Replace With Keyword Combinations
These can be built from existing keywords:

```hopper
// Remove these constants:
// const string nextWithoutFor = "NEXT WITHOUT FOR";
// const string functionExists = "FUNCTION EXISTS"; 
// const string constantExists = "CONSTANT EXISTS";
// const string typeMismatch = "TYPE MISMATCH";

// Replace with builder functions using keyword table
```

## Memory Savings Calculation

### Dictionary Compression Benefits
- **Keywords already in memory**: ~50 strings in keyword table
- **Typical reuse**: 2-4 keywords per error message
- **Space savings**: ~30-50% reduction in error string storage
- **ROM efficiency**: Better alignment with keyword table usage

### Example Memory Usage

**Current approach**:
```hopper
const string nextWithoutFor = "NEXT WITHOUT FOR";      // 16 bytes
const string functionExists = "FUNCTION EXISTS";        // 16 bytes  
const string constantExists = "CONSTANT EXISTS";        // 16 bytes
// Total: 48 bytes
```

**Optimized approach**:
```hopper
const string without = " WITHOUT ";  // 9 bytes
const string exists = " EXISTS";     // 8 bytes
// Total: 17 bytes + keyword reuse
// Savings: ~65% less storage
```

## Implementation Benefits

1. **Dictionary Compression**: Maximum reuse of existing keyword strings
2. **Consistency**: Error messages use exact same terms as language keywords  
3. **Specificity**: `INT EXPECTED` is clearer than `TYPE MISMATCH`
4. **Maintainability**: Changes to keywords automatically update error messages
5. **ROM Efficiency**: Better cache locality with keyword table
6. **Accessibility**: More descriptive error messages for better user experience

## Migration Order

1. **Start with type errors**: Most common and highest impact
2. **Control flow errors**: FOR/NEXT, WHILE/WEND, IF/ENDIF
3. **Declaration errors**: FUNC, CONST, VAR
4. **Expression errors**: More specific type requirements
5. **Legacy cleanup**: Remove old string constants

This approach maximizes dictionary compression while improving error message quality and maintaining classic BASIC terminology.