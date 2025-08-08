# VAR Type Implementation - Technical Specification

## Overview
VAR is a storage modifier (bit 0x10) that enables runtime duck typing for HopperBASIC variables. It allows a variable to change its underlying type during program execution while maintaining type safety for operations.

## Design Principles

### VAR as Storage Modifier
- **VAR bit (0x10)** exists ONLY in variable nodes in the symbol table
- Never propagates into expressions, operations, or the runtime stack
- When reading a VAR variable's value, only the underlying type (bits 0x0F) is exposed
- VAR is a "storage capability" not a runtime type

### Type System Integration
```
Variable Node Storage:
  BASICType.VAR | BASICType.INT     = 0x11  (VAR variable currently holding INT)
  BASICType.VAR | BASICType.STRING  = 0x16  (VAR variable currently holding STRING)
  BASICType.VAR | BASICType.WORD    = 0x13  (VAR variable currently holding WORD)
```

## Implementation Points

### 1. Global Variable Declaration
**Location**: `Statement.ParseDeclaration()` and `Variables.Declare()`
- When declaring `VAR myVar`, set type as `BASICType.VAR | default_type`
- Default underlying type can be VOID (0x10) or INT (0x11)
- Initial value determines starting underlying type

### 2. Assignment to Variables  
**Location**: `Executor.executePopGlobal()` and `Variables.SetValue()`

Required logic:
```hopper
// In Variables.SetValue() or similar
Objects.GetData();  // Get current variable info
LDA ZP.ACCT
AND #BASICType.VAR
if (NZ)  // Variable has VAR bit set
{
    // Allow type change
    LDA ZP.ACCT
    AND #BASICType.MASK  // Get current underlying type
    CMP #BASICType.STRING
    if (Z)
    {
        FreeStringValue();  // Clean up old string if needed
    }
    
    // Update to new type while preserving VAR bit
    LDA ZP.TOPT  // New value's type
    ORA #BASICType.VAR  // Keep VAR modifier
    STA ZP.ACCT
    
    // Update the node's type field
    LDY #Objects.snType
    STA [ZP.IDX], Y
}
else
{
    // Non-VAR variable - use existing type checking
    Instructions.CheckRHSTypeCompatibility();
}
```

### 3. Memory Management
**String Transitions**: The existing `FreeStringValue()` mechanism should handle:
- VAR currently STRING → any other type: free string memory
- VAR currently non-STRING → STRING: allocate new string memory
- This already works for regular STRING variables

### 4. Reading VAR Variables
**Location**: `Variables.GetValue()` and `Executor.executePushGlobal()`
- Mask off VAR bit when exposing type: `AND #BASICType.MASK`
- Only the underlying type (0x0F) is pushed to stack
- VAR bit never leaves the variable node

## Type Safety Rules

### Enforced Constraints
1. **Boolean contexts**: VAR variable must currently hold BIT type
   - `VAR x = 5; IF x THEN...` → Type mismatch
   
2. **Type comparisons**: Follow underlying type rules
   - `VAR a = 5; VAR b = "test"; IF a = b...` → Type mismatch
   
3. **Built-in functions**: Must match expected parameter types
   - VAR variable's current underlying type must match

4. **Operations**: Standard type compatibility applies
   - VAR variables contribute their underlying type to expressions

## Current Status

### Implemented
- ✅ BASICType.VAR defined as 0x10
- ✅ VAR token parsing
- ✅ Variable storage structure supports packed type byte
- ✅ String memory management infrastructure

### Not Implemented  
- ❌ VAR bit checking in `Variables.SetValue()`
- ❌ Type update logic preserving VAR bit
- ❌ VAR bit masking in `Variables.GetValue()`
- ❌ Initial VAR variable declaration handling

## Testing Scenarios

```basic
' Basic type changes
VAR x
x = 100        ' x becomes VAR|INT
x = "hello"    ' x becomes VAR|STRING (free old value, allocate string)
x = 50000      ' x becomes VAR|WORD (free string, store number)

' Type safety
VAR y = 5
IF y THEN      ' ERROR: Type mismatch (INT not BIT)

' Function arguments (future)
FUNC test(a)   ' 'a' implicitly has VAR bit set
  a = "str"    ' OK - can change type
  a = 123      ' OK - can change type again
ENDFUNC
```

## Implementation Priority
1. First: Add VAR bit checking to assignment path
2. Second: Ensure proper type masking when reading  
3. Third: Verify STRING memory management works correctly
4. Future: Extend to function arguments and locals