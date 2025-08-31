# Hopper BASIC Testing Documentation v3.1
## Simplified Type System Edition

## Major Type System Changes

### 🚨 CRITICAL: Simplified Type System Overview
**The type system has been completely redesigned around strict type boundaries with no automatic promotion.**

### Core Types for Variables (via VAR with Type Inference)
- **LONG**: 32-bit signed integer (-2,147,483,648 to 2,147,483,647) - **The only numeric type for variables**
- **CHAR**: 8-bit character (ASCII 0-255) - Character operations only
- **BIT**: Pure boolean (TRUE/FALSE only) - Logical operations only  
- **STRING**: Immutable strings - Text operations only

### Array Types (Compact Declaration, LONG Access)
Arrays use compact types for memory efficiency but all access uses LONG values:
- **`BIT name[size]`** - Packed boolean array, accessed with LONG (0=FALSE, 1=TRUE, overflow error for other values)
- **`CHAR name[size]`** - Character array, accessed with LONG (0-255 range, overflow error beyond)  
- **`BYTE name[size]`** - 8-bit unsigned array, accessed with LONG (0-255 range, overflow error beyond)
- **`WORD name[size]`** - 16-bit unsigned array, accessed with LONG (0-65535 range, overflow error beyond)
- **`INT name[size]`** - 16-bit signed array, accessed with LONG (-32768 to 32767 range, overflow error beyond)

### Strict Type Compatibility Rules
- **LONG ↔ LONG only** - No automatic promotion from smaller types
- **CHAR ↔ CHAR only** - Use ASC()/CHR() for LONG conversion
- **BIT ↔ BIT only** - Pure boolean logic
- **STRING ↔ STRING only** - Text operations only
- **Array indices are always LONG** - All indexing uses 32-bit arithmetic

---

## Testing Constraints and Guidelines

### 1. Buffer Limitations
**Critical Constraint**: Each function body (including BEGIN/END blocks) is limited to 512 characters maximum.

**Implications**:
- Test suites must be modular with small, focused functions
- Complex tests must be decomposed into multiple function calls
- Comments should be minimal within test functions
- Use short but clear variable names and messages

**Best Practice**: Aim for functions under 400 characters to leave safety margin.

### 2. Progressive Isolation Testing Methodology

**Critical Lesson**: Test writing IS debugging. Start with isolated tests for each feature before combining.

**Progressive Isolation Protocol**:
1. **Start Minimal**: Test one variable type, one feature, one operation at a time
2. **Isolate on Failure**: When a combined test fails, immediately break it down
3. **Binary Search**: Comment out half the test to identify which part fails
4. **Reduce to Minimum**: Keep removing code until you have the smallest failing case

**Example of Progressive Isolation**:
```basic
! Step 1: Test basic arithmetic
NEW
BEGIN
    PRINT "is "; 2 + 3; ", expect 5"
END
RUN

! Step 2: Test basic bitwise  
NEW
BEGIN
    PRINT "is "; 3 & 4; ", expect 0"
END
RUN

! Step 3: Test combined (where precedence fails)
NEW
BEGIN
    PRINT "is "; 2 + 3 & 4; ", expect 2"
    PRINT "is "; 2 + (3 & 4); ", expect 2 (explicit)"
END
RUN
```

### 3. **Minimal Failure Testing Format**

**Best Practice**: Use the concise "is/expect" format for failure testing:

```basic
NEW
FUNC TestFeature()
    PRINT "=== TESTING FEATURE ==="
    
    PRINT "is "; expression; ", expect result"     ! Clear expectation
    PRINT "is "; explicit_version; ", expect result (explicit)"  ! Verification
    
    ! Comment out unimplemented features:
    ! PRINT "is "; unimplemented_expr; ", expect result"
ENDFUNC
BEGIN
    TestFeature()
END
```

**Key Benefits**:
- **Clear failure identification**: "is 4, expect 2" immediately shows the problem
- **Explicit versions**: Parenthesized versions prove what should happen
- **Commented missing features**: Shows what needs implementation
- **Compact format**: Fits within buffer constraints

### 4. **Operator Testing Strategy**

#### General Precedence Testing Pattern
```basic
! Test operator precedence with clear expectations
PRINT "is "; a + b * c; ", expect result"     ! Tests precedence
PRINT "is "; a + (b * c); ", expect result (explicit)"  ! Verification

! Test associativity
PRINT "is "; a - b - c; ", expect result"     ! Left-to-right
PRINT "is "; (a - b) - c; ", expect result (explicit)"

! Test complex expressions
PRINT "is "; complex_expr; ", expect result"
PRINT "is "; explicit_parentheses; ", expect result (explicit)"
```

#### Precedence Verification Pattern
```basic
! Always include explicit parentheses version to prove correctness:
PRINT "is "; test_expression; ", expect value"
PRINT "is "; (explicit) + (grouping); ", expect value (explicit)"
```

### 5. Missing Feature Testing

#### Testing Unimplemented Operators
```basic
! Comment out until implemented:
! PRINT "is "; new_operator_expr; ", expect result"
! PRINT "is "; complex_new_expr; ", expect result"

! Test framework ready for when feature is implemented
FUNC TestNewOperator()
    ! PRINT "is "; operator_test; ", expect result"
    PRINT "Operator not implemented yet"
ENDFUNC
```

### 6. Memory and State Verification

**Essential Diagnostic Pattern**:
```basic
MEM                  ! Check memory before
! ... test code ...
NEW                  ! Clear everything
MEM                  ! Verify no memory leak
```

### 7. Type System Testing Focus Areas

#### Type Inference Testing
```basic
NEW
VAR num = 123      ! Should infer LONG
VAR letter = 'Z'   ! Should infer CHAR  
VAR flag = TRUE    ! Should infer BIT
VAR text = "HI"    ! Should infer STRING
BEGIN
    PRINT "Types inferred correctly"
END
RUN
VARS  ! Verify inferred types
```

#### Strict Type Boundary Testing
```basic
NEW
VAR lng = 100     ! LONG
VAR chr = 'A'     ! CHAR
BEGIN
    PRINT "is "; lng = chr; ", expect TYPE MISMATCH"
END
RUN
```

#### LONG-Only Numeric Operations
```basic
NEW
VAR x = 2000000000    ! Large LONG value
VAR y = 1000000000    ! Large LONG value  
BEGIN
    PRINT "is "; x + y; ", expect 3000000000 or overflow"
    PRINT "is "; x * 2; ", expect 4000000000 or overflow"
END
RUN
```

### 8. Output Validation

**Expected Output Format**:
- **Failure format**: `is 4, expect 2` (shows mismatch clearly)
- **Success format**: `value expect value` (shows working correctly)  
- **Error format**: `expect TYPE MISMATCH` (documents expected errors)

**Type System Error Detection**:
Watch for specific error patterns:
```
?SYNTAX ERROR (0x92C1)     # Missing ~ operator
?TYPE MISMATCH (0x9F36)    # Type boundary violations
```

### 9. Crash Type Classification

**Document the TYPE of failure, not just that it failed**:

1. **Syntax Error**: Missing operator/feature
   - Example: `?SYNTAX ERROR (0x92C1)` for `~` operator
   - System remains stable, feature not implemented

2. **Logic Error**: Wrong precedence 
   - Example: `is 4, expect 2` 
   - System stable, wrong parsing order

3. **Type Mismatch Error**: Clean error with specific code
   - Example: `?TYPE MISMATCH (0x9F36)`  
   - System remains stable, expected behavior

4. **Clean Crash**: Program stops with crash dump
   - Example: `== CRASH == SP:CF`
   - May need NEW to recover

### 10. **Complete Test Template**

```basic
CLS
NEW
MEM

! Test specific functionality
FUNC TestFeature()
    PRINT "=== TESTING FEATURE ==="
    
    ! Test basic operations
    PRINT "is "; basic_expr; ", expect result"
    PRINT "is "; explicit_version; ", expect result (explicit)"
    
    ! Test complex cases
    PRINT "is "; complex_expr; ", expect result"
    PRINT "is "; explicit_complex; ", expect result (explicit)"
ENDFUNC

! Test what works correctly
FUNC TestWorking()
    PRINT "=== WORKING CORRECTLY ==="
    PRINT "is "; working_expr1; ", expect result"
    PRINT "is "; working_expr2; ", expect result" 
    PRINT "is "; (parenthesized); ", expect result"   ! Verify parentheses work
ENDFUNC

BEGIN
    TestFeature()
    PRINT
    TestWorking()
END
RUN

NEW
MEM
```

---

## Key Testing Principles

### **1. Minimal and Focused**
- **"is X, expect Y"** format makes failures immediately obvious
- **Explicit parentheses versions** prove what should happen  
- **Commented missing features** show implementation needs

### **2. Progressive Isolation**
- Test one feature at a time
- Break down complex failures into minimal cases
- Use explicit parentheses to verify expected behavior

### **3. Evidence-Based Testing**
- Show exact mismatches with expected values
- Provide verification through explicit grouping
- Document both working and failing cases

### **4. Implementation-Ready**
- Test frameworks ready for new features
- Clear expectations for when fixes are applied
- Modular tests that can be enabled as features are implemented

---

*Document Version: 3.1*  
*Focus: General testing methodology for ongoing development*  
*Platform: 6502 Cycle-Accurate Emulator*  
*Hopper BASIC Version: 3.0+ - Simplified Type System*