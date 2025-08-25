# Hopper BASIC Testing Documentation v3.0
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

**Example of Progressive Isolation with New Type System**:
```basic
! BAD: Testing everything at once
BEGIN
    ! Tests all types - can't identify failure
    PRINT long_test; char_test; bit_test; string_test
END

! GOOD: Progressive isolation
! Step 1: Test LONG variables only
NEW
VAR n = 42    ! LONG via type inference
BEGIN
    PRINT "LONG: "; n; " ! expect 42"
END
RUN

! Step 2: If that works, add CHAR type
NEW
VAR n = 42    ! LONG
VAR c = 'A'   ! CHAR via type inference
BEGIN
    PRINT "LONG: "; n; " ! expect 42"
    PRINT "CHAR: "; c; " ! expect A"
END
RUN

! Step 3: On failure, isolate to minimum
NEW
VAR v = 42     ! Minimal failing case found
v = 'X'        ! Type change CHAR-LONG incompatible
BEGIN
END
RUN
```

### 3. Type System Testing Focus Areas

#### Type Inference Testing
```basic
! Test VAR type inference
NEW
VAR num = 123      ! Should infer LONG
VAR letter = 'Z'   ! Should infer CHAR  
VAR flag = TRUE    ! Should infer BIT
VAR text = "HI"    ! Should infer STRING
BEGIN
    PRINT num; letter; flag; text
END
RUN
VARS  ! Verify inferred types
```

#### Strict Type Boundary Testing
```basic
! Test type incompatibility - these should ERROR
NEW
VAR lng = 100     ! LONG
VAR chr = 'A'     ! CHAR
BEGIN
    PRINT lng = chr    ! Should give TYPE MISMATCH
END
RUN
```

#### LONG-Only Numeric Operations
```basic
! All numeric variables are LONG now
NEW
VAR x = 2000000000    ! Large LONG value
VAR y = 1000000000    ! Large LONG value  
BEGIN
    PRINT x + y        ! 32-bit LONG arithmetic
    PRINT x * 2        ! Should handle large values
END
RUN
```

#### Array Range Testing
```basic
! Arrays declared with compact types for memory, but accessed with LONG values
NEW
BYTE data[10]         ! BYTE array (0-255 range)
VAR index = 5         ! LONG index variable
VAR value = 255       ! LONG value
BEGIN
    data[index] = value    ! LONG value stored in BYTE element - OK
    PRINT data[5]          ! expect 255
    
    value = 300            ! LONG value too big for BYTE
    data[index] = value    ! Should give OVERFLOW error
END
RUN
```

### 4. Memory and State Verification

**Essential Diagnostic Pattern**:
```basic
MEM                  ! Check memory before
! ... test code ...
NEW                  ! Clear everything
MEM                  ! Verify no memory leak
```

**State Verification Tools**:
- **MEM**: Check for memory leaks between tests
- **VARS**: Verify global state and type evolution (now shows LONG, CHAR, BIT, STRING)
- **FUNCS**: Confirm function definitions loaded correctly
- **LIST**: Full program verification

### 5. Expected Type System Behaviors

#### VAR Type Evolution (Should Work)
```basic
NEW
VAR v                 ! Defaults to LONG:0
v = 42               ! Stays LONG
v = -2000000000      ! Large negative LONG
BEGIN
    PRINT v; " ! expect -2000000000"
END
RUN
```

#### VAR Type Changes (Should ERROR)
```basic
NEW
VAR v = 42           ! LONG
BEGIN
    v = 'A'          ! Should give TYPE MISMATCH error
    PRINT v
END
RUN
```

#### Conversion Function Testing
```basic
NEW
VAR lng = 65         ! LONG
VAR chr = 'A'        ! CHAR
BEGIN
    PRINT ASC(chr)   ! CHAR -> LONG conversion
    PRINT CHR(lng)   ! LONG -> CHAR conversion  
    PRINT lng = ASC(chr)  ! Should be TRUE (65 = 65)
END
RUN
```

### 6. Array Testing with LONG Values and Range Checking

#### Array Declaration vs Access Pattern
```basic
NEW
BYTE small_array[5]      ! Declared as BYTE for memory efficiency
VAR index = 2            ! LONG index
VAR value = 100          ! LONG value
BEGIN
    small_array[index] = value   ! LONG value assigned to BYTE element
    PRINT small_array[2]         ! expect 100
END
RUN
```

#### Array Range Overflow Testing
```basic
NEW
BYTE data[3]             ! BYTE array (0-255 range)
WORD wide[3]             ! WORD array (0-65535 range)  
INT signed[3]            ! INT array (-32768 to 32767 range)
VAR index = 0            ! LONG index

BEGIN
    ! Test successful assignments
    data[index] = 255           ! Max BYTE - OK
    wide[index] = 65535         ! Max WORD - OK  
    signed[index] = -32768      ! Min INT - OK
    
    ! Test overflow errors (should error cleanly)
    data[index] = 256           ! BYTE overflow - should error
    wide[index] = 65536         ! WORD overflow - should error
    signed[index] = 32768       ! INT overflow - should error
END
RUN
```

#### Intrinsic Range Testing (Similar Pattern)
```basic
NEW
VAR good_val = 65        ! LONG in valid range
VAR bad_val = 300        ! LONG out of range
BEGIN
    PRINT CHR(good_val)  ! expect 'A' (65 is valid 0-255)
    PRINT CHR(bad_val)   ! Should give OVERFLOW error (300 > 255)
END
RUN
```

### 7. Comparison Testing

#### Same-Type Comparisons (Legal)
```basic
NEW
VAR a = 1000000      ! LONG
VAR b = 2000000      ! LONG  
VAR c1 = 'A'         ! CHAR
VAR c2 = 'Z'         ! CHAR
VAR flag1 = TRUE     ! BIT
VAR flag2 = FALSE    ! BIT
BEGIN
    PRINT a < b      ! LONG comparison - expect TRUE
    PRINT c1 < c2    ! CHAR comparison - expect TRUE  
    PRINT flag1 = flag2  ! BIT comparison - expect FALSE
END
RUN
```

#### Cross-Type Comparisons (Should ERROR)
```basic
NEW
VAR lng = 65         ! LONG
VAR chr = 'A'        ! CHAR (ASCII 65)
BEGIN
    PRINT lng = chr  ! Should give TYPE MISMATCH error
END
RUN
```

### 8. Output Validation

**Expected Output Format**:
- Always include expected values directly in output
- Format: `value ! expect expected`
- Makes test self-documenting and failures obvious

**Type System Error Detection**:
Watch for TYPE MISMATCH errors with specific error codes:
```
?TYPE MISMATCH (0x9F36)
```

### 9. Crash Type Classification

**Document the TYPE of failure, not just that it failed**:

1. **Type Mismatch Error**: Clean error with specific code
   - Example: `?TYPE MISMATCH (0x9F36)`  
   - System remains stable, expected behavior

2. **Clean Error**: Program stops with error message and code
   - Example: `?BOUNDS ERROR (0x9F38)`
   - System remains stable

3. **Clean Crash**: Program stops with crash dump
   - Example: `== CRASH == SP:CF`
   - May need NEW to recover

4. **Hang**: Program enters infinite loop
   - Requires ^C interrupts to break
   - Often indicates type system bug

### 10. Global Variable Re-initialization Behavior

**Behavior with Simplified Type System**:

```basic
NEW
VAR initialized = 1000000    ! LONG, will reset to 1000000 on RUN
VAR uninitialized           ! LONG, will reset to 0 on RUN
CONST c = 'A'               ! CHAR constant, immutable

! Modify before RUN
initialized = -5000000
uninitialized = 999999

BEGIN
    PRINT initialized; " ! expect 1000000"    ! Resets to declaration value
    PRINT uninitialized; " ! expect 0"       ! Resets to LONG default
    PRINT c; " ! expect A"                   ! Constants never change
END
RUN
```

### 11. Known Issues and Workarounds

**Issue #1**: VAR type change crashes
- **Symptom**: Crash when VAR changes type between incompatible types
- **Status**: SHOULD BE FIXED with strict type system
- **Test**: Try changing LONG VAR to CHAR - should give TYPE MISMATCH error now

**Issue #2**: Large LONG value handling
- **Status**: UNKNOWN - needs testing with 32-bit values
- **Test range**: Values near ±2,147,483,648 boundary

### 12. Test Suite Architecture for Simplified Types

**Recommended Structure**:
```basic
! Phase 1: Individual type tests
NEW
! Test LONG variables only
VAR n = 1234567890
BEGIN
    PRINT n; " ! expect 1234567890"
END
RUN
VARS
MEM

NEW  
! Test CHAR variables only
VAR c = 'X'
BEGIN
    PRINT c; " ! expect X"
END
RUN
VARS
MEM

NEW
! Test BIT variables only  
VAR flag = TRUE
BEGIN
    PRINT flag; " ! expect TRUE"
END
RUN
VARS
MEM

NEW
! Test STRING variables only
VAR text = "HELLO"
BEGIN
    PRINT text; " ! expect HELLO"
END
RUN
VARS
MEM

! Phase 2: Type boundary tests (should error cleanly)
NEW
VAR lng = 100
VAR chr = 'A'
BEGIN
    PRINT lng = chr    ! Should give TYPE MISMATCH
END
RUN
```

### 13. Testing Checklist Template

For the simplified type system:

#### Core Type Tests
- [ ] LONG variables with large values (±2 billion range)
- [ ] CHAR variables with full ASCII range (0-255 via CHR)
- [ ] BIT variables with TRUE/FALSE only
- [ ] STRING variables with various text content

#### Type Inference Tests  
- [ ] `VAR n = 42` correctly infers LONG
- [ ] `VAR c = 'A'` correctly infers CHAR
- [ ] `VAR b = TRUE` correctly infers BIT  
- [ ] `VAR s = "X"` correctly infers STRING

#### Type Boundary Tests (Should ERROR)
- [ ] LONG = CHAR comparison (should give TYPE MISMATCH)
- [ ] BIT = LONG comparison (should give TYPE MISMATCH)  
- [ ] STRING = CHAR comparison (should give TYPE MISMATCH)
- [ ] Cross-type assignments (should give TYPE MISMATCH)

#### Conversion Function Tests
- [ ] `ASC(char_var)` returns LONG value
- [ ] `CHR(long_var)` returns CHAR value
- [ ] CHR bounds checking (LONG must be 0-255)

#### Array Tests with LONG Values
- [ ] All array types with LONG index variables
- [ ] LONG values within array element range (should succeed)
- [ ] LONG values exceeding array element range (should give OVERFLOW error)
- [ ] Large LONG indices (bounds checking vs overflow checking)
- [ ] Array element type preservation despite LONG access pattern

#### Intrinsic Range Tests (Similar to Array Overflow)
- [ ] CHR(LONG) with values 0-255 (should succeed)
- [ ] CHR(LONG) with values > 255 (should give OVERFLOW error)
- [ ] Other range-limited intrinsics with boundary testing

#### 32-bit LONG Arithmetic
- [ ] Large number addition/subtraction
- [ ] Large number multiplication (overflow behavior)
- [ ] Large number division
- [ ] Negative number handling at extremes

#### Memory Leak Tests
- [ ] MEM before/after for each type
- [ ] Array allocation/deallocation  
- [ ] Large LONG value storage

---

## Key Changes from Previous Version

### Obsolete Testing Areas (No Longer Relevant)
- ❌ INT, WORD, BYTE variable testing (these are array-only now)
- ❌ Type promotion between numeric types (no promotion exists)
- ❌ Cross-numeric-type comparisons (BYTE-WORD, etc.) 
- ❌ Automatic type widening
- ❌ Mixed numeric type arithmetic

### New Focus Areas
- ✅ LONG as single numeric type for variables
- ✅ Strict type boundaries with no promotion
- ✅ Type inference via VAR declarations
- ✅ ASC/CHR as only conversion between CHAR and LONG
- ✅ Array storage efficiency vs. variable simplicity
- ✅ 32-bit arithmetic and large number handling

### Testing Philosophy Changes
- **Before**: Test complex type promotion rules and interactions
- **After**: Test strict type boundaries and clean error handling
- **Before**: Verify automatic conversions work correctly  
- **After**: Verify automatic conversions are properly blocked
- **Before**: Test edge cases in multiple numeric types
- **After**: Test edge cases in single LONG type with 32-bit range

---

## Example Test Suite

```basic
! Hopper BASIC v3.0 Type System Test Suite
CLS

! Test 1: LONG variable operations
NEW
VAR big = 2000000000     ! Large LONG
VAR small = -1000000000  ! Large negative LONG
BEGIN
    PRINT "LONG Test:"
    PRINT big + small; " ! expect 1000000000"
    PRINT big * 2; " ! expect 4000000000 or overflow"
END
RUN
VARS

! Test 2: Type inference verification
NEW
VAR num = 42        ! Should be LONG
VAR chr = 'Z'       ! Should be CHAR
VAR bit = TRUE      ! Should be BIT  
VAR str = "TEST"    ! Should be STRING
BEGIN
    PRINT "Type inference test - check VARS"
END
RUN
VARS

! Test 3: Type boundary enforcement (should error)
NEW
VAR lng = 100
VAR chr = 'A'
BEGIN
    PRINT "Testing type boundaries (expect errors):"
    PRINT lng = chr     ! Should give TYPE MISMATCH
END
RUN

! Test 4: Conversion functions
NEW
VAR ascii_val = 65   ! LONG
VAR letter = 'B'     ! CHAR  
BEGIN
    PRINT "Conversions:"
    PRINT CHR(ascii_val); " ! expect A"
    PRINT ASC(letter); " ! expect 66"
END
RUN

! Test 5: Array with LONG values and range checking
NEW
BYTE data[5]        ! BYTE array (0-255 range)
VAR index = 2       ! LONG index
VAR good_val = 200  ! LONG value in range
VAR bad_val = 300   ! LONG value out of range
BEGIN
    data[index] = good_val      ! Should succeed
    PRINT data[2]; " ! expect 200"
    data[index] = bad_val       ! Should give OVERFLOW error
END
RUN

! Test 6: Intrinsic range checking (similar pattern)
NEW  
VAR ascii = 65      ! LONG in valid CHR range
VAR too_big = 300   ! LONG out of CHR range
BEGIN
    PRINT CHR(ascii); " ! expect A"
    PRINT CHR(too_big)          ! Should give OVERFLOW error
END
RUN

! Memory verification
NEW  
MEM
```

---

## Key Lessons for Simplified Type System

1. **Strict Boundaries**: No automatic conversions - test that errors occur where expected
2. **Type Inference**: VAR declarations now critical - verify correct type inference
3. **LONG Everywhere**: All numeric variables and operations are 32-bit - test large value handling  
4. **Array Declaration vs Access**: Arrays declared with compact types for memory, but accessed with LONG values
5. **Range Checking**: LONG values get range-checked when stored in smaller array elements or passed to intrinsics
6. **Clean Errors**: Type mismatches should give TYPE MISMATCH errors, range violations should give OVERFLOW errors
7. **Conversion Functions**: ASC/CHR are the ONLY way to convert CHAR↔LONG, with CHR doing range checking

---

*Document Version: 3.0*  
*Major Update: Completely redesigned for simplified LONG-based type system*  
*Platform: 6502 Cycle-Accurate Emulator*  
*Hopper BASIC Version: 3.0 - Simplified Type System*