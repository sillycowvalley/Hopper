# Hopper BASIC Open Issues

## SHOWSTOPPERS
*Crashes, hangs, or data corruption that prevent basic use*

### 1. FOR/NEXT Type Overflow Infinite Loop
**Symptom:** WORD/BYTE iterators wrap at max value causing infinite loops  
**Reproduce:**
```basic
FUNC Test()
    VAR w
    FOR w = 65534 TO 65536  ! Wraps at 65535
        PRINT w
    NEXT w
    PRINT W
ENDFUNC
```
**Status:** ACTIVE  
**Proposed Fix:** Limit FORITF optimization to TO < 254 (BYTE) and TO < 65534 (WORD)

### 2. STEP 0 Infinite Loop
**Symptom:** No error checking for STEP 0  
**Reproduce:**
```basic
FOR i = 1 TO 5 STEP 0
    PRINT i    ! Infinite loop
NEXT i
```
**Status:** ACTIVE  
**Note:** Classic BASIC compatibility issue - error vs. infinite loop

---

## CRITICAL BUGS
*Core functionality completely broken*

### 3. CHR() Function TYPE MISMATCH Error
**Symptom:** CHR() function fails with TYPE MISMATCH when passed LONG values  
**Reproduce:**
```basic
FUNC TestChr()
    VAR val = 65
    PRINT CHR(val)    ! TYPE MISMATCH error
ENDFUNC
```
**Error:** `?TYPE MISMATCH` during CHR() call  
**Status:** ACTIVE  
**Impact:** CRITICAL - Cannot convert LONG to CHAR, breaks fundamental type conversion
**Note:** This breaks the primary conversion mechanism between numeric and character types

### 4. PEEK/POKE Memory Access VALUE OUT OF RANGE
**Symptom:** PEEK/POKE functions fail with hex addresses  
**Reproduce:**
```basic
FUNC TestMemory()
    VAR addr = 0x0B40
    PRINT PEEK(addr)      ! VALUE OUT OF RANGE error
    POKE(addr, 42)        ! VALUE OUT OF RANGE error
ENDFUNC
```
**Error:** `?VALUE OUT OF RANGE`  
**Status:** ACTIVE  
**Impact:** CRITICAL - Memory access functions unusable with standard hex addresses
**Note:** Hex parsing works fine, but address validation appears broken

### 5. Hardware I/O READ() Function TYPE MISMATCH
**Symptom:** READ() function fails with TYPE MISMATCH  
**Reproduce:**
```basic
FUNC TestRead()
    PINMODE(0, 0)         ! Works fine
    PRINT READ(0)         ! TYPE MISMATCH error
ENDFUNC
```
**Error:** `?TYPE MISMATCH` during READ() call  
**Status:** ACTIVE  
**Impact:** CRITICAL - Digital input completely broken
**Note:** PINMODE and WRITE work fine, only READ() affected

---

## BUGS
*Wrong behavior but recoverable*

### 6. Boolean Literal Type Resolution in IF Conditions
**Symptom:** IF statements with literal TRUE/FALSE fail with TYPE MISMATCH  
**Reproduce:**
```basic
FUNC TestLiteralBoolean()
    IF TRUE THEN          ! TYPE MISMATCH here
        PRINT "Should work"
    ENDIF
    IF FALSE THEN         ! TYPE MISMATCH here  
        PRINT "Also should work"
    ENDIF
ENDFUNC
```
**Error:** `?TYPE MISMATCH` at IF condition evaluation  
**Status:** ACTIVE  
**Impact:** Cannot use literal TRUE/FALSE in IF conditions, must assign to BIT variable first
**Workaround:** `VAR flag = TRUE; IF flag THEN...` works fine

### 7. String Indexing Returns ASCII Value Instead of Character
**Symptom:** String indexing operation returns LONG ASCII value instead of CHAR  
**Reproduce:**
```basic
FUNC TestStringIndex()
    VAR text = "BASIC"
    VAR first = text[0]     ! Should get 'B' but gets 66
    PRINT "first="; first; " ! expect B but shows 66"
ENDFUNC
```
**Expected:** `first=B` (CHAR type)  
**Actual:** `first=66` (LONG ASCII value)  
**Status:** ACTIVE  
**Impact:** String indexing produces wrong type, affects character processing

### 8. CHR() Function Result Type Incompatibility
**Symptom:** CHR() function result not recognized as CHAR type for comparisons  
**Reproduce:**
```basic
FUNC TestChr()
    VAR c = 'M'
    VAR val = 65
    PRINT CHR(val) < c    ! TYPE MISMATCH error
ENDFUNC
```
**Error:** `?TYPE MISMATCH` during comparison evaluation  
**Status:** ACTIVE  
**Impact:** Cannot directly compare CHR() results with CHAR variables
**Note:** CHR() converts LONG to CHAR but result isn't typed correctly for comparison operators
**Workaround:** Assign CHR() result to variable first: `VAR temp = CHR(val); PRINT temp < c`

### 9. LONG Arithmetic Overflow (Silent Wraparound)
**Symptom:** 32-bit LONG arithmetic silently wraps on overflow without warning  
**Reproduce:**
```basic
VAR bigNum = 1000000
bigNum = bigNum * 2000  ! Should be 2,000,000,000
PRINT bigNum            ! Shows -2000000000 (wrapped)
```
**Expected:** `2000000000` or overflow error  
**Actual:** `-2000000000` (silent wraparound)  
**Status:** ACTIVE  
**Impact:** Mathematical calculations can produce incorrect results without warning
**Note:** Max LONG = 2,147,483,647; result exceeded and wrapped to negative

### 10. Negative Start FOR Loop Failure
**Symptom:** FOR loops with negative start values fail to execute completely  
**Reproduce:**
```basic
FUNC TestNegative()
    FOR i = -2 TO 2        ! Loop body never executes
        PRINT i;           ! No output produced
    NEXT i
    PRINT " ! expect -2 -1 0 1 2"
ENDFUNC
```
**Expected:** `-2 -1 0 1 2`  
**Actual:** No output (loop body never executes)  
**Status:** ACTIVE  
**Impact:** Cannot iterate through ranges with negative starting values
**Note:** FORCHK opcode logic appears to fail condition evaluation for negative start values

### 11. Local Variable Limit in FOR Loop Bodies
**Symptom:** Cannot declare new VAR variables within FOR loop body  
**Reproduce:**
```basic
FUNC TestLoop()
    FOR i = 1 TO 5
        VAR temp = i * 2    ! NO MORE LOCALS error
        PRINT temp
    NEXT i
ENDFUNC
```
**Error:** `?NO MORE LOCALS`  
**Status:** ACTIVE  
**Impact:** Variables must be declared before FOR loops, limiting dynamic local declarations
**Workaround:** Declare all needed variables at function start

### 12. Comment Parsing Issues
**Symptom:** SYNTAX ERROR when comments follow certain statements  
**Reproduce:**
```basic
PRINT ! Empty line
PRINT "text" ! comment
VARS ! comment  
VAR uninitVar ! comment on uninitialized VAR
MEM ! comment
```
**Error:** `?SYNTAX ERROR` (various error codes)  
**Status:** ACTIVE  
**Note:** Comments work fine on separate lines - appears to be tokenizer issue with inline comments after specific statement types

### 13. Colon Statements in Multiline IF Blocks
**Symptom:** Parser fails when colon-separated statements used in multiline IF context  
**Reproduce:**
```basic
IF x > 5 THEN a = 1 : b = 2 : PRINT "test"
ELSE a = 3 : b = 4 : PRINT "else"
ENDIF
```
**Error:** `?ENDIF EXPECTED (0xB36F)`  
**Status:** ACTIVE  
**Note:** Colon statements work fine outside IF blocks

---

## UNIMPLEMENTED FEATURES
*Known missing functionality marked as placeholders*

### 14. ABS() Function Not Implemented
**Symptom:** ABS() returns "NOT IMPLEMENTED" message  
**Reproduce:**
```basic
PRINT ABS(-42)    ! Returns "? NOT IMPLEMENTED"
```
**Status:** EXPECTED - Phase 5 feature  
**Impact:** No absolute value function available
**Priority:** LOW - can work around with IF statements

### 15. RND() Function Not Implemented  
**Symptom:** RND() returns "NOT IMPLEMENTED" message  
**Reproduce:**
```basic
PRINT RND(10)     ! Returns "? NOT IMPLEMENTED"
```
**Status:** EXPECTED - Phase 5 feature  
**Impact:** No random number generation available
**Priority:** LOW - not essential for basic programming

---

## ANNOYANCES
*Minor issues, cosmetic, or nice-to-haves*

### 16. CHAR Ordered Comparison Error Message
**Symptom:** STRING comparison gives INVALID OPERATOR instead of TYPE MISMATCH  
**Reproduce:**
```basic
CHAR c = 'A'
STRING s = "A"
PRINT c >= s
```
**Error:** `?INVALID OPERATOR` (should be `?TYPE MISMATCH` for consistency)  
**Status:** ACTIVE

### 17. Underscore Identifiers Not Supported
**Symptom:** Variable names with underscores rejected  
**Reproduce:**
```basic
INT global_counter = 0
```
**Error:** `?SYNTAX ERROR (0x9981)`  
**Status:** WORKING AS DESIGNED  
**Note:** Aesthetic choice - underscores not allowed in identifiers

---

## RESOLVED ISSUES
*Recently fixed bugs*

### ✅ String Variable Corruption in Deep Nesting (FIXED)
**Previous Symptom:** String variables lost content in deeply nested IF statements  
**Previous Result:** `path=ue` instead of `path=ABCE`  
**Current Status:** **RESOLVED** - Deep nesting now works correctly
**Test Result:** `path=ABCE ! expect ABCE` ✅

---

## ISSUE PRIORITY SUMMARY

**IMMEDIATE ACTION REQUIRED:**
1. **CHR() Function** - Core type conversion broken (Issue #3)
2. **PEEK/POKE Memory Access** - Essential debugging tool broken (Issue #4) 
3. **Hardware READ()** - Digital input completely broken (Issue #5)

**HIGH PRIORITY:**
4. **FOR Loop Infinite Loops** - Data corruption risk (Issue #1, #2)
5. **Negative FOR Loops** - Basic loop functionality broken (Issue #10)

**MEDIUM PRIORITY:**
6. **Boolean Literals in IF** - Common syntax pattern broken (Issue #6)
7. **String Indexing Type** - Wrong result type (Issue #7)

**LOW PRIORITY:**
8. Parser edge cases, local variable limits, comment parsing
9. Unimplemented functions (ABS, RND)
10. Cosmetic issues

---

**Test Suite Status**: Comprehensive testing revealed that several "COMPLETE" features from Phase 4 (Hardware I/O) and core functions are actually broken. The type system implementation appears to have issues with function parameter and return value type checking.