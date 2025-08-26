# Hopper BASIC Open Issues

## SHOWSTOPPERS
*Crashes, hangs, or data corruption*

### 1. FOR/NEXT Type Overflow Infinite Loop
**Symptom:** WORD/BYTE iterators wrap at max value causing infinite loops  
**Reproduce:**
```basic
FUNC Test()
    WORD w
    FOR w = 65534 TO 65536  ! Wraps at 65535
        PRINT w
    NEXT w
ENDFUNC
```
**Status:** ACTIVE  
**Proposed Fix:** Limit FORITF optimization to TO < 254 (BYTE) and TO < 65534 (WORD)

---

## BUGS
*Wrong behavior but recoverable*

### 2. Boolean Literal Type Resolution in IF Conditions
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
**Note:** `VAR flag = TRUE; IF flag THEN...` works fine

### 3. String Indexing Returns ASCII Value Instead of Character
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

### 4. LONG Arithmetic Overflow (Silent Wraparound)
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

### 5. Comment Parsing Issues
**Symptom:** SYNTAX ERROR when comments follow certain statements  
**Reproduce:**
```basic
PRINT ! Empty line
PRINT "text" ! comment
VARS ! comment  
VAR uninitVar ! comment on uninitialized VAR
```
**Error:** `?SYNTAX ERROR` (various error codes)  
**Status:** ACTIVE  
**Note:** Comments work fine on separate lines - appears to be tokenizer issue with inline comments after specific statement types

### 6. Colon Statements in Multiline IF Blocks
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

### 7. STEP 0 Infinite Loop
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

## RESOLVED ISSUES
*Recently fixed bugs*

### ✅ String Variable Corruption in Deep Nesting (FIXED)
**Previous Symptom:** String variables lost content in deeply nested IF statements  
**Previous Result:** `path=ue` instead of `path=ABCE`  
**Current Status:** **RESOLVED** - Deep nesting now works correctly
**Test Result:** `path=ABCE ! expect ABCE` ✅

---

## ANNOYANCES
*Minor issues, cosmetic, or nice-to-haves*

### 8. CHAR Ordered Comparison Error Message
**Symptom:** STRING comparison gives INVALID OPERATOR instead of TYPE MISMATCH  
**Reproduce:**
```basic
CHAR c = 'A'
STRING s = "A"
PRINT c >= s
```
**Error:** `?INVALID OPERATOR` (should be `?TYPE MISMATCH` for consistency)  
**Status:** ACTIVE

### 9. Underscore Identifiers Not Supported
**Symptom:** Variable names with underscores rejected  
**Reproduce:**
```basic
INT global_counter = 0
```
**Error:** `?SYNTAX ERROR (0x9981)`  
**Status:** WORKING AS DESIGNED  
**Note:** Aesthetic choice - underscores not allowed in identifiers