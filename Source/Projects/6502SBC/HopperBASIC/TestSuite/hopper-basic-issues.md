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

## BUGS
*Wrong behavior but recoverable*

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

## ANNOYANCES
*Minor issues, cosmetic, or nice-to-haves*

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

## ISSUE PRIORITY SUMMARY

**HIGH PRIORITY:**
1. **FOR Loop Infinite Loops** - Data corruption risk (Issue #1, #2)
2. **LONG Arithmetic Silent Overflow** - Incorrect calculations (Issue #9)

**MEDIUM PRIORITY:**
3. **Comment Parsing Issues** - Code readability (Issue #12)
4. **Colon Statement Parser** - Syntax flexibility (Issue #13)

**LOW PRIORITY:**
5. **Local Variable Limits** - Has workaround (Issue #11)
6. **Underscore Identifiers** - Cosmetic/design choice (Issue #17)