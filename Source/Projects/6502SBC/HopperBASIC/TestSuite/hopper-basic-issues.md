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

### 2. Single-line IF/THEN/ELSE/ENDIF Not Supported
**Symptom:** SYNTAX ERROR on single-line IF statements with ELSE clause  
**Reproduce:**
```basic
IF TRUE THEN PRINT "yes" ELSE PRINT "no" ENDIF
IF x > 5 THEN PRINT "big" ELSE PRINT "small" ENDIF
```
**Error:** `?SYNTAX ERROR`  
**Status:** ACTIVE  
**Impact:** Major limitation - forces multi-line IF statements only
**Note:** Simple IF/THEN/ENDIF (without ELSE) works fine

### 3. Multi-line IF/THEN/ELSE/ENDIF Also Fails
**Symptom:** SYNTAX ERROR even on properly formatted multi-line IF statements  
**Reproduce:**
```basic
IF level > 2 THEN
    PRINT "big"
ELSE
    PRINT "small"
ENDIF
```
**Error:** `?SYNTAX ERROR` on ELSE line  
**Status:** ACTIVE  
**Impact:** IF/ELSE constructs completely unusable

---

## BUGS
*Wrong behavior but recoverable*

### 4. STEP 0 Infinite Loop
**Symptom:** No error checking for STEP 0  
**Reproduce:**
```basic
FOR i = 1 TO 5 STEP 0
    PRINT i    ! Infinite loop
NEXT i
```
**Status:** ACTIVE  
**Note:** Classic BASIC compatibility issue - error vs. infinite loop

### 5. Expression Evaluation in Multi-statement Context
**Symptom:** Incorrect results when chaining statements with colons in IF blocks  
**Reproduce:**
```basic
count = 0
IF TRUE THEN count = count + 1: PRINT "First"; count = count + 1: PRINT "Second" ENDIF
! Expected: count=2, "FirstSecond"
! Actual: count=1, "FirstFALSESecond"
```
**Status:** ACTIVE  
**Note:** Expression evaluation appears corrupted in complex statement chains

### 6. VARS Command Comment Parsing
**Symptom:** SYNTAX ERROR when VARS followed by comment  
**Reproduce:**
```basic
VARS ! comment
```
**Error:** `?SYNTAX ERROR (0xE0FB)`  
**Status:** ACTIVE

---

## ANNOYANCES
*Minor issues, cosmetic, or nice-to-haves*

### 7. CHAR Ordered Comparison Error Message
**Symptom:** STRING comparison gives INVALID OPERATOR instead of TYPE MISMATCH  
**Reproduce:**
```basic
CHAR c = 'A'
STRING s = "A"
PRINT c >= s
```
**Error:** `?INVALID OPERATOR` (should be `?TYPE MISMATCH` for consistency)  
**Status:** ACTIVE  
**Note:** Acceptable behavior since STRING doesn't support ordering operators

---

## FIXED
*Already resolved*

### ~~VAR Type Re-initialization~~ ✅
**Was:** Hang when initialized VAR changed type to STRING then RUN  
**Fixed:** Now properly resets to declared type/value on RUN

### ~~STRING Local Variables~~ ✅
**Was:** SYNTAX ERROR for STRING declarations in functions  
**Fixed:** Tokenizer issue with >256 byte token streams

### ~~DEL Reserved Word~~ ✅
**Was:** Variable name 'del' conflicts with DEL command  
**Note:** Working as designed - DEL reserved for future file operations

---

## WORKING CORRECTLY
*Features that tested successfully*

### ✅ Simple IF/THEN/ENDIF (No ELSE)
**Working:** Basic conditional execution without ELSE clause
```basic
IF condition THEN statements ENDIF
```

### ✅ Complex Expression Conditions
**Working:** Arithmetic, logical, comparison expressions as conditions
```basic
IF a + b = 7 THEN PRINT "works" ENDIF
IF a < b AND b < c THEN PRINT "chain" ENDIF
```

### ✅ All Data Types as Conditions
**Working:** BIT, VAR, comparison results, string equality, char ordering
```basic
IF flag THEN ... ENDIF              ! BIT
IF s = "HELLO" THEN ... ENDIF       ! STRING equality
IF c1 < c2 THEN ... ENDIF           ! CHAR ordering
```

### ✅ Type Safety in Conditions
**Working:** Proper TYPE MISMATCH errors for invalid comparisons
```basic
IF string_var = int_var THEN ... ENDIF  ! Correctly errors
```

---

*Last Updated: IF statement testing reveals major conditional logic limitations*  
*Version: Hopper BASIC v2.0*  
*Critical Issue: IF/ELSE constructs completely non-functional*