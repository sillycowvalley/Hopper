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

### 3. Variable Declaration Comment Parsing
**Symptom:** SYNTAX ERROR when variable declaration followed by comment  
**Reproduce:**
```basic
INT outer ! comment
```
**Error:** `?SYNTAX ERROR (0xC77A)`  
**Status:** ACTIVE

### 4. VARS Command Comment Parsing
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

### 5. CHAR Ordered Comparison Error Message
**Symptom:** STRING comparison gives INVALID OPERATOR instead of TYPE MISMATCH  
**Reproduce:**
```basic
CHAR c = 'A'
STRING s = "A"
PRINT c >= s
```
**Error:** `?INVALID OPERATOR` (should be `?TYPE MISMATCH` for consistency)  
**Status:** ACTIVE

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

*Last Updated: Testing session with FOR/NEXT loops*  
*Version: Hopper BASIC v2.0*  
*Critical Bug: VAR initialized variables crash on type change to STRING*