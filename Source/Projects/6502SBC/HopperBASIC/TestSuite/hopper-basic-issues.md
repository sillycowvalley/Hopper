# Hopper BASIC Open Issues

## SHOWSTOPPERS
*Crashes, hangs, or data corruption that prevent basic use*

### 1. STEP 0 Infinite Loop
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

### 2. Local Variable Limit in FOR Loop Bodies
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

### 3. Comment Parsing Issues
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

### 4. Colon Statements in Multiline IF Blocks
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

### 5. Underscore Identifiers Not Supported
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
1. **STEP 0 Infinite Loop** - Data corruption risk (Issue #1)

**MEDIUM PRIORITY:**
2. **Comment Parsing Issues** - Code readability (Issue #3)
3. **Colon Statement Parser** - Syntax flexibility (Issue #4)

**LOW PRIORITY:**
4. **Local Variable Limits** - Has workaround (Issue #2)
5. **Underscore Identifiers** - Cosmetic/design choice (Issue #5)