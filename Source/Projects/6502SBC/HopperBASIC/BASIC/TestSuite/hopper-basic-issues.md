# Hopper BASIC Open Issues

## BUGS
*Wrong behavior but recoverable*

### 1. Local Variable Limit in FOR Loop Bodies
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
**Workaround:** Declare all needed variables at function start

### 2. Colon Statements in Multiline IF Blocks
**Symptom:** Parser fails when colon-separated statements used within multiline IF/ELSE blocks  
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

### 3. Underscore Identifiers Not Supported
**Symptom:** Variable names with underscores rejected  
**Reproduce:**
```basic
INT global_counter = 0
```
**Error:** `?SYNTAX ERROR`  
**Status:** WORKING AS DESIGNED