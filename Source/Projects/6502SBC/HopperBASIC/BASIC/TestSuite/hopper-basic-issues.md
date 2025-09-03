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
**Impact:** Variables must be declared before FOR loops, limiting dynamic local declarations
**Workaround:** Declare all needed variables at function start

### 2. Comment Parsing Issues (PARTIALLY RESOLVED)
**Symptom:** SYNTAX ERROR when comments follow certain system commands  
**Reproduce:**
```basic
PRINT ! Empty line           ! Still causes SYNTAX ERROR
VARS ! comment              ! Still causes SYNTAX ERROR  
MEM ! comment               ! Still causes SYNTAX ERROR
```
**Error:** `?SYNTAX ERROR`  
**Status:** PARTIALLY RESOLVED  
**Fixed:** PRINT with expressions, VAR declarations now support trailing comments
**Still Broken:** System commands (VARS, MEM), PRINT without arguments
**Impact:** REDUCED - Main use cases now work, only edge cases remain
**Note:** Most common trailing comment scenarios now functional

### 3. Colon Statements in Multiline IF Blocks
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

### 4. Underscore Identifiers Not Supported
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

**MEDIUM PRIORITY:**
1. **Colon Statement Parser** - Syntax flexibility (Issue #3)

**LOW PRIORITY:**
2. **Comment Parsing Remaining Cases** - Minor edge cases (Issue #2)
3. **Local Variable Limits** - Has workaround (Issue #1)
4. **Underscore Identifiers** - Design choice (Issue #4)

---

**Current Status:** Hopper BASIC is in excellent condition with all core functionality working correctly. Recent fixes to comment parsing have resolved the main interactive development workflow issues. Only 3 minor parser edge cases remain, none affecting basic programming capabilities.