# Hopper BASIC Testing Documentation

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
! BAD: Testing everything at once
BEGIN
    ! Tests all types - can't identify failure
    PRINT int_test; word_test; bit_test; char_test
END

! GOOD: Progressive isolation
! Step 1: Test each type separately
NEW
INT i = 5
BEGIN
    PRINT "INT: "; i; " ! expect 5"
END
RUN

! Step 2: If that works, add next type
NEW
INT i = 5
WORD w = 100
BEGIN
    PRINT "INT: "; i; " ! expect 5"
    PRINT "WORD: "; w; " ! expect 100"
END
RUN

! Step 3: On failure, isolate to minimum
NEW
VAR v = 1    ! Minimal failing case found
v = "X"
BEGIN
END
RUN
```

### 3. Memory and State Verification

**Essential Diagnostic Pattern**:
```basic
MEM                  ! Check memory before
! ... test code ...
NEW                  ! Clear everything
MEM                  ! Verify no memory leak
```

**State Verification Tools**:
- **MEM**: Check for memory leaks between tests
- **VARS**: Verify global state and type evolution
- **FUNCS**: Confirm function definitions loaded correctly
- **LIST**: Full program verification

**Post-Execution Verification**:
Always check state AFTER program execution, not just during:
```basic
RUN
! Program completes
VARS    ! Check final state of all globals
```

### 4. Output Validation

**Expected Output Format**:
- Always include expected values directly in output
- Format: `value ! expect expected`
- Makes test self-documenting and failures obvious

**Debug Output Detection**:
Watch for unexpected debug traces like:
```
NEXT:BIT:0000 TOP:BIT:0001
D5
[00][00][00]
```
Any such output indicates incomplete debug code removal and should be treated as a test failure.

### 5. Crash Type Classification

**Document the TYPE of failure, not just that it failed**:

1. **Clean Error**: Program stops with error message and code
   - Example: `?TYPE MISMATCH (0x9F36)`
   - System remains stable

2. **Clean Crash**: Program stops with crash dump
   - Example: `== CRASH == SP:CF`
   - May need NEW to recover

3. **Hang**: Program enters infinite loop
   - Requires ^C interrupts to break
   - Example: VAR re-initialization bug
   - Often corrupts interpreter state

4. **Silent Corruption**: No visible error but subsequent commands fail
   - Most dangerous type
   - Example: SYNTAX ERROR on valid commands after crash

### 6. Global Variable Re-initialization Behavior

**CORRECTED BEHAVIOR - ALL globals re-initialize on RUN**:

1. **Initialized Globals**: `BIT B = TRUE`
   - Reset to declared value on each `RUN`
   - Example: `BIT B = TRUE` always resets to TRUE

2. **Uninitialized Globals**: `BIT B`
   - Reset to type-appropriate default on each `RUN`
   - Defaults: INT/WORD/BYTE→0, BIT→FALSE, CHAR→NUL, STRING→empty, VAR→FALSE

**Test Example**:
```basic
INT i = 100      ! Will reset to 100 on RUN
INT i2           ! Will reset to 0 on RUN
i = 999: i2 = -5555
BEGIN
    PRINT i; " "; i2; " ! expect 100 0"
END
RUN              ! Both reset
```

### 7. Test Variant Management

**Use Comments for Multiple Test Scenarios**:
Instead of writing separate test files, use comments to create variants:
```basic
! Test variant selection via comments
VAR v = 42       ! Test initialized VAR
!VAR v           ! Alternative: test uninitialized

v = "STRING"     ! This line causes crash with initialized VAR
!v = 100         ! Alternative: stays same type (safe)

BEGIN
    PRINT v
END
RUN
```

This technique allows rapid switching between test cases by commenting/uncommenting lines.

### 8. Known Issues and Workarounds

**Issue #1**: Debug output in PRINT statements
- **Status**: Fixed
- **Workaround if needed**: Use multiple PRINT statements instead of semicolons

**Issue #2**: VAR re-initialization crash
- **Symptom**: Hang/crash when initialized VAR changes from INT/WORD to STRING then RUN
- **Status**: ACTIVE BUG
- **Minimal reproduction**:
  ```basic
  VAR v = 1
  v = "X"
  BEGIN
  END
  RUN    ! HANGS
  ```
- **Workaround**: Don't use initialized VAR variables that change to STRING type

### 9. Testing Methodology

**Test Development Workflow**:

1. **Hypothesis**: Form a theory about behavior
2. **Minimal Test**: Create smallest possible test
3. **Observe**: Run and document actual behavior
4. **Isolate**: If failure, progressively isolate
5. **Document**: Record both expected and actual behavior
6. **Verify State**: Check MEM, VARS after completion

**Golden Rules**:
- One test, one purpose
- Start isolated, combine only after individual success
- Always include expected values in output
- Check state both during AND after execution
- Document crash TYPE, not just occurrence
- Use diagnostic commands liberally (MEM, VARS, FUNCS)

### 10. Test Suite Architecture

**Recommended Structure for Complex Testing**:
```basic
! Phase 1: Individual type tests with NEW between each
NEW
! Test Type A
RUN
VARS
MEM

NEW  
! Test Type B
RUN
VARS
MEM

! Phase 2: Combined test only after individuals pass
NEW
! Combined test
RUN
```

### 11. Quality Indicators

**Signs of a Good Test**:
- Can identify failure point immediately
- Under 400 characters per function
- Self-documenting with embedded expectations
- No memory leaks (MEM before = MEM after)
- Tests one specific behavior
- Provides minimal reproduction case on failure

**Signs Testing is Needed**:
- Any crash without clear error message
- Unexpected debug output
- Memory changes between runs
- Type system edge cases
- Scope interaction complexity

### 12. Testing Checklist Template

For each data type (BIT, INT, WORD, BYTE, CHAR, STRING, VAR):

#### Progressive Isolation Tests
- [ ] Test type in complete isolation first
- [ ] Verify with diagnostic commands (VARS, MEM)
- [ ] Only combine with other types after individual success

#### Global Scope Tests
- [ ] Declaration without initialization (resets to default on RUN)
- [ ] Declaration with initialization (resets to declared value on RUN)
- [ ] Verify re-initialization with before/after RUN comparison
- [ ] Assignment after declaration
- [ ] CONST declaration and immutability
- [ ] Type mismatch errors with error code verification

#### Memory Leak Tests
- [ ] MEM before operations
- [ ] Execute test operations
- [ ] NEW to clear
- [ ] MEM after to verify no leak

#### VAR Type Special Cases
- [ ] VAR with initial value that doesn't change type (safe)
- [ ] VAR without initial value (safe)
- [ ] ⚠️ VAR with initial value that changes to STRING (KNOWN CRASH)

#### Post-Execution Verification
- [ ] State persists correctly after function return
- [ ] VARS shows expected values after RUN
- [ ] No corruption of other variables

---

## Key Lessons Learned

1. **Progressive Isolation is Essential**: Never test multiple features together until each works alone
2. **Memory State Verification**: Always check MEM before/after with NEW
3. **Post-Execution State Matters**: Use VARS after RUN, not just during
4. **Crash Types Matter**: Distinguish between clean errors, crashes, hangs, and corruption
5. **Test Writing IS Debugging**: Tests aren't just validation, they're active bug hunting tools
6. **Comment-Based Variants**: Use comments to create multiple test scenarios in one file
7. **Expected Values in Output**: Self-documenting tests with `! expect X` patterns

---

*Document Version: 2.0*  
*Last Updated: Major revision based on VAR crash discovery and testing methodology refinement*  
*Platform: 6502 Cycle-Accurate Emulator*  
*Hopper BASIC Version: 2.0*  
*Critical Bug: VAR initialized variables crash on type change to STRING*