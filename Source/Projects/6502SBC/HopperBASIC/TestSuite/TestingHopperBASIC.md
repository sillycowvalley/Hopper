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

### 2. Test Suite Architecture

**Recommended Structure**:
```basic
! Test Suite Title
FUNC TestFeatureA()
    ! Focused test of specific feature
    ! Keep under 400 characters
ENDFUNC

FUNC TestFeatureB()
    ! Another focused test
ENDFUNC

BEGIN
    PRINT "=== Test Suite Name ==="
    TestFeatureA()
    TestFeatureB()
    PRINT "=== Complete ==="
END
```

### 3. Output Validation

**Expected Output Format**:
- Clean output without debug traces
- Clear indication of test vs expected values
- Example: `flag2=TRUE (expect TRUE)`

**Debug Output Detection**:
During initial testing, unexpected debug output was discovered:
```
NEXT:BIT:0000 TOP:BIT:0001
```
This indicated incomplete removal of debug code in the PRINT statement handler when processing semicolon-separated expressions. Such output should be considered a test failure.

### 4. Testing Scopes

**Global Scope Testing** (REPL level):
- Variables declared outside functions/BEGIN-END
- Direct execution at READY prompt
- Persistent across function calls
- Example: `> BIT GLOBAL = TRUE`

**Local Scope Testing** (within functions):
- Variables declared inside FUNC/ENDFUNC or BEGIN/END
- Scope limited to function
- Stack frame managed with BP-relative addressing
- **Local variables shadow globals** - same name hides global in local scope

**Scope Shadowing Test**:
```
> BIT B = FALSE      (global)
> BEGIN
*   BIT B = TRUE     (local shadows global)
*   PRINT B          (prints local: TRUE)
* END
> RUN
TRUE
> PRINT B            (prints global: FALSE)
```

**Test Coverage Areas for Each Type**:
1. Global declaration and initialization
2. Local declaration and initialization  
3. Assignment operations
4. Type-specific operations
5. CONST declarations (immutability testing)
6. VAR declarations (runtime typing)
7. Type checking and error cases
8. Control flow usage
9. Scope shadowing behavior
10. Edge cases and boundaries

### 5. Type Safety Validation

**Type Checking Tests**:
Each type must reject incompatible assignments and operations.

**Expected Error Formats**:
- Type mismatch: `?TYPE MISMATCH (0x9F36)`
- Const assignment: `?ILLEGAL ASSIGNMENT (0x9BAF)`

**CONST Immutability Testing**:
- Verify CONST declarations create immutable values
- Test assignment attempts to CONST variables
- Confirm error on modification attempt
- Verify CONST value remains unchanged
- Test in both global and local scopes

**VAR Runtime Typing**:
- VAR variables are duck-typed (runtime type resolution)
- VARS command shows current type: `VAR(BIT)`, `VAR(INT)`, etc.
- Test type changes through assignment
- Verify operations use current runtime type
- Test type inference from initial value

**Test Categories**:
1. **Invalid Assignments**: Wrong type literals to typed variables
2. **Const Modification**: Attempts to change CONST values  
3. **Invalid Operations**: Operations between incompatible types
4. **Invalid Conversions**: Implicit conversions that shouldn't work
5. **Function Parameters**: Type mismatches in function calls
6. **VAR Type Evolution**: Runtime type changes for VAR variables

**Testing Protocol**:
- Verify error message appears with correct code
- Confirm variable state after failed operations
- Test both global and local scope type checking
- Verify CONST immutability at all scopes
- Track VAR type changes through VARS command

### 6. Diagnostic Tools for Testing

**VARS Command**:
- Lists all global variables and constants
- Shows current values
- Displays in declaration order
- **CONST variables** shown as: `CONST BIT CBIT = TRUE`
- **VAR variables** show runtime type: `VAR(BIT) VBIT = FALSE`
- Use to verify global state and type evolution

**FUNCS Command**:
- Lists all defined functions
- Shows function signatures
- Includes BEGIN/END as main program
- Use to verify function loading

**LIST Command**:
- Complete program listing
- Shows all declarations and definitions
- Preserves original structure
- Use for full program verification

**MEM Command**:
- Shows available memory
- Use before/after tests (with the help of `NEW`) to check for leaks
- Monitor memory consumption

**Testing Workflow**:
1. Use VARS to check initial state
2. Execute test operations
3. Use VARS to verify changes and types
4. Use FUNCS to confirm function definitions
5. Use MEM to check memory health
6. Test scope shadowing with local/global comparison

### 7. Testing Methodology

**Comprehensive Testing Approach**:

1. **Global Scope First**:
   - Test at REPL prompt
   - Verify with VARS command
   - Test persistence across operations

2. **Type Safety Validation**:
   - Test invalid assignments
   - Verify error messages and codes
   - Confirm no state corruption

3. **Local Scope Testing**:
   - Create test functions
   - Verify scope isolation
   - Test parameter passing

4. **Integration Testing**:
   - Mix global and local variables
   - Test in expressions and control flow
   - Verify type interactions

**Output Verification**:
- Always include expected values in output
- Use consistent format: `actual (expect expected)`
- Test both success and failure cases
- Verify error messages match expected format

**State Verification**:
- Use VARS to check global state
- Use FUNCS to verify function definitions
- Use MEM to monitor memory usage
- Use LIST for full program verification

### 6. Known Issues and Workarounds

**Issue #1**: Debug output in PRINT statements
- **Symptom**: Stack/type debug messages appear with semicolon-separated PRINT
- **Status**: Fixed (as of last test run)
- **Workaround if needed**: Use multiple PRINT statements instead of semicolons

### 7. Test Execution Protocol

1. Enter each test function individually
2. Verify OK response after ENDFUNC
3. Execute tests one at a time initially
4. Check for unexpected output or error messages
5. Document any deviations from expected behavior

### 8. Future Testing Priorities

**Critical Test Areas**:
1. **Type System Completeness**: All types with both global and local scope
2. **Type Safety**: Comprehensive error checking and type mismatch detection  
3. **Memory Management**: No leaks, proper cleanup, stack frame handling
4. **Function System**: Recursion, parameter passing, return values
5. **Control Flow**: All structures with type-appropriate conditions
6. **Array Support**: When implemented, full indexing and bounds checking

**Regression Testing**:
- Maintain test suite for each completed feature
- Run after any interpreter changes
- Document any new error codes discovered
- Track performance metrics (MEM usage)

### 9. Test Suite Character Budget

**Typical character counts**:
- Simple declaration test: ~250 chars
- Operation test with multiple cases: ~300 chars
- Complex expression test: ~350 chars
- Control flow test: ~280 chars
- Main BEGIN/END orchestrator: ~400 chars max

**Budget Planning**:
- Reserve 100 chars for function wrapper
- Allocate 300-350 chars for test logic
- Keep messages concise but clear

### 10. Quality Indicators

**Good Test Function**:
- Single responsibility
- Clear expected vs actual output
- Under 400 characters
- No complex nested structures
- Descriptive but concise naming

**Test Suite Success Criteria**:
- All functions load without error
- No unexpected debug output
- All actual values match expected
- Clean execution without crashes
- Proper memory cleanup (check with MEM command)

---

## Testing Checklist Template

For each data type (BIT, INT, WORD, BYTE, CHAR, STRING):

### Global Scope Tests
- [ ] Declaration without initialization
- [ ] Declaration with initialization  
- [ ] Assignment after declaration
- [ ] CONST declaration and immutability
- [ ] Type mismatch errors
- [ ] Verify with VARS command

### CONST Testing
- [ ] CONST declaration with value
- [ ] Attempt assignment (expect ILLEGAL ASSIGNMENT)
- [ ] Verify value unchanged after error
- [ ] Use CONST in expressions
- [ ] CONST in local scope
- [ ] CONST as function parameter

### VAR Type Testing (duck typing)
- [ ] VAR declaration with initial type
- [ ] Type shown correctly in VARS listing
- [ ] Assignment changes runtime type
- [ ] Operations use current type
- [ ] Type errors based on current type
- [ ] VAR as function parameter

### Local Scope Tests  
- [ ] Declaration in function
- [ ] Declaration in BEGIN/END
- [ ] Local shadows global (same name)
- [ ] Verify shadowing with before/after values
- [ ] Parameter passing
- [ ] Return values
- [ ] Verify scope isolation

### Type-Specific Operations
- [ ] Valid operations for type
- [ ] Invalid operation errors
- [ ] Type promotion behavior
- [ ] Boundary conditions
- [ ] Edge cases

### Integration Tests
- [ ] Use in expressions
- [ ] Use in control flow
- [ ] Use in loops
- [ ] Mixed type operations
- [ ] Function arguments
- [ ] Scope interaction (global/local)

---

*Document Version: 1.2*  
*Last Updated: Added CONST immutability, VAR duck typing, and scope shadowing*  
*Platform: 6502 Cycle-Accurate Emulator*  
*Hopper BASIC Version: 2.0*