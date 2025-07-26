# HopperBASIC Refactoring Best Practices and Rules (Updated)

## Core Principles

### 1. Single Exit Pattern
- Use `loop/break` blocks to ensure single exit point for complex functions
- All register/variable preservation happens at entry, restoration at exit
- Consistent flag setting (C for success, NC for failure) happens before the break
- Exception: Don't use single exit blocks when there's already only one exit point

### 2. API Documentation Format
```asm
// Brief description of function purpose
// Input: Parameter descriptions with register/variable locations
// Output: Result descriptions with register/variable locations and flag states
// Munts: List of ZP variables modified (excluding outputs and ZP.Mx slots)
```

### 3. Preservation Rules
- **Always preserve**: Registers (A, X, Y) unless documented otherwise
- **Key variables**: Preserve ZP.IDX and ZP.ACC by default unless they are outputs
- **Don't list outputs** in the "Munts" section - it's redundant
- **Don't care about**: ZP.M0-M15 slots (used by Memory management)
- **Internal workspace**: Don't list internal ZP slots (like ZP.Symbol*) in public API munts lists

### 4. Optimization Rules
- Only skip preservation if:
  1. A called function already preserves it, AND
  2. You don't modify it yourself between/after the calls
- Remove redundant push/pop only when you can prove the value isn't changed
- Be careful about preservation in iterative functions that modify state

## Additional Rules from Project Guidelines

### Silent Failure Prevention (RULE #1)
- **NEVER allow silent failures** - always signal errors clearly
- BRK is acceptable for unimplemented features (emulator throws exception)
- Good pattern for TODO:
```asm
// TODO: Show variables
LDA #(Messages.NotImplemented % 256)
STA ZP.LastErrorL
LDA #(Messages.NotImplemented / 256)
STA ZP.LastErrorH
BRK
```

### Zero Page Usage (RULE #2)
- **Don't use undocumented ZP slots** without asking first
- Always check ZeroPage.asm before using any ZP location
- For temporary values over short sections, prefer stack operations (PHX, PHY, PHA) over zero page variables
- This preserves precious ZP space and avoids conflicts

### Flag Comments (RULE #3)
- **Don't use '0' and '1'** in flag comments
- Use clear comments like:
  - `// Set Z` or `// Set NZ`
  - `// Set C` or `// Set NC`
- Much easier to understand, especially for dyslexic developers

### Complete Code Generation (RULE #4)
- **NEVER use partial code placeholders** like:
```asm
// Rest of the function remains the same...
```
- Always generate complete methods to avoid cut-and-paste errors
- Prevents loss of good working code

### Debug-First Approach (RULE #5)
- When debugging, **don't start by generating tons of code**
- First suggest where to insert debug output (DumpXXX methods)
- Analyze the problem before jumping to solutions
- "Thoughts?" is shorthand for "analyze, don't generate code yet"

### Identifier Naming (RULE #8)
- Avoid ALL_CAPS_WITH_UNDERSCORES style
- Prefer CamelCase or camelCase for readability
- More consistent with modern coding standards

### Enum Usage (RULE #9)
- Use direct enum syntax: `LDA #SymbolType.VARIABLE`
- Don't qualify with namespace: ~~`LDA #Objects.SymbolType.VARIABLE`~~

## Code Patterns

### Standard Function Template
```asm
FunctionName()
{
    PHA
    PHX
    PHY
    
    LDA ZP.IDXL
    PHA
    LDA ZP.IDXH
    PHA
    
    LDA ZP.ACCL
    PHA
    LDA ZP.ACCH
    PHA
    
    loop // start of single exit block
    {
        // Function logic here
        
        if (error_condition)
        {
            CLC  // Error
            break;
        }
        
        // Success path
        SEC  // Success
        break;
    } // end of single exit block
    
    PLA
    STA ZP.ACCH
    PLA
    STA ZP.ACCL
    
    PLA
    STA ZP.IDXH
    PLA
    STA ZP.IDXL
    
    PLY
    PLX
    PLA
}
```

### Simple Function (No Complex Logic)
```asm
SimpleFunctionName()
{
    // Direct implementation without loop/break
    LDA some_value
    STA some_location
    
    if (Z)
    {
        CLC  // Failure
    }
    else
    {
        SEC  // Success
    }
}
```

## Flag Usage
- **C | NC**: Preferred for success/failure status
  - C (Carry Set) = Success
  - NC (Carry Clear) = Failure
- **Z | NZ**: Avoid for return values - too easy to accidentally modify
  - Reserve for comparisons only
  - Comment as "Set Z" or "Set NZ" (not "0" or "1")

## Error Handling
- Set error messages in ZP.LastErrorL/H before returning NC
- Use meaningful error messages from Messages unit
- Never fail silently - always set an error or use BRK with TODO pattern
- When a caller checks a flag incorrectly (Z instead of C), bugs are hard to find

## Memory Management Integration
- Memory.Allocate() munts: ZP.IDY, ZP.TOP, ZP.NEXT (preserves ZP.ACC)
- Memory.Free() munts: ZP.IDY, ZP.TOP, ZP.NEXT (preserves ZP.IDX, ZP.ACC)
- Both preserve all registers (A, X, Y)
- Watch for memory leaks in iterative operations

## Helper Functions
- Create helper functions for common operations (StringLength, StringCompare, CopyBytes)
- Document helper function APIs clearly
- Use consistent parameter passing (X,Y for addresses, ZP locations for complex params)
- Helpers improve code clarity and provide optimization opportunities

## Debug Output Best Practices
- Use Tools debug methods that actually exist (check Tools.asm first!)
- Tools.DumpHeap() is particularly useful for memory leak detection
- Debug methods should preserve all registers and state
- Don't invent methods that don't exist

## Common Pitfalls
1. **Using Z flag for return values** - Too fragile, use C instead
2. **Manufacturing new ZP slots** - Always check ZeroPage.asm first
3. **Forgetting to preserve iteration state** - IterateNext needs consistent state from IterateStart
4. **Not checking actual tool parameters** - Tools.CopyBytes might use different params than expected
5. **Missing preservation around complex operations** - Nested calls can munt unexpected variables
6. **Assuming debug methods exist** - Always verify in Tools.asm

## Testing and Debugging
- Always test memory leak detection with complex operations
- Add debug output when tracking down issues
- Heap dumps are invaluable for understanding memory state
- Check both success and failure paths for proper cleanup

## Client Code Benefits
- Clean APIs allow removal of redundant preservation in client code
- Example: TestTable became much simpler after Table APIs were refactored
- Focus on test/business logic rather than defensive preservation
- Trust the API contracts

## Summary
The goal is to create consistent, well-documented APIs that:
1. Clearly state their contracts (inputs/outputs/side effects)
2. Preserve key working variables unless they're outputs
3. Use single exit points for complex logic
4. Return consistent success/failure indicators (prefer C/NC)
5. Can be optimized by removing only truly redundant operations
6. Avoid fragile flag usage (Z) for return values
7. Provide clear helper functions for common operations
8. Follow all project-specific rules (RULE #1 through #9)

---

## Test System Call Tree

```
Test.Hopper()
├── SEI
├── Test.InitializeTest()
│   ├── Serial.Initialize()
│   ├── Memory.InitializeHeapSize()
│   ├── Stacks.Initialize()
│   ├── Objects.Initialize()
│   │   └── STZ (ZP.VariablesListL/H, ZP.FunctionsListL/H)
│   ├── STZ (TableHeadLocationL/H)
│   ├── STZ ZP.FLAGS / SMB0 ZP.FLAGS
│   └── Test.PrintString() [testHeader]
│       └── Serial.WriteChar() [loop]
├── CLI
├── Test.RunAllTests()
│   ├── Test.PrintSectionHeader() [tableSection]
│   │   └── Test.PrintString()
│   ├── TestTable.RunTableTests()
│   │   ├── testEmptyList()
│   │   ├── testAddSingle()
│   │   ├── testAddMultiple()
│   │   ├── testTraverse()
│   │   ├── testDeleteFirst()
│   │   ├── testClearList()
│   │   ├── testDeleteMiddleNode()
│   │   ├── testDeleteLastNode()
│   │   ├── testDeleteFromSingleNode()
│   │   ├── testDeleteNonExistentNode()
│   │   ├── testComplexAddDeleteSequence()
│   │   └── testDeleteAllNodesIndividually()
│   │       └── [Each test follows similar pattern]:
│   │           ├── Test.PrintTestHeader()
│   │           │   ├── Test.PrintString()
│   │           │   └── Test.StartMemoryTest()
│   │           │       ├── Memory.Available()
│   │           │       └── Stacks.PopTop()
│   │           ├── [Test-specific Table operations]
│   │           │   ├── Table.GetFirst() ✓
│   │           │   ├── Table.GetNext() ✓
│   │           │   ├── Table.Add() ✓
│   │           │   │   └── Memory.Allocate() ✓
│   │           │   ├── Table.Delete() ✓
│   │           │   │   └── Memory.Free() ✓
│   │           │   └── Table.Clear() ✓
│   │           │       └── Table.Delete() [loop] ✓
│   │           └── Test.PrintResult()
│   │               ├── Test.EndMemoryTest()
│   │               │   ├── Memory.Available()
│   │               │   └── Stacks.PopTop()
│   │               ├── Test.PrintString() [testPassed/testFailed]
│   │               ├── Serial.WriteChar()
│   │               ├── Serial.HexOut() [if fail]
│   │               └── Tools.DumpHeap() [if DEBUG & fail]
│   │
│   ├── Test.PrintSectionHeader() [objectsSection]
│   ├── TestObjects.RunObjectsTests()
│   │   ├── testAddSymbol()
│   │   ├── testFindSymbol()
│   │   ├── testGetSymbolData()
│   │   ├── testSetSymbolValue()
│   │   ├── testSymbolFiltering()
│   │   ├── testRemoveSymbol()
│   │   ├── testGetSetTokens()
│   │   ├── testSymbolNotFound()
│   │   ├── testMixedSymbolIteration()
│   │   ├── testSimilarNameComparison()
│   │   └── testDestroy()
│   │       └── [Each test uses]:
│   │           ├── Objects.Add() ✓
│   │           │   ├── Objects.Find() ✓
│   │           │   │   ├── Table.GetFirst() ✓
│   │           │   │   ├── Objects.compareNames() ✓
│   │           │   │   │   └── Tools.StringCompare()
│   │           │   │   └── Table.GetNext() ✓
│   │           │   ├── Objects.calculateNodeSize() ✓
│   │           │   ├── Table.Add() ✓
│   │           │   └── Objects.initializeNode() ✓
│   │           │       ├── Objects.copyNameToNode() ✓
│   │           │       └── Tools.CopyBytes()
│   │           ├── Objects.GetData() ✓
│   │           ├── Objects.SetValue() ✓
│   │           ├── Objects.GetTokens() ✓
│   │           ├── Objects.SetTokens() ✓
│   │           ├── Objects.Remove() ✓
│   │           │   └── Table.Delete() ✓
│   │           ├── Objects.IterateStart() ✓
│   │           │   ├── Table.GetFirst() ✓
│   │           │   └── Objects.findNextMatch() ✓
│   │           ├── Objects.IterateNext() ✓
│   │           │   ├── Table.GetNext() ✓
│   │           │   └── Objects.findNextMatch() ✓
│   │           └── Objects.Destroy() ✓
│   │               └── Table.Clear() ✓
│   │
│   ├── Test.PrintSectionHeader() [variablesSection]
│   ├── TestVariables.RunVariablesTests()
│   │   ├── testDeclareIntVariable()
│   │   ├── testDeclareWordConstant()
│   │   ├── testFindVariableByName()
│   │   ├── testGetVariableValue()
│   │   ├── testSetVariableValue()
│   │   ├── testSetConstantValue()
│   │   ├── testGetVariableTokens()
│   │   ├── testIterateVariablesOnly()
│   │   ├── testDeclareBitVariable()
│   │   ├── testRemoveVariable()
│   │   ├── testGetVariableType()
│   │   ├── testGetVariableName()
│   │   ├── testIterateAllSymbols()
│   │   ├── testIterateConstants()
│   │   └── testDeclareByteVariable()
│   │       └── [Each test uses]:
│   │           ├── TestVariables.allocateTestTokens()
│   │           │   └── Memory.Allocate() ✓
│   │           ├── Variables.Declare() ✓
│   │           │   ├── Objects.Find() ✓
│   │           │   └── Objects.Add() ✓
│   │           ├── Variables.Find() ✓
│   │           │   ├── Objects.Find() ✓
│   │           │   └── Objects.GetData() ✓
│   │           ├── Variables.GetValue() ✓
│   │           │   └── Objects.GetData() ✓
│   │           ├── Variables.SetValue() ✓
│   │           │   ├── Objects.GetData() ✓
│   │           │   └── Objects.SetValue() ✓
│   │           ├── Variables.GetType() ✓
│   │           │   └── Objects.GetData() ✓
│   │           ├── Variables.GetName() ✓
│   │           ├── Variables.GetTokens() ✓
│   │           │   └── Objects.GetTokens() ✓
│   │           ├── Variables.Remove() ✓
│   │           │   ├── Variables.Find() ✓
│   │           │   ├── Objects.GetTokens() ✓
│   │           │   ├── Objects.Remove() ✓
│   │           │   └── Memory.Free() ✓
│   │           ├── Variables.IterateVariables() ✓
│   │           │   └── Objects.IterateStart() ✓
│   │           ├── Variables.IterateConstants() ✓
│   │           │   └── Objects.IterateStart() ✓
│   │           ├── Variables.IterateAll() ✓
│   │           │   └── Objects.IterateStart() ✓
│   │           ├── Variables.IterateNext() ✓
│   │           │   └── Objects.IterateNext() ✓
│   │           └── Variables.Clear() ✓
│   │               ├── Table.GetFirst() ✓
│   │               ├── Objects.GetTokens() ✓
│   │               ├── Table.Delete() ✓
│   │               └── Memory.Free() ✓
│   │
│   ├── Test.PrintSectionHeader() [constantsSection]
│   ├── TestConstants.RunConstantsTests()
│   │   └── [Similar pattern to Variables tests]
│   │
│   ├── Test.PrintSectionHeader() [functionsSection]
│   ├── TestFunctions.RunFunctionsTests()
│   │   ├── testDeclareFunctionNoArgs()
│   │   ├── testDeclareFunctionWithArgs()
│   │   ├── testFindFunctionByName()
│   │   ├── testGetFunctionSignature()
│   │   ├── testGetFunctionBody()
│   │   ├── testGetFunctionArguments()
│   │   ├── testIterateFunctionsOnly()
│   │   ├── testDuplicateFunction()
│   │   ├── testRemoveFunction()
│   │   ├── testGetFunctionName()
│   │   ├── testSetFunctionArguments()
│   │   └── testMultipleFunctionIteration()
│   │       └── [Each test uses]:
│   │           ├── TestFunctions.allocateTestTokens()
│   │           ├── Functions.Declare()
│   │           │   ├── Objects.Find() ✓
│   │           │   └── Objects.Add() ✓
│   │           ├── Functions.Find()
│   │           │   ├── Objects.Find() ✓
│   │           │   └── Objects.GetData() ✓
│   │           ├── Functions.GetSignature()
│   │           │   └── Objects.GetData() ✓
│   │           ├── Functions.GetBody()
│   │           │   └── Objects.GetTokens() ✓
│   │           ├── Functions.GetName()
│   │           ├── Functions.SetArguments()
│   │           │   └── Objects.SetValue() ✓
│   │           ├── Functions.GetArguments()
│   │           │   └── Objects.GetData() ✓
│   │           ├── Functions.Remove()
│   │           │   ├── Functions.Find()
│   │           │   ├── Arguments.Clear() ✓
│   │           │   │   └── Memory.Free() ✓
│   │           │   └── Objects.Remove() ✓
│   │           ├── Functions.IterateFunctions()
│   │           │   └── Objects.IterateStart() ✓
│   │           ├── Functions.IterateNext()
│   │           │   └── Objects.IterateNext() ✓
│   │           └── Functions.Clear()
│   │               ├── Table.GetFirst() ✓
│   │               ├── Arguments.Clear() ✓
│   │               ├── Objects.GetTokens() ✓
│   │               ├── Memory.Free() ✓
│   │               └── Table.Delete() ✓
│   │
│   ├── Test.PrintSectionHeader() [argumentSection]
│   ├── TestArguments.RunArgumentsTests()
│   │   ├── testFunctionArgumentCount()
│   │   ├── testFindArgumentByName()
│   │   ├── testGetArgumentType()
│   │   ├── testGetArgumentName()
│   │   ├── testFindArgumentByIndex()
│   │   └── testIterateFunctionArguments()
│   │       └── [Each test uses]:
│   │           ├── TestArguments.allocateTestTokens()
│   │           ├── Arguments.Add() ✓
│   │           │   ├── Arguments.calculateNodeSize() ✓
│   │           │   ├── Memory.Allocate() ✓
│   │           │   └── Arguments.initializeNode() ✓
│   │           │       ├── Arguments.copyNameToNode() ✓
│   │           │       └── Tools.CopyBytes()
│   │           ├── Arguments.Find() ✓
│   │           │   └── Arguments.compareNames() ✓
│   │           │       └── Tools.StringCompare()
│   │           ├── Arguments.GetType() ✓
│   │           ├── Arguments.GetName() ✓
│   │           ├── Arguments.FindByIndex() ✓
│   │           ├── Arguments.GetCount() ✓
│   │           ├── Arguments.IterateStart() ✓
│   │           └── Arguments.IterateNext() ✓
│   │
│   ├── Test.PrintSectionHeader() [scenarioSection]
│   ├── TestScenarios.RunScenarioTests()
│   │   ├── testVariableReassignmentAfterDeclaration()
│   │   │   └── [Complex scenario using Variables API]
│   │   └── testClearCommandImplementation()
│   │       └── [Complex scenario using Variables iteration]
│   │
│   └── Test.PrintString() [testComplete]
│
└── loop {} [infinite]

Test.IRQ()
└── Serial.ISR()

Test.NMI()
└── [empty]
```

This call tree shows the complete test system structure, with each unique call path traced to its leaf nodes. The test system is organized in layers:

1. **Test Framework** (Test.asm) - Provides test infrastructure
2. **Unit Test Modules** - Test each layer of the symbol table system
3. **Symbol Table Layers** (tested bottom-up):
   - Table (generic linked list)
   - Objects (symbol-specific operations)
   - Variables/Functions (language-specific semantics)
   - Arguments (function parameter management)
4. **Integration Tests** (TestScenarios) - Test real-world usage patterns

---

## Refactored APIs Summary

| Qualified Name | Inputs | Outputs | Munts |
|----------------|--------|---------|-------|
| **Table.GetFirst** ✓ | X = ZP address of list head pointer | ZP.IDX = first node address<br>C = set if found, clear if empty | - |
| **Table.GetNext** ✓ | ZP.IDX = current node | ZP.IDX = next node address<br>C = set if found, clear if end | - |
| **Table.Add** ✓ | X = ZP address of list head pointer<br>ZP.ACC = node size (16-bit) | ZP.IDX = new node address<br>C = set if successful, clear if allocation failed | ZP.IDY, ZP.TOP, ZP.NEXT<br>ZP.LCURRENT, ZP.LHEADX, ZP.LNEXT |
| **Table.Delete** ✓ | X = ZP address of list head pointer<br>ZP.IDX = node to delete | C = set if successful, clear if not found | ZP.IDY, ZP.TOP, ZP.NEXT<br>ZP.LCURRENT, ZP.LPREVIOUS<br>ZP.LNEXT, ZP.LHEADX |
| **Table.Clear** ✓ | X = ZP address of list head pointer | C = set (always succeeds)<br>List head = 0x0000 | ZP.IDY, ZP.TOP, ZP.NEXT<br>ZP.LCURRENT, ZP.LPREVIOUS<br>ZP.LNEXT, ZP.LHEADX |
| **Memory.Allocate** ✓ | ZP.ACC = requested size (16-bit) | ZP.IDX = allocated address<br>(0x0000 if failed) | ZP.IDY, ZP.TOP, ZP.NEXT |
| **Memory.Free** ✓ | ZP.IDX = address to free<br>(must not be 0x0000) | - | ZP.IDY, ZP.TOP, ZP.NEXT |
| **Objects.Initialize** ✓ | - | ZP.VariableListL/H = 0x0000<br>ZP.FunctionsListL/H = 0x0000 | - |
| **Objects.Add** ✓ | X = ZP address of table head<br>ZP.TOP = name pointer<br>ZP.ACC = symbolType\|dataType<br>ZP.IDY = tokens pointer<br>ZP.NEXT = value/args | ZP.IDX = new symbol node<br>C = set if successful | ZP.LCURRENT, ZP.LHEADX, ZP.LNEXT |
| **Objects.Find** ✓ | X = ZP address of table head<br>ZP.TOP = name to search | ZP.IDX = symbol node<br>C = set if found | ZP.LCURRENT, ZP.LNEXT |
| **Objects.Remove** ✓ | X = ZP address of table head<br>ZP.IDX = symbol node | C = set if successful | ZP.IDY, ZP.TOP, ZP.NEXT<br>ZP.LCURRENT, ZP.LPREVIOUS<br>ZP.LNEXT, ZP.LHEADX |
| **Objects.GetData** ✓ | ZP.IDX = symbol node | ZP.ACC = symbolType\|dataType<br>ZP.NEXT = tokens pointer<br>ZP.IDY = value/args | - |
| **Objects.SetValue** ✓ | ZP.IDX = symbol node<br>ZP.IDY = new value | C = set if successful<br>(NC if not a variable) | - |
| **Objects.GetTokens** ✓ | ZP.IDX = symbol node | ZP.IDY = tokens pointer | - |
| **Objects.SetTokens** ✓ | ZP.IDX = symbol node<br>ZP.IDY = new tokens | - | - |
| **Objects.Destroy** ✓ | X = ZP address of table head | C = set (always succeeds) | ZP.IDY, ZP.TOP, ZP.NEXT<br>ZP.LCURRENT, ZP.LPREVIOUS<br>ZP.LNEXT, ZP.LHEADX |
| **Objects.IterateStart** ✓ | X = ZP address of table head<br>ZP.ACC = filter (0=all) | ZP.IDX = first match<br>C = set if found | ZP.LCURRENT, ZP.LNEXT |
| **Objects.IterateNext** ✓ | - | ZP.IDX = next match<br>C = set if found | ZP.LCURRENT, ZP.LNEXT |
| **Variables.Declare** ✓ | ZP.TOP = name pointer<br>ZP.ACC = symbolType\|dataType<br>ZP.NEXT = initial value<br>ZP.IDY = tokens pointer | C = set if successful | ZP.LCURRENT, ZP.LHEADX, ZP.LNEXT |
| **Variables.Find** ✓ | ZP.TOP = name pointer<br>ZP.SymbolIteratorFilter = expected type | ZP.IDX = symbol node<br>C = set if found | ZP.LCURRENT, ZP.LNEXT, ZP.SymbolTemp0 |
| **Variables.GetValue** ✓ | ZP.IDX = symbol node | ZP.TOP = value<br>ZP.TOPT = dataType<br>C = set if successful | - |
| **Variables.SetValue** ✓ | ZP.IDX = symbol node<br>ZP.TOP = new value | C = set if successful | - |
| **Variables.GetType** ✓ | ZP.IDX = symbol node | ZP.ACC = symbolType\|dataType<br>C = set if successful | - |
| **Variables.GetName** ✓ | ZP.IDX = symbol node | ZP.ACC = name pointer<br>C = set (always succeeds) | - |
| **Variables.GetTokens** ✓ | ZP.IDX = symbol node | ZP.NEXT = tokens pointer<br>C = set (always succeeds) | - |
| **Variables.Remove** ✓ | ZP.TOP = name pointer | C = set if successful | ZP.IDY, ZP.TOP, ZP.NEXT<br>ZP.LCURRENT, ZP.LPREVIOUS<br>ZP.LNEXT, ZP.LHEADX<br>ZP.SymbolTemp0, ZP.SymbolTemp1 |
| **Variables.IterateVariables** ✓ | - | ZP.IDX = first variable<br>C = set if found | ZP.LCURRENT, ZP.LNEXT |
| **Variables.IterateConstants** ✓ | - | ZP.IDX = first constant<br>C = set if found | ZP.LCURRENT, ZP.LNEXT |
| **Variables.IterateAll** ✓ | - | ZP.IDX = first symbol<br>C = set if found | ZP.LCURRENT, ZP.LNEXT |
| **Variables.IterateNext** ✓ | - | ZP.IDX = next symbol<br>C = set if found | ZP.LCURRENT, ZP.LNEXT |
| **Variables.Clear** ✓ | - | C = set (always succeeds) | ZP.IDY, ZP.TOP, ZP.NEXT<br>ZP.LCURRENT, ZP.LPREVIOUS<br>ZP.LNEXT, ZP.LHEADX |
| **Arguments.Add** ✓ | ZP.IDX = function node<br>ZP.TOP = argument name<br>ZP.ACCL = argument type | C = set if successful | ZP.IDY, ZP.TOP, ZP.NEXT<br>ZP.LCURRENT, ZP.LHEADX<br>ZP.LNEXT, ZP.LPREVIOUS<br>ZP.SymbolType, ZP.SymbolNameL/H<br>ZP.SymbolLength |
| **Arguments.Find** ✓ | ZP.IDX = function node<br>ZP.TOP = argument name | ZP.IDY = argument node<br>ZP.ACCL = argument index<br>C = set if found | ZP.LCURRENT, ZP.LNEXT |
| **Arguments.GetType** ✓ | ZP.IDY = argument node | ZP.ACCL = argument type<br>C = set (always succeeds) | - |
| **Arguments.GetName** ✓ | ZP.IDY = argument node | ZP.TOP = name pointer<br>C = set (always succeeds) | - |
| **Arguments.FindByIndex** ✓ | ZP.IDX = function node<br>ZP.ACCL = argument index | ZP.IDY = argument node<br>C = set if found | ZP.LCURRENT, ZP.LNEXT<br>ZP.SymbolTemp0 |
| **Arguments.GetCount** ✓ | ZP.IDX = function node | ZP.ACCL = argument count<br>C = set (always succeeds) | ZP.LCURRENT, ZP.LNEXT |
| **Arguments.IterateStart** ✓ | ZP.IDX = function node | ZP.IDY = first argument<br>C = set if found | - |
| **Arguments.IterateNext** ✓ | ZP.IDY = current argument | ZP.IDY = next argument<br>C = set if found | ZP.LCURRENT |
| **Arguments.Clear** ✓ | ZP.IDX = function node | C = set (always succeeds) | ZP.IDY, ZP.TOP, ZP.NEXT<br>ZP.LCURRENT, ZP.LNEXT<br>ZP.SymbolTemp0, ZP.SymbolTemp1 |

### Notes:
- All APIs preserve registers (A, X, Y) unless otherwise noted
- Table and Objects APIs preserve ZP.IDX and ZP.ACC unless they are outputs
- Memory APIs preserve ZP.ACC, and Free also preserves ZP.IDX
- ZP.M0-M15 slots are munted by Memory operations but are not listed as we don't care about them
- Outputs are never listed under Munts (per the refinement rules)
- Internal workspace variables (ZP.Symbol*) are not listed in public API documentation
```