# HopperBASIC Refactoring Best Practices and Rules

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

### 4. Optimization Rules
- Only skip preservation if:
  1. A called function already preserves it, AND
  2. You don't modify it yourself between/after the calls
- Remove redundant push/pop only when you can prove the value isn't changed

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
- **Z | NZ**: Use for comparisons
  - Comment as "Set Z" or "Set NZ" (not "0" or "1")

## Error Handling
- Set error messages in ZP.LastErrorL/H before returning NC
- Use meaningful error messages from Messages unit
- Never fail silently - always set an error or use BRK with TODO pattern

## Enum Usage
- Use direct enum syntax: `LDA #SymbolType.VARIABLE`
- Don't qualify with namespace: ~~`LDA #Objects.SymbolType.VARIABLE`~~

## Memory Management Integration
- Memory.Allocate() munts: ZP.IDY, ZP.TOP, ZP.NEXT (preserves ZP.ACC)
- Memory.Free() munts: ZP.IDY, ZP.TOP, ZP.NEXT (preserves ZP.IDX, ZP.ACC)
- Both preserve all registers (A, X, Y)

## Summary
The goal is to create consistent, well-documented APIs that:
1. Clearly state their contracts (inputs/outputs/side effects)
2. Preserve key working variables unless they're outputs
3. Use single exit points for complex logic
4. Return consistent success/failure indicators
5. Can be optimized by removing only truly redundant operations



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
│   │           │   ├── Table.GetFirst() √
│   │           │   ├── Table.GetNext() √
│   │           │   ├── Table.Add() √
│   │           │   │   └── Memory.Allocate() √
│   │           │   ├── Table.Delete() √
│   │           │   │   └── Memory.Free() √
│   │           │   └── Table.Clear() √
│   │           │       └── Table.Delete() [loop] √
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
│   │           ├── Objects.Add()
│   │           │   ├── Objects.Find()
│   │           │   │   ├── Table.GetFirst() √
│   │           │   │   ├── Objects.compareNames()
│   │           │   │   └── Table.GetNext() √
│   │           │   ├── Objects.calculateNodeSize()
│   │           │   ├── Table.Add() √
│   │           │   └── Objects.initializeNode()
│   │           │       ├── Objects.copyNameToNode()
│   │           │       └── Tools.CopyBytes()
│   │           ├── Objects.GetData()
│   │           ├── Objects.SetValue()
│   │           ├── Objects.GetTokens()
│   │           ├── Objects.SetTokens()
│   │           ├── Objects.Remove()
│   │           │   └── Table.Delete() √
│   │           ├── Objects.IterateStart()
│   │           │   ├── Table.GetFirst() √
│   │           │   └── Objects.findNextMatch()
│   │           ├── Objects.IterateNext()
│   │           │   ├── Table.GetNext() √
│   │           │   └── Objects.findNextMatch()
│   │           └── Objects.Destroy()
│   │               └── Table.Clear() √
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
│   │           │   └── Memory.Allocate() √
│   │           ├── Variables.Declare()
│   │           │   ├── Objects.Find()
│   │           │   └── Objects.Add()
│   │           ├── Variables.Find()
│   │           │   ├── Objects.Find()
│   │           │   └── Objects.GetData()
│   │           ├── Variables.GetValue()
│   │           │   └── Objects.GetData()
│   │           ├── Variables.SetValue()
│   │           │   ├── Objects.GetData()
│   │           │   └── Objects.SetValue()
│   │           ├── Variables.GetType()
│   │           │   └── Objects.GetData()
│   │           ├── Variables.GetName()
│   │           ├── Variables.GetTokens()
│   │           │   └── Objects.GetTokens()
│   │           ├── Variables.Remove()
│   │           │   ├── Variables.Find()
│   │           │   ├── Objects.GetTokens()
│   │           │   ├── Objects.Remove()
│   │           │   └── Memory.Free() √
│   │           ├── Variables.IterateVariables()
│   │           │   └── Objects.IterateStart()
│   │           ├── Variables.IterateConstants()
│   │           │   └── Objects.IterateStart()
│   │           ├── Variables.IterateAll()
│   │           │   └── Objects.IterateStart()
│   │           ├── Variables.IterateNext()
│   │           │   └── Objects.IterateNext()
│   │           └── Variables.Clear()
│   │               ├── Table.GetFirst() √
│   │               ├── Objects.GetTokens()
│   │               ├── Memory.Free() √
│   │               └── Table.Delete() √
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
│   │           │   ├── Objects.Find()
│   │           │   └── Objects.Add()
│   │           ├── Functions.Find()
│   │           │   ├── Objects.Find()
│   │           │   └── Objects.GetData()
│   │           ├── Functions.GetSignature()
│   │           │   └── Objects.GetData()
│   │           ├── Functions.GetBody()
│   │           │   └── Objects.GetTokens()
│   │           ├── Functions.GetName()
│   │           ├── Functions.SetArguments()
│   │           │   └── Objects.SetValue()
│   │           ├── Functions.GetArguments()
│   │           │   └── Objects.GetData()
│   │           ├── Functions.Remove()
│   │           │   ├── Functions.Find()
│   │           │   ├── Arguments.Clear()
│   │           │   │   ├── Table.GetFirst() √
│   │           │   │   ├── Table.Delete() √
│   │           │   │   └── Memory.Free() √
│   │           │   └── Objects.Remove()
│   │           ├── Functions.IterateFunctions()
│   │           │   └── Objects.IterateStart()
│   │           ├── Functions.IterateNext()
│   │           │   └── Objects.IterateNext()
│   │           └── Functions.Clear()
│   │               ├── Table.GetFirst() √
│   │               ├── Arguments.Clear()
│   │               ├── Objects.GetTokens()
│   │               ├── Memory.Free() √
│   │               └── Table.Delete() √
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
│   │           ├── Arguments.Add()
│   │           │   ├── Arguments.calculateArgumentNodeSize()
│   │           │   ├── Memory.Allocate() √
│   │           │   └── Arguments.initializeArgumentNode()
│   │           │       ├── Arguments.copyArgumentNameToNode()
│   │           │       └── Tools.CopyBytes()
│   │           ├── Arguments.Find()
│   │           │   └── Arguments.compareArgumentNames()
│   │           ├── Arguments.GetType()
│   │           ├── Arguments.GetName()
│   │           ├── Arguments.FindByIndex()
│   │           ├── Arguments.GetCount()
│   │           ├── Arguments.IterateStart()
│   │           └── Arguments.IterateNext()
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


Here's a table of the APIs improved so far:

| Qualified Name | Inputs | Outputs | Munts |
|----------------|--------|---------|-------|
| **Table.GetFirst** √ | X = ZP address of list head pointer | ZP.IDX = first node address<br>C = set if found, clear if empty | - |
| **Table.GetNext** √ | ZP.IDX = current node | ZP.IDX = next node address<br>C = set if found, clear if end | - |
| **Table.Add** √ | X = ZP address of list head pointer<br>ZP.ACC = node size (16-bit) | ZP.IDX = new node address<br>C = set if successful, clear if allocation failed | ZP.IDY, ZP.TOP, ZP.NEXT<br>ZP.LCURRENT, ZP.LHEADX, ZP.LNEXT |
| **Table.Delete** √ | X = ZP address of list head pointer<br>ZP.IDX = node to delete | C = set if successful, clear if not found | ZP.IDY, ZP.TOP, ZP.NEXT<br>ZP.LCURRENT, ZP.LPREVIOUS<br>ZP.LNEXT, ZP.LHEADX |
| **Table.Clear** √ | X = ZP address of list head pointer | C = set (always succeeds)<br>List head = 0x0000 | ZP.IDY, ZP.TOP, ZP.NEXT<br>ZP.LCURRENT, ZP.LPREVIOUS<br>ZP.LNEXT, ZP.LHEADX |
| **Memory.Allocate** √ | ZP.ACC = requested size (16-bit) | ZP.IDX = allocated address<br>(0x0000 if failed) | ZP.IDY, ZP.TOP, ZP.NEXT |
| **Memory.Free** √ | ZP.IDX = address to free<br>(must not be 0x0000) | - | ZP.IDY, ZP.TOP, ZP.NEXT |

Notes:
- All APIs preserve registers (A, X, Y) unless otherwise noted
- Table APIs preserve ZP.IDX and ZP.ACC unless they are outputs
- Memory APIs preserve ZP.ACC, and Free also preserves ZP.IDX
- ZP.M0-M15 slots are munted by Memory operations but are not listed as we don't care about them
- Outputs are never listed under Munts (per the refinement rules)
