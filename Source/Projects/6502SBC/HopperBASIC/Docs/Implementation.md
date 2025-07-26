# HopperBASIC Critical Functional Tests Implementation Plan

## Overview
We are implementing 9 critical functional tests in `testscenarios.asm` that validate HopperBASIC integration patterns. These tests go beyond unit testing to verify complete workflows that the actual interpreter will use.

## Implementation Status

### ✅ COMPLETED (2/9)
1. **testVariableReassignmentAfterDeclaration()** - ✅ DONE
   - Tests multiple variable updates after initial declaration
   - Validates Find/SetValue/GetValue pattern preservation
   - Includes rapid update testing for real-world usage patterns

2. **testClearCommandImplementation()** - ✅ DONE
   - Tests CLEAR command behavior: reset variables to type defaults
   - Verifies constants remain unchanged during CLEAR
   - Validates proper iteration through variables only

### 🔄 TO IMPLEMENT (7/9)

#### High Priority - Core Language Features
3. **testMainProgramStorage()** - TODO
   - **Purpose**: Store/retrieve main program block (BEGIN/END)
   - **Methods**: Functions.Declare() with "main", Functions.Find()
   - **Pattern**: Store main program as special function with lowercase name
   - **Key test**: "main" cannot be created by user (protected name)

4. **testForgetCommandIntegration()** - TODO
   - **Purpose**: Complete symbol removal with resource cleanup
   - **Methods**: Variables.Remove(), Functions.Remove()
   - **Pattern**: Find first, then remove with token memory cleanup
   - **Key test**: Proper cleanup of both variables and functions

5. **testSafeSymbolCreationPattern()** - TODO
   - **Purpose**: HopperBASIC's check-before-create pattern
   - **Methods**: Variables.Find(), Functions.Find(), then Declare()
   - **Pattern**: Always check both namespaces before creating
   - **Key test**: Prevent name conflicts between variables and functions

#### Medium Priority - Advanced Features
6. **testTokenMemoryLifecycle()** - TODO
   - **Purpose**: Token memory management across symbol operations
   - **Methods**: Variables.GetTokens(), Variables.Remove()
   - **Pattern**: Proper allocation and cleanup of token streams
   - **Key test**: No memory leaks after symbol removal

7. **testMixedGlobalSymbolUsage()** - TODO
   - **Purpose**: Access multiple global symbols from function context
   - **Methods**: Variables.Find() with different symbol types
   - **Pattern**: Global variable/constant access patterns
   - **Key test**: Mixed variable and constant lookup

#### Lower Priority - System Features
8. **testListCommandDataRetrieval()** - TODO
   - **Purpose**: Reconstruct complete program for display
   - **Methods**: Variables.IterateAll(), Functions.IterateFunctions()
   - **Pattern**: Comprehensive iteration for program listing
   - **Key test**: All symbols accessible in declaration order

9. **testSymbolTableSerializationReadiness()** - TODO
   - **Purpose**: Prepare for SAVE/LOAD session state
   - **Methods**: All iteration methods, GetTokens(), GetBody()
   - **Pattern**: Complete state enumeration
   - **Key test**: Consistent iteration order for reliable save/restore

## Test Implementation Rules

### Architecture Rules Applied
- **RULE #1**: No silent failures - use BRK with proper error messages
- **RULE #2**: No unauthorized zero page usage - use stack for temporaries
- **RULE #3**: Clear flag comments (Set Z / Set NZ, not 0/1)
- **RULE #4**: Complete methods only - no "rest remains the same"
- **RULE #5**: Debug-first approach for bug analysis
- **RULE #9**: Direct enum syntax (SymbolType.VARIABLE not Objects.SymbolType.VARIABLE)

### Test Structure Pattern
Each test follows this pattern:
```assembly
testXxxxxYyyyy()
{
    // 1. Print test header with description
    LDA #(scenarioDescX % 256)
    STA ZP.TOPL
    LDA #(scenarioDescX / 256)
    STA ZP.TOPH
    Test.PrintTestHeader();
    
    // 2. Setup test data (allocateTestTokens() if needed)
    
    // 3. Execute test scenario with proper error codes
    // Each failure gets unique error code (0x10, 0x11, etc.)
    
    // 4. Cleanup and report result
    Variables.Clear();  // Always cleanup
    SEC  // Pass or CLC // Fail
    Test.PrintResult();
}
```

### Error Code Allocation
- **0x10-0x1F**: testVariableReassignmentAfterDeclaration() ✅
- **0x20-0x3F**: testClearCommandImplementation() ✅  
- **0x40-0x4F**: testMainProgramStorage() - RESERVED
- **0x50-0x5F**: testForgetCommandIntegration() - RESERVED
- **0x60-0x6F**: testSafeSymbolCreationPattern() - RESERVED
- **0x70-0x7F**: testTokenMemoryLifecycle() - RESERVED
- **0x80-0x8F**: testMixedGlobalSymbolUsage() - RESERVED
- **0x90-0x9F**: testListCommandDataRetrieval() - RESERVED
- **0xA0-0xAF**: testSymbolTableSerializationReadiness() - RESERVED

## Implementation Strategy

### One Test At A Time
We implement these tests **one at a time** to:
1. Validate each integration pattern thoroughly
2. Ensure proper error handling and cleanup
3. Build confidence in the symbol table foundation
4. Prepare for parser integration step by step

### Current Focus
Next to implement: **testMainProgramStorage()** (scenario #3)
- Critical for BEGIN/END block handling
- Establishes "main" function pattern
- Validates protected name handling

### Integration Readiness
These tests validate that the symbol table system is ready for HopperBASIC parser integration by ensuring:
- All BASIC language patterns work correctly
- Memory management is robust
- Error handling is comprehensive
- Resource cleanup is automatic

## Development Workflow
1. **Select next test** from TO IMPLEMENT list
2. **Review test specification** from TestCoverage.md
3. **Implement complete test** following architecture rules
4. **Test thoroughly** with error injection
5. **Update this document** to mark COMPLETED
6. **Move to next test**

This systematic approach ensures each critical functional pattern is validated before proceeding to parser integration, providing a robust foundation for the complete HopperBASIC interpreter.