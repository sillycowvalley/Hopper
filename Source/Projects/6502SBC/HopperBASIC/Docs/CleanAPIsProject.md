# HopperBASIC Call Tree Analysis & Clean API Project Plan

## Project Objectives

### Primary Goal: Clean API Standards
Convert HopperBASIC codebase to "Clean API" standards where:
- **Public methods** preserve caller state except for documented outputs
- **Well-defined scratch space** with clear scope and purpose (ZP.M*, ZP.Symbol*, etc.)
- **No accidental side effects** from utility functions
- **Predictable contracts** for all public interfaces

### Implementation Strategy
1. **Leaf-first approach**: Start with leaf nodes in call tree
2. **One file at a time**: Systematically convert each source file
3. **Document status**: Add "API Status: Clean" comments when complete
4. **Preserve performance**: Maintain scratch space for legitimate needs

---

## Complete Call Tree Analysis

### Level 0: Entry Point
```
HopperBASIC.Hopper()
├── InitializeBASIC()
├── printStartupBanner()
└── interpreterLoop()
```

### Level 1: Core Systems
```
InitializeBASIC()
├── Messages.ClearError() [LEAF]
├── Serial.Initialize() [EXTERNAL - Hopper VM]
├── Memory.InitializeHeapSize() [EXTERNAL - Hopper VM]
├── Stacks.Initialize() [EXTERNAL - Hopper VM]
└── Console.Initialize()

printStartupBanner()
├── Tools.PrintString() [LEAF]
└── Console.CmdMem()

interpreterLoop()
├── Tools.PrintString() [LEAF]
├── Console.ReadLine()
├── Console.ProcessLine()
└── Messages.CheckAndPrintError()
```

### Level 2: Console System
```
Console.Initialize()
├── Tokenizer.Initialize() [LEAF]
└── Objects.Initialize() [LEAF]

Console.ReadLine()
├── Tokenizer.ReadLine()
└── Tokenizer.TokenizeLine()

Console.ProcessLine()
├── Messages.CheckError() [LEAF]
├── Tokenizer.NextToken()
├── Console.cmdNew()
├── Console.cmdList() [STUB]
├── Console.cmdRun() [STUB]
├── Console.cmdClear() [STUB]
├── Console.cmdVars() [STUB]
├── Console.cmdFuncs() [STUB]
├── Console.CmdMem()
├── Console.cmdBye() [LEAF]
└── Statement.Execute()

Console.CmdMem()
├── Tools.PrintString() [LEAF]
├── Memory.Available() [EXTERNAL - Hopper VM]
├── Stacks.PopTop() [EXTERNAL - Hopper VM]
├── Tools.PrintDecimalWord()
└── Tools.PrintString() [LEAF]

Console.cmdNew()
├── HopperBASIC.InitializeBASIC()
└── Messages.PrintOK()
```

### Level 3: Tokenizer System
```
Tokenizer.ReadLine()
├── Serial.WaitForChar() [EXTERNAL - Hopper VM]
└── Serial.WriteChar() [EXTERNAL - Hopper VM]

Tokenizer.TokenizeLine()
├── Messages.ClearError() [LEAF]
├── Tokenizer.skipWhitespace()
├── Tokenizer.getCharType() [LEAF]
├── Tokenizer.appendToTokenBuffer()
└── Tokenizer.findKeyword()

Tokenizer.NextToken()
├── Tokenizer.compareTokenizerPosToLength() [LEAF]
├── Tokenizer.setTokenizerPointer() [LEAF]
├── Tokenizer.incrementTokenizerPos() [LEAF]
└── Tokenizer.skipInlineString()

Tokenizer.GetTokenNumber()
├── Tokenizer.getCharType() [LEAF]
├── Tokenizer.checkMultiply10PlusDigitOverflow()
└── Messages (error setting) [LEAF]

Tokenizer.GetTokenString()
└── [LEAF - address calculation only]
```

### Level 4: Statement Execution
```
Statement.Execute()
├── Statement.executePrint()
├── Statement.executeIf()
├── Statement.executeReturn() [STUB]
├── Statement.executeEnd() [STUB]
├── Statement.executeIdentifier() [STUB]
└── Statement.executeVariableDeclaration()

Statement.executePrint()
├── Tokenizer.NextToken()
├── Messages.CheckError() [LEAF]
├── Expression.Evaluate()
├── Stacks.PopTop() [EXTERNAL - Hopper VM]
├── Tools.PrintDecimalWord()
└── Serial.WriteChar() [EXTERNAL - Hopper VM]

Statement.executeIf()
├── Tokenizer.NextToken()
├── Messages.CheckError() [LEAF]
├── Expression.Evaluate()
├── Stacks.PopTop() [EXTERNAL - Hopper VM]
└── Statement.Execute() [RECURSIVE]

Statement.executeVariableDeclaration()
├── Tokenizer.NextToken()
├── Messages.CheckError() [LEAF]
├── Tokenizer.GetTokenString()
├── Objects.Find()
├── Variables.Find()
├── Variables.GetType()
├── Variables.Remove()
├── Expression.Evaluate()
├── Statement.createTokenStream()
├── Statement.expectEOF()
├── Instructions.CheckRHSTypeCompatibility()
├── Variables.Declare()
└── Memory.Free() [EXTERNAL - Hopper VM]
```

### Level 5: Expression System
```
Expression.Evaluate()
└── Expression.parseComparison()

Expression.parseComparison()
├── Expression.parseLogical()
├── Tokenizer.NextToken()
├── Messages.CheckError() [LEAF]
└── Instructions.[Equal|NotEqual|LessThan|GreaterThan|LessEqual|GreaterEqual]()

Expression.parseLogical()
├── Expression.parseLogicalAnd()
├── Tokenizer.NextToken()
├── Messages.CheckError() [LEAF]
└── Instructions.Or()

Expression.parseLogicalAnd()
├── Expression.parseAddition()
├── Tokenizer.NextToken()
├── Messages.CheckError() [LEAF]
└── Instructions.And()

Expression.parseAddition()
├── Expression.parseMultiplicative()
├── Tokenizer.NextToken()
├── Messages.CheckError() [LEAF]
└── Instructions.[Addition|Subtraction]()

Expression.parseMultiplicative()
├── Expression.parseUnary()
├── Tokenizer.NextToken()
├── Messages.CheckError() [LEAF]
└── Instructions.[Multiply|Divide|Modulo]()

Expression.parseUnary()
├── Tokenizer.NextToken()
├── Messages.CheckError() [LEAF]
├── Stacks.PushTop() [EXTERNAL - Hopper VM]
├── Expression.parsePrimary()
├── Instructions.Subtraction()
└── Instructions.LogicalNot()

Expression.parsePrimary()
├── Tokenizer.GetTokenNumber()
├── Stacks.PushTop() [EXTERNAL - Hopper VM]
├── Tokenizer.NextToken()
├── Messages.CheckError() [LEAF]
├── Expression.parseComparison() [RECURSIVE]
└── Messages (error setting) [LEAF]
```

### Level 6: Symbol Table System
```
Variables.Find()
├── Objects.Find()
├── Objects.GetData()
└── Messages (error setting) [LEAF]

Variables.Declare()
├── Objects.Find()
├── Objects.Add()
└── Messages (error setting) [LEAF]

Variables.Remove()
├── Variables.Find()
├── Objects.GetTokens()
├── Objects.Remove()
└── Memory.Free() [EXTERNAL - Hopper VM]

Objects.Find()
├── Table.GetFirst()
├── Objects.compareNames()
└── Table.GetNext()

Objects.Add()
├── Objects.calculateNodeSize()
├── Table.Add()
└── Objects.initializeNode()

Objects.Remove()
└── Table.Delete()

Objects.calculateNodeSize()
└── Tools.StringLength()

Objects.initializeNode()
└── Objects.copyNameToNode()

Objects.copyNameToNode()
└── Tools.CopyBytes() [LEAF]

Objects.compareNames()
└── Tools.StringCompare() [LEAF]
```

### Level 7: Table System (Lowest Level)
```
Table.GetFirst() [LEAF]
Table.GetNext() [LEAF]
Table.Add()
└── Memory.Allocate() [EXTERNAL - Hopper VM]

Table.Delete()
└── Memory.Free() [EXTERNAL - Hopper VM]

Table.Clear()
└── Table.Delete() [RECURSIVE]
```

### Level 8: Instruction System
```
Instructions.[All Operations]()
├── Stacks.PopTopNext() [EXTERNAL - Hopper VM]
├── Instructions.CheckTypeCompatibility()
├── Messages (error setting) [LEAF]
├── Instructions.doSigns() [LEAF]
├── IntMath.[various]() [EXTERNAL - Hopper VM]
└── Stacks.Push[Top|Next|X]() [EXTERNAL - Hopper VM]

Instructions.CheckTypeCompatibility() [LEAF]
Instructions.CheckRHSTypeCompatibility()
└── Instructions.CheckTypeCompatibility() [LEAF]
```

---

## Leaf Node Identification

### Pure Leaf Methods (No Dependencies)
```
Messages.ClearError()              ✅ CLEAN
Messages.CheckError()              ✅ CLEAN  
Messages.CheckAndPrintError()      ✅ CLEAN
Messages.PrintOK()                 ✅ CLEAN

Tokenizer.Initialize()             ✅ CLEAN
Tokenizer.compareTokenizerPosToLength()  ✅ CLEAN
Tokenizer.getCharType()            ✅ CLEAN
Console.cmdBye()                   ✅ CLEAN (NOP)
Objects.Initialize()               ✅ CLEAN

Instructions.CheckTypeCompatibility()     ✅ CLEAN
Instructions.doSigns()                    ✅ CLEAN

Table.GetFirst()                   ❌ NEEDS REVIEW
Table.GetNext()                    ❌ NEEDS REVIEW
```

### Utility Leaf Methods (Need Clean API Treatment)
```
Tokenizer.incrementTokenizerPos()        ❌ MUNTS: Nothing (only affects intended output)
Tokenizer.incrementTokenBufferLength()   ❌ MUNTS: Nothing (only affects intended output) 
Tokenizer.setTokenizerPointer()          ❌ MUNTS: ZP.IDX (but that's the output)
Tokenizer.setTokenBufferEndPointer()     ❌ MUNTS: ZP.IDX (but that's the output)

Tools.StringLength()                     ❌ MUNTS: ZP.TOP (accidental side effect)
Tools.StringCompare()                    ❌ MUNTS: A, Y (should preserve)
Tools.CopyBytes()                        ❌ MUNTS: Multiple (should preserve registers)
Tools.PrintString()                      ❌ MUNTS: A, Y (should preserve)
Tools.PrintDecimalWord()                 ❌ MUNTS: Multiple (should preserve registers)
```

### Complex Leaf Methods (Need Scratch Space Analysis)
```
Objects.calculateNodeSize()         ❌ USES: ZP.Symbol* scratch space (GOOD)
Objects.initializeNode()            ❌ USES: ZP.Symbol* scratch space (GOOD)  
Objects.copyNameToNode()            ❌ USES: ZP.F* scratch space (review needed)
Objects.compareNames()              ❌ USES: ZP.NEXT temporarily (should preserve)

Tokenizer.appendToTokenBuffer()     ❌ USES: Multiple ZP locations
Tokenizer.findKeyword()             ❌ USES: ZP.ACC temporarily
Tokenizer.skipInlineString()        ❌ USES: Multiple ZP locations
```

---

## Clean API Implementation Plan

### Phase 1: Pure Utility Leaf Methods (High Impact, Low Risk)
**Target Files:** `Tools.asm`, simple `Tokenizer.asm` methods

1. **Tools.StringLength()** - Convert to preserve ZP.TOP, use stack instead
2. **Tools.StringCompare()** - Preserve A, Y registers  
3. **Tools.PrintString()** - Preserve A, Y registers
4. **Tools.PrintDecimalWord()** - Preserve registers, use stack for scratch
5. **Tools.CopyBytes()** - Preserve registers
6. **Tokenizer increment methods** - Add register preservation if needed

**Success Criteria:** All utility functions preserve caller state
**File Status:** Add `// API Status: Clean` comments

### Phase 2: Table System Foundation (Medium Risk)
**Target Files:** `Table.asm`

1. **Table.GetFirst()** - Review and ensure register preservation
2. **Table.GetNext()** - Review and ensure register preservation  
3. **Table.Add()** - Document ZP.L* scratch space usage
4. **Table.Delete()** - Document ZP.L* scratch space usage

**Success Criteria:** Table operations have clean, documented contracts
**File Status:** Update comments with scratch space documentation

### Phase 3: Objects System (Medium-High Risk)
**Target Files:** `Objects.asm`

1. **Objects.compareNames()** - Fix accidental ZP.NEXT corruption
2. **Objects.copyNameToNode()** - Review ZP.F* usage, document or fix
3. **Objects.calculateNodeSize()** - Verify ZP.Symbol* usage is proper
4. **Complex Objects methods** - Document ZP.Symbol* scratch space contracts

**Success Criteria:** Objects system uses only documented scratch space
**File Status:** Document ZP.Symbol* scratch space contracts

### Phase 4: Tokenizer Complex Methods (High Risk)
**Target Files:** `Tokenizer.asm`

1. **Tokenizer.appendToTokenBuffer()** - Review ZP usage patterns
2. **Tokenizer.findKeyword()** - Fix ZP.ACC temporary usage
3. **Tokenizer.skipInlineString()** - Review and clean up ZP usage
4. **Complex tokenizer methods** - Establish dedicated scratch space if needed

**Success Criteria:** Tokenizer has clean public API with documented internals

### Phase 5: Work Up Call Tree (Ongoing)
**Target Files:** `Variables.asm`, `Functions.asm`, `Arguments.asm`, `Statement.asm`, `Expression.asm`, `Instructions.asm`

1. **Symbol table layers** - Ensure they only use documented scratch space
2. **Statement execution** - Clean up register handling
3. **Expression parsing** - Establish clean parsing contracts
4. **Instruction execution** - Verify stack-based parameter passing

### Phase 6: Integration & Testing
1. **Full system testing** - Ensure no functionality regression
2. **Documentation update** - All files marked with API status
3. **Performance verification** - Ensure scratch space optimizations remain

---

## Success Metrics

### Code Quality Indicators
- ✅ All leaf methods preserve caller registers (except documented outputs)
- ✅ All scratch space usage documented with clear scope
- ✅ No accidental side effects in utility functions
- ✅ Consistent error handling patterns
- ✅ All source files marked with "API Status: Clean"

### Performance Indicators  
- ✅ No performance regression from added register preservation
- ✅ Scratch space optimizations maintained where justified
- ✅ Memory allocation patterns unchanged

### Maintainability Indicators
- ✅ Clear contracts for all public methods
- ✅ Predictable behavior for all utility functions
- ✅ Easy to add new functionality without breaking existing contracts
- ✅ Debugging simplified by eliminating accidental side effects

---

## File Conversion Checklist

When converting a file to Clean API standards:

### 1. Analyze Current State
- [ ] Identify all public methods
- [ ] Map current side effects and register usage
- [ ] Identify legitimate scratch space needs
- [ ] Document current contracts

### 2. Fix Accidental Side Effects
- [ ] Add register preservation where needed
- [ ] Use stack instead of random ZP locations
- [ ] Preserve caller's working variables

### 3. Document Legitimate Contracts
- [ ] Document scratch space usage clearly
- [ ] Ensure scratch space doesn't leak across API boundaries
- [ ] Verify scratch space scope is appropriate

### 4. Update Documentation
- [ ] Add "API Status: Clean" comment block
- [ ] Document any scratch space used
- [ ] Update method contracts in comments
- [ ] Note any preserved exceptions

### 5. Test Integration
- [ ] Verify no functionality regression
- [ ] Test error handling paths
- [ ] Confirm performance characteristics
- [ ] Validate with complex expressions/statements

This systematic approach will transform HopperBASIC into a much more maintainable and predictable codebase while preserving the performance benefits of legitimate scratch space usage.