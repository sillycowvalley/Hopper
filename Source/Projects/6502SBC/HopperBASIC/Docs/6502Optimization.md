# HopperBASIC Size Optimization Analysis Guide
**Document Type: Optimization Strategy**

## Project Context
- **Target**: 65C02S processor (full instruction set available)
- **Constraint**: 32K EEPROM hard limit
- **Current Issue**: Approaching size limit with DEBUG & TRACE enabled (~2.5K trace strings)
- **Optimizer**: Custom peephole optimizer already handles basic patterns (BRA vs JMP, PHA/PLA, STZ, dead code, small method inlining)

## Top 3 Optimization Opportunities

### 1. Register Over-Preservation (Wait for Stability)
**Impact**: Potentially massive  
**Status**: Postponed until codebase stabilizes  
**Approach**:
- Build call graph from BASIC.lst
- Track register usage per function
- Identify leaf functions with unnecessary preservation
- Find preservation chains across call levels
- Look for patterns like:
  ```
  PHA/PHX/PHY    ; Function entry
  ; ... code that never touches Y ...  
  PLY/PLX/PLA    ; Function exit
  ```

### 2. Zero Page Optimization (Immediate Focus)
**Impact**: Smaller instructions + speed improvement  
**Current Usage**: ~64 of 256 available slots  
**Analysis Targets**:
- **Hot variables** using absolute addressing repeatedly
- **Temporary values** loaded frequently to registers
- **Loop variables** and iteration pointers
- **Buffer pointers** (BasicTokenizerBuffer, BasicOpcodeBuffer, etc.)
- **Frequently accessed structure offsets**
- **Parser/compiler state variables**

**What to Look For**:
```
LDA $0A00,X    ; 3 bytes - could be ZP
STA $0C00,Y    ; 3 bytes - could be ZP
LDA BasicTokenizerBuffer,X  ; Repeated buffer access
```

### 3. Pattern Recognition for Optimizer Enhancement
**Impact**: Improves all future builds, reaches beyond this project  
**Beyond Current Peephole Patterns**:
- Redundant flag operations (beyond CLC/SEC)
- Register transfer sequences with shorter alternatives
- 65C02-specific instruction opportunities (missed STZ, BRA, etc.)
- Common multi-instruction patterns that could be single instructions
- Repeated error handling sequences

**Examples to Find**:
```
CMP #0         ; Redundant - previous operation set flags
BEQ label      

LDA value      ; Could use 65C02 TRB/TSB for bit operations
AND #$FE
STA value
```

## Additional Opportunities Identified

### 4. Code Deduplication
**Areas**: Parser/tokenizer and code generator (mentioned as cut-and-paste heavy)
- Similar instruction sequences differing only in parameters
- Common error paths that could be subroutines
- Table-driven solutions for repetitive operations

## Analysis Output Format
```
PATTERN: [Description]
LOCATION: [Address range or file:line]
CURRENT: [Current instruction sequence]
OPTIMIZED: [Proposed improvement]
SAVINGS: [Bytes saved]
FREQUENCY: [Occurrences found]
```

## Zero Page Analysis Methodology
1. **Scan for absolute address usage frequency**
2. **Identify access patterns** (read-only, read-write, pointer dereferencing)
3. **Calculate byte savings** (2-byte vs 3-byte instructions)
4. **Prioritize by**: frequency × savings
5. **Consider locality** (variables used together in hot paths)
6. **Review existing ZP allocation** for consolidation opportunities


#Tool Building Potential#


## 1. **Call Tree Analyzer** (For Register Preservation)
```csharp
// Outputs:
// - Call graph with register usage per function
// - Unnecessary push/pop operations
// - Register lifetime analysis
// - Estimated bytes saved per optimization
```

## 2. **Zero Page Usage Profiler**
```csharp
// Tracks:
// - Memory access frequency by address
// - Instruction types using each address
// - Hot path analysis
// - Recommends optimal ZP allocations
// Could even simulate different ZP allocation strategies
```

## 3. **Size Regression Monitor**
```csharp
// Tracks per build:
// - Total size by section
// - Function sizes over time
// - Alerts on size increases > threshold
// - Identifies which changes caused bloat
```

## 4. **Pattern Mining Tool**
```csharp
// Finds:
// - Repeated instruction sequences
// - Candidates for subroutine extraction
// - Peephole optimization opportunities
// - Outputs patterns in format your optimizer can consume
```

## 5. **Cycle Counter/Profiler**
```csharp
// Using your cycle-accurate emulator:
// - Hot path identification
// - Per-function cycle counts
// - Bottleneck analysis
// - Compare before/after optimization
```

## Tool Architecture Benefits

Since you already have:
- A working 6502 emulator (cycle-accurate!)
- Assembly/disassembly infrastructure
- Symbol table information

We could build tools that:
1. **Parse BASIC.lst** format directly
2. **Run automated analysis** on every build
3. **Generate reports** comparing builds
4. **Feed back into your optimizer** with discovered patterns


## Key Constraints
- Must preserve existing Hopper VM runtime API behavior
- Zero page slots must not conflict with hardware mappings
- Optimizations must maintain debuggability where possible
- Speed optimizations deferred - size is primary concern

## Next Steps
1. Perform zero page hotspot analysis on BASIC.lst
2. Document findings with concrete examples
3. Propose specific ZP allocations with rationale
4. Identify patterns for optimizer enhancement as discovered


