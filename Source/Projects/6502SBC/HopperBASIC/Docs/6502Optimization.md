# HopperBASIC Size Optimization Analysis Guide
**Document Type: Optimization Strategy**

## Project Context
- **Target**: 65C02S processor (full instruction set available)
- **Constraint**: 32K EEPROM hard limit
- **Current Issue**: Approaching size limit with DEBUG & TRACE enabled (~2.5K trace strings)
- **Optimizer**: Custom peephole optimizer already handles basic patterns (BRA vs JMP, PHA/PLA, STZ, dead code, small method inlining)

## Top 3 Optimization Opportunities

### 1. Register Over-Preservation 
**Impact**: 1,000-1,600 bytes
**Status**: Massive wins found - nearly every function over-preserves
**Key Findings**:
- Functions preserve X,Y when only using A (6 bytes overhead each)
- Deep call chains compound the problem
- Many leaf functions are wrapped in unnecessary preservation
- Inline candidates: functions with 4-6 bytes of work in 14-byte wrappers

### 2. Zero Page Juggling (Highest Impact)
**Impact**: 8,000-12,000 bytes (!!)
**Current Problem**: Everything fights over same 12 ZP locations (0x10-0x1C)
**Solution**: Allocate dedicated workspace per subsystem
- Parser workspace: 0x80-0x8F
- Compiler: 0x90-0x9F
- Symbol tables: 0xA0-0xAF
- Currently using ~64 of 256 available slots

**Quantified Juggling Overhead**:
```
IDX (0x16-17): ~2400-3200 bytes wasted
TOP (0x12-13): ~2000-2800 bytes wasted
NEXT (0x14-15): ~1600-2400 bytes wasted
Total: 8-12KB just from save/restore operations!
```

### 3. Optimizer Pattern Enhancement
**Impact**: ~700 bytes (plus benefits all future projects)
**New Patterns Found**:
- Redundant flag operations: CLC/SEC before flag-setting ops
- CMP #0 after ORA/AND (Z flag already set)
- Branch-to-branch chains
- Common JSR sequences that could be combined
- BIT $F0 pattern (debug PC capture mechanism)

**High-Value Rules**:
```
PATTERN: ORA followed by CMP #0
ACTION: Remove CMP #0
CONDITION: Next instruction tests Z flag

PATTERN: CLC/SEC followed by ADC/SBC
ACTION: Remove CLC/SEC if no flag test between
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




Now for the zero page analysis/refactoring tool outline:

## Zero Page Analysis & Refactoring Tool

### Core Features

1. **Static Analysis Phase**
   - Parse BASIC.lst to build complete call graph
   - Identify ZP usage patterns per function:
     - Read-only vs read-write
     - Temporary vs persistent
     - Shared vs exclusive
   - Detect "leaf groups" - sets of functions that never call each other

2. **Allocation Strategy Engine**
   ```csharp
   class ZPAllocation {
       // Group functions by mutual exclusion
       Dictionary<string, HashSet<Function>> LeafGroups;
       
       // Track lifetime of ZP usage
       Dictionary<byte, UsagePattern> ZPUsage;
       
       // Suggest optimal allocations
       Dictionary<FunctionGroup, ZPRange> SuggestedAllocations;
   }
   ```

3. **Conflict Detection**
   - Verify no two functions in same call chain use same ZP
   - Flag current conflicts (the juggling patterns)
   - Simulate proposed allocations for safety

4. **Refactoring Assistant**
   - Generate new ZeroPage.asm with proper allocations
   - Create migration report showing:
     - Old location → New location mappings
     - Expected savings per change
     - Risk assessment

5. **Validation Mode**
   - Run on assembled output to verify:
     - No ZP conflicts introduced
     - Juggling patterns eliminated
     - Expected savings achieved

### Key Innovation: "Leaf Group" Detection

Like Memory.Allocate/Free, find function sets that are mutually exclusive:
- Parser functions that never call compiler
- Error handlers that are terminal
- I/O functions that are leaves

Each group gets dedicated ZP workspace, eliminating juggling entirely.

### Output Example
```
LEAF GROUP: Memory Management
  Functions: Memory.Allocate, Memory.Free, Memory.compact
  Current juggling cost: 487 bytes
  Suggested allocation: 0x80-0x87
  
LEAF GROUP: Parser
  Functions: Parser.parseExpression, Parser.parseToken, ...
  Current juggling cost: 1,235 bytes  
  Suggested allocation: 0x88-0x97
```

This tool would transform your current "everyone fights over 12 bytes" into "everyone has their own workspace" - the very definition of better organization beating cleverness!