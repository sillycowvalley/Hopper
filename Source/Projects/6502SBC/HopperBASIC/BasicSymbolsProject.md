# HopperBASIC Symbol Table Design Document

## Overview

HopperBASIC uses a unified symbol table to store all program identifiers: variables, constants, and functions. This design provides a clean foundation for Phase 1 completion and future expansion.

## Architecture Decisions

### Single Unified Table
- **Decision**: One symbol table for all identifier types (variables, constants, functions)
- **Rationale**: 
  - FORGET command works across all types with single lookup
  - Simpler implementation (one set of list operations)
  - Unified namespace prevents naming conflicts
  - Code reuse reduces ROM footprint

### Memory Management
- **Storage**: Hopper VM heap using `Memory.Allocate()` and `Memory.Free()`
- **Structure**: Simple linked list (no optimization for lookup speed)
- **Rationale**: Small scale (5-20 identifiers typical), proven heap implementation

### Node Structure (Fixed Layout)

Each symbol is stored as a heap-allocated node with fixed field offsets:

```
Heap Node Layout:
[2-byte heap header] [1-byte symbolType] [1-byte dataType] [2-byte value/address] [2-byte next] [null-terminated name]

Field Offsets from node start:
- Offset 0: symbolType (VARIABLE/CONSTANT/FUNCTION)
- Offset 1: dataType (BasicType.INT/WORD/BIT, or 0 for functions) 
- Offset 2-3: value/address (value for primitives, heap address for objects)
- Offset 4-5: next pointer (0x0000 = end of list)
- Offset 6+: null-terminated name string
```

### Symbol Types

```hopper
enum SymbolType
{
    VARIABLE = 0x01,   // Mutable values (INT, WORD, BIT, future: BYTE, STRING, ARRAY)
    CONSTANT = 0x02,   // Immutable values (defined with CONST)
    FUNCTION = 0x03    // Executable code blocks (including main program)
}
```

### Value/Address Field Usage
- **Primitive types** (INT, WORD, BIT): Direct value storage
- **Object types** (STRING, ARRAY, FUNCTION): Heap address of separately allocated data
- **Benefits**: Allows re-allocation of objects while keeping symbol table address stable

## Implementation Architecture

### Two-Layer Design

#### Layer 1: GenericList (Reusable)
Pure list operations with no symbol-specific knowledge:

```hopper
unit GenericList
{
    ListCreate();                               // Returns empty list (0x0000)
    ListAdd(listHead, nodeSize);               // Allocate node, link to list
    ListRemove(listHead, nodeAddr);            // Unlink and free node  
    ListClear(listHead);                       // Free entire list
    ListFind(listHead, nameOffset, targetName); // Find node by name
}
```

#### Layer 2: SymbolTable (BASIC-Specific)
Symbol operations using GenericList foundation:

```hopper
unit SymbolTable
{
    SymbolCreate();                            // Initialize empty table
    SymbolLookup(name);                        // Find symbol by name
    SymbolAdd(name, symbolType, dataType, value);
    SymbolRemove(name);                        // Also frees referenced objects
    SymbolDestroy();                           // Free table + all objects
    SymbolIterate(symbolType);                 // Type-specific iteration
}
```

## Memory Layout

### Zero Page Allocation
- **0x3A/0x3B**: SymbolListL/SymbolListH (global symbol table head pointer)
- **Available slots**: 0x3C-0x3F remain available for future features

### Runtime Resolution
- **Approach**: Identifiers stored as strings during tokenization
- **Lookup**: Performed at runtime when statements execute
- **Rationale**: Supports forward references, dynamic redefinition, simpler implementation

## Object Management Strategy

### Phase 1 (Primitives Only)
- **Types**: INT, WORD, BIT stored directly in value field
- **No reference counting needed**

### Phase 3 (Objects)
- **Ownership**: Simple ownership model - symbol table owns referenced objects
- **No sharing**: Each object owned by exactly one symbol
- **Cleanup**: SymbolRemove() and SymbolDestroy() free referenced objects
- **Future**: Could add reference counting if sharing needed

## Implementation Plan

### Phase 1: Foundation
1. **GenericList implementation** with comprehensive tests
2. **SymbolTable layer** using GenericList
3. **Integration** with BASIC interpreter
4. **Variable support**: INT, WORD, BIT declarations and assignments

### Testing Strategy
Standalone test suite covering:
- Empty list operations
- Single/multiple node operations  
- Name-based lookups
- Memory leak verification
- Edge cases (duplicate names, empty strings, etc.)

### Phase 1 Integration Points
- **Variable declarations**: `INT x`, `WORD count`, `BIT flag`
- **Assignment statements**: `x = expr`
- **Expression evaluation**: Variable name lookup
- **Console commands**: `VARS`, `FORGET name`, `CLEAR`

## Benefits

1. **Minimal code footprint** - Single table implementation
2. **Predictable behavior** - Consistent operations across identifier types
3. **Easy debugging** - One data structure to inspect
4. **Flexible execution** - Supports forward references and redefinition
5. **Memory efficient** - No duplicate management code
6. **Extensible** - Clean foundation for Phase 3 features

## Performance Characteristics

- **Lookup time**: O(n) where n ≈ 20 maximum
- **Memory overhead**: Minimal linked list nodes
- **Runtime cost**: Negligible on 6502 systems
- **Development cost**: Low complexity, maintainable

---

This design provides the foundation needed to complete Phase 1 of HopperBASIC while establishing a clean architecture for future expansion.