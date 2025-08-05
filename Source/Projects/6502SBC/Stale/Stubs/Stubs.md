## Task: Converting Source Files to Reference Stubs

### Objective
Reduce project knowledge volume by converting stable source files from full implementation to "reference stubs" that contain only the public API interface without implementation details.

### Conversion Rules

1. **Keep the file structure**:
   - Preserve unit declaration and uses statements
   - Keep all API status comments and documentation blocks
   - In the absence of API comments, generate some based on the source for the stub version of the file.
   - Maintain all explanatory comments about data structures, memory layout, etc.

2. **Handle conditional compilation properly**:
   - **INCLUDE** code sections that are defined for our project
   - **EXCLUDE** code sections that would not be compiled for our project
   - **STOP and ASK** if encountering any new conditional symbol not in the known list

3. **Known conditional symbols for HopperBASIC project**:
   - `HOPPER_BASIC` - Always defined for our project
   - `DEBUG` - Sometimes defined (we use it)
   - `FASTINTS` - Defined for our project
   - `HAS_SERIAL_ISR` - Defined for our project
   - `CPU_65C02S` - Defined for our project
   - `ZEROPAGE_IO` - Defined for our project
   - `ACIA_6850` - Defined for our project
   - `ALLOW_EEPROM` - Defined for our project
   - `INLINE_EXPANSIONS` - Defined for our project

3. **Known conditional symbols that are NOT part of the HopperBASIC project**:
   - `TIGGERC`
   - `MECB6502_IO`
   - `APPLE_I`
   - `ACIA_6551`
   - `BENEATER_IO`
   - `X16_IO`
   - `MEMDEBUG`
   - `CHECKED`
   - `LONGS`

4. **Conditional compilation processing**:
   - Include `#ifdef HOPPER_BASIC`, `#ifdef DEBUG`, `#ifdef CPU_65C02S`, etc. sections
   - Exclude `#ifndef HOPPER_BASIC`, `#ifdef BENEATER_IO`, `#ifdef X16_IO`, etc. sections
   - For `#if !defined(...)` statements, evaluate based on our known symbols
   - When encountering **any new conditional symbol** not in the above list, **STOP immediately and ASK** whether that symbol is defined for our project

5. **Strip implementation, keep declarations**:
   - Convert all public methods from full implementation to declaration-only
   - Remove all method bodies (everything between the `{` and `}`)
   - Keep the method signature and all documentation comments
   - Preserve parameter and return value documentation
   - Apply conditional compilation rules to method declarations too

6. **Public symbols only**:
   - Keep all public constants (uppercase identifiers)
   - Keep all public enums and their values
   - Keep all public method declarations
   - Remove private/internal methods entirely
   - Remove friend declarations and internal implementation details
   - Apply conditional compilation to constants and enums
   - remove `uses` statements
   - remove `friend` statements

7. **Preserve critical documentation**:
   - Keep API status blocks
   - Keep memory layout comments (like "Node layout: [0-1: next pointer] [user data]")
   - Keep scratch space usage documentation
   - Keep data structure explanations
   - Remove implementation-specific comments
   - Keep conditional compilation comments that explain platform differences

### Example Pattern
```hopper
// BEFORE (full implementation):
#ifdef CPU_65C02S        
    STZ ZP.SP
    STZ ZP.BP
#else
    LDA #0
    STA ZP.SP
    STA ZP.BP
#endif

GetFirst()
{
    // Read the node pointer value stored at that head address
    LDA 0x00, X
    STA ZP.IDXL
    // ... rest of implementation
}

// AFTER (reference stub, assuming CPU_65C02S is defined):
#ifdef CPU_65C02S        
    STZ ZP.SP
    STZ ZP.BP
#endif

GetFirst()
```

### Critical Safety Rule
**If you encounter ANY conditional compilation symbol that is not in the known list above, STOP processing immediately and ASK the user whether that symbol is defined for the HopperBASIC project before continuing.**

### When to Apply
Use this for "relatively stable" source files that:
- Have well-established public APIs
- Are not frequently modified
- Serve as foundation/utility code
- Are referenced by many other files but their internals don't change often

### Files to Consider
Start with foundational files like:
- `Table.asm` (generic linked list operations)
- `Messages.asm` (error handling utilities)  
- `Tools.asm` (string and utility functions)
- Other stable utility units

### Benefits
- Reduces project knowledge storage requirements
- Focuses attention on the public API contracts
- Makes it easier to see what methods are available without implementation noise
- Still preserves all the information needed to understand how to use the APIs
- Maintains correct conditional compilation behavior for the target platform

The goal is to create clean reference documentation that shows exactly what public methods and constants are available (for our specific platform configuration), with their full documentation, but without the implementation details cluttering up the project knowledge space.