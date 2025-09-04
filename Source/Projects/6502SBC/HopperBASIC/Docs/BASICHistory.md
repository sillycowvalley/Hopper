# Master BASIC Comparison Table (1977-1982)

## Overview Comparison

| Feature | TRS-80 Level I | TRS-80 Level II | Applesoft I/II | Commodore 2.0 | BBC BASIC | HopperBASIC |
|---------|---------------|----------------|----------------|---------------|-----------|-------------|
| **Author(s)** | Steve Leininger | Microsoft | Microsoft (Marc McDonald, Ric Weiland) + Apple (Randy Wigginton) | Microsoft | Sophie Wilson (Acorn) | Hopper Team |
| **Release Date** | August 1977 | 1978 | 1977/1978 | 1977 (PET), 1982 (C64) | December 1981 | 2025 |
| **ROM Size** | 4K | 8K | 10K | 8K | 16K | 32K target |
| **CPUs Supported** | Z80 | Z80 | 6502 | 6502 | 6502, later ARM | 6502 |
| **Addressable RAM** | 4K-48K | 4K-48K | 4K-64K | 4K-64K | 32K+ | 27K+ available |

## Data Types & Numeric Support

| Feature | TRS-80 Level I | TRS-80 Level II | Applesoft I/II | Commodore 2.0 | BBC BASIC | HopperBASIC |
|---------|---------------|----------------|----------------|---------------|-----------|-------------|
| **Floating Point** | Single precision only | Double precision | Yes (5-byte format) | Yes (5-byte format) | 5-byte, later IEEE 8-byte | ❌ None |
| **Integer Support** | ❌ Floating only | 16-bit integers | 16-bit integers | 16-bit integers | 32-bit integers | 32-bit LONG only |
| **String Support** | Basic | Full string functions | Full with concatenation | Full with concatenation | Advanced string handling | Modern with escape sequences |
| **Array Support** | 1D arrays | Multi-dimensional | Multi-dimensional | Multi-dimensional | Multi-dimensional | Typed arrays (BIT, BYTE, CHAR, INT, WORD) |
| **Variable Names** | Full names | Full names | **First 2 chars only** | Full names | Full names | Full CamelCase names |

## Language Features

| Feature | TRS-80 Level I | TRS-80 Level II | Applesoft I/II | Commodore 2.0 | BBC BASIC | HopperBASIC |
|---------|---------------|----------------|----------------|---------------|-----------|-------------|
| **Structured Programming** | ❌ | ❌ | ❌ | ❌ | ✅ Full support | ✅ Full support |
| **Procedures/Functions** | ❌ | ❌ | ❌ | ❌ | ✅ PROC/FN with local vars | ✅ FUNC with parameters |
| **Loop Types** | FOR/NEXT, GOTO | FOR/NEXT, GOTO | FOR/NEXT, GOTO | FOR/NEXT, GOTO | FOR/NEXT, REPEAT/UNTIL, WHILE/WEND | FOR/NEXT, WHILE/WEND, DO/UNTIL |
| **IF Statements** | Single line IF/THEN | Single line IF/THEN | Single line IF/THEN | Single line IF/THEN | **Multi-line IF/ELSE/ENDIF** | **Multi-line IF/ELSE/ENDIF** |
| **Inline Assembly** | ❌ | ❌ | ❌ | ❌ | ✅ **[assembly code]** | ✅ **Built-in opcodes** |
| **Error Handling** | Basic | Basic | Basic | Basic | Advanced with ON ERROR | Advanced with BRK handling |

## Hardware & System Integration

| Feature | TRS-80 Level I | TRS-80 Level II | Applesoft I/II | Commodore 2.0 | BBC BASIC | HopperBASIC |
|---------|---------------|----------------|----------------|---------------|-----------|-------------|
| **Graphics Commands** | ❌ | Limited text graphics | Apple II hi-res support | ❌ **POKE only** | Full graphics commands | Modern I/O functions |
| **Sound Commands** | ❌ | ❌ | Bell only | ❌ **POKE only** | Full sound/music support | Hardware I/O support |
| **Hardware Access** | Limited | PEEK/POKE | PEEK/POKE + slots (PR#/IN#) | **POKE/PEEK only** | System calls + inline ASM | GPIO, I2C native support |
| **Memory Management** | Basic | Basic | Basic | Basic | Advanced (HIMEM, PAGE, etc.) | Sophisticated heap/stack |

## File System & Storage

| Feature | TRS-80 Level I | TRS-80 Level II | Applesoft I/II | Commodore 2.0 | BBC BASIC | HopperBASIC |
|---------|---------------|----------------|----------------|---------------|-----------|-------------|
| **File System** | Cassette only | **Named files** on cassette/disk | ❌ **No file commands** | Poor disk support | Full file system | EEPROM with named files |
| **Directory Support** | ❌ | Yes | ❌ | **Destroys program** | Yes | Yes |
| **Data Management** | Basic | **Encoded numeric data** | ❌ | Limited | Advanced | IMPORT/EXPORT arrays |
| **Storage Types** | Cassette | Cassette + 5.25" floppy | Cassette + disk (via DOS) | Cassette + 5.25" floppy | Cassette + disk + network | EEPROM (64K) |

## Unique Features & Innovations

| System | Unique Strengths | Major Limitations |
|---------|-----------------|-------------------|
| **TRS-80 Level I** | • Cheapest entry point<br>• Widespread availability | • Floating-point only<br>• Very limited functionality |
| **TRS-80 Level II** | • Named files on cassette<br>• Sophisticated data encoding<br>• Wide software library | • No structured programming<br>• Limited graphics |
| **Applesoft** | • Apple II graphics integration<br>• Slot-based I/O system | • **2-character variable names**<br>• No disk commands<br>• Reserved word conflicts |
| **Commodore 2.0** | • Microsoft compatibility<br>• Stable interpreter | • **No graphics/sound commands**<br>• Poor disk integration<br>• Directory destroys program |
| **BBC BASIC** | • **First structured BASIC**<br>• Inline assembly<br>• Procedures/functions<br>• Educational focus | • Limited to BBC hardware initially<br>• Higher memory requirements |
| **HopperBASIC** | • Modern language design<br>• Type safety<br>• Native I2C/GPIO<br>• JIT compilation<br>• Interactive REPL | • No floating-point<br>• 6502-specific<br>• Modern design (2025) |

## Performance & Development Notes

| Feature | TRS-80 Level I | TRS-80 Level II | Applesoft I/II | Commodore 2.0 | BBC BASIC | HopperBASIC |
|---------|---------------|----------------|----------------|---------------|-----------|-------------|
| **Execution Speed** | Slow | Moderate | Moderate | Slow | **Fast** ("fastest interpreted BASIC") | **Fast** (JIT compilation) |
| **Memory Efficiency** | Very compact | Compact | Moderate | Compact | Efficient | Very efficient |
| **Development Tools** | Minimal | Basic debugger | Minimal | Minimal | **Advanced** (libraries, etc.) | **Modern** (REPL, auto-complete) |
| **Educational Suitability** | Poor | Basic | Poor | Poor | **Excellent** | **Excellent** |

## Market Impact Summary

| System | Market Position | Legacy |
|---------|----------------|---------|
| **TRS-80** | Dominated early market (1977-1982) | Proved demand for home computers |
| **Applesoft** | Strong in education and home market | Foundation for Apple II ecosystem |
| **Commodore 2.0** | Most units sold (C64) but limited BASIC | Gaming success despite BASIC limitations |
| **BBC BASIC** | UK education standard, 80% of schools | Influenced modern BASIC design |

---
*Note: This table represents the state of each BASIC as originally released. Many received updates and extensions over time.*