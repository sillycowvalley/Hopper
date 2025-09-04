# Master BASIC Comparison Table (1977-1982)

## Overview Comparison

| Feature | TRS-80 Level I | TRS-80 Level II | Applesoft I/II | Commodore 2.0 | BBC BASIC |
|---------|---------------|----------------|----------------|---------------|-----------|
| **Author(s)** | Steve Leininger | Microsoft | Microsoft (Marc McDonald, Ric Weiland) + Apple (Randy Wigginton) | Microsoft | Sophie Wilson (Acorn) |
| **Release Date** | August 1977 | 1978 | 1977/1978 | 1977 (PET), 1982 (C64) | December 1981 |
| **ROM Size** | 4K | 8K | 10K | 8K | 16K |
| **CPUs Supported** | Z80 | Z80 | 6502 | 6502 | 6502, later ARM |
| **Addressable RAM** | 4K-48K | 4K-48K | 4K-64K | 4K-64K | 32K+ |

## Data Types & Numeric Support

| Feature | TRS-80 Level I | TRS-80 Level II | Applesoft I/II | Commodore 2.0 | BBC BASIC |
|---------|---------------|----------------|----------------|---------------|-----------|
| **Floating Point** | Single precision only | Double precision | Yes (5-byte format) | Yes (5-byte format) | 5-byte, later IEEE 8-byte |
| **Integer Support** | ❌ Floating only | 16-bit integers | 16-bit integers | 16-bit integers | 32-bit integers |
| **String Support** | Basic | Full string functions | Full with concatenation | Full with concatenation | Advanced string handling |
| **Array Support** | 1D arrays | Multi-dimensional | Multi-dimensional | Multi-dimensional | Multi-dimensional |
| **Variable Names** | Full names | Full names | **First 2 chars only** | Full names | Full names |

## Language Features

| Feature | TRS-80 Level I | TRS-80 Level II | Applesoft I/II | Commodore 2.0 | BBC BASIC |
|---------|---------------|----------------|----------------|---------------|-----------|
| **Structured Programming** | ❌ | ❌ | ❌ | ❌ | ✅ Full support |
| **Procedures/Functions** | ❌ | ❌ | ❌ | ❌ | ✅ PROC/FN with local vars |
| **Loop Types** | FOR/NEXT, GOTO | FOR/NEXT, GOTO | FOR/NEXT, GOTO | FOR/NEXT, GOTO | FOR/NEXT, REPEAT/UNTIL, WHILE/WEND |
| **IF Statements** | Single line IF/THEN | Single line IF/THEN | Single line IF/THEN | Single line IF/THEN | **Multi-line IF/ELSE/ENDIF** |
| **Inline Assembly** | ❌ | ❌ | ❌ | ❌ | ✅ **[assembly code]** |
| **Error Handling** | Basic | Basic | Basic | Basic | Advanced with ON ERROR |

## Hardware & System Integration

| Feature | TRS-80 Level I | TRS-80 Level II | Applesoft I/II | Commodore 2.0 | BBC BASIC |
|---------|---------------|----------------|----------------|---------------|-----------|
| **Graphics Commands** | ❌ | Limited text graphics | Apple II hi-res support | ❌ **POKE only** | Full graphics commands |
| **Sound Commands** | ❌ | ❌ | Bell only | ❌ **POKE only** | Full sound/music support |
| **Hardware Access** | Limited | PEEK/POKE | PEEK/POKE + slots (PR#/IN#) | **POKE/PEEK only** | System calls + inline ASM |
| **Memory Management** | Basic | Basic | Basic | Basic | Advanced (HIMEM, PAGE, etc.) |

## File System & Storage

| Feature | TRS-80 Level I | TRS-80 Level II | Applesoft I/II | Commodore 2.0 | BBC BASIC |
|---------|---------------|----------------|----------------|---------------|-----------|
| **File System** | Cassette only | **Named files** on cassette/disk | ❌ **No file commands** | Poor disk support | Full file system |
| **Directory Support** | ❌ | Yes | ❌ | **Destroys program** | Yes |
| **Data Management** | Basic | **Encoded numeric data** | ❌ | Limited | Advanced |
| **Storage Types** | Cassette | Cassette + 5.25" floppy | Cassette + disk (via DOS) | Cassette + 5.25" floppy | Cassette + disk + network |

## Unique Features & Innovations

| System | Unique Strengths | Major Limitations |
|---------|-----------------|-------------------|
| **TRS-80 Level I** | • Cheapest entry point<br>• Widespread availability | • Floating-point only<br>• Very limited functionality |
| **TRS-80 Level II** | • Named files on cassette<br>• Sophisticated data encoding<br>• Wide software library | • No structured programming<br>• Limited graphics |
| **Applesoft** | • Apple II graphics integration<br>• Slot-based I/O system | • **2-character variable names**<br>• No disk commands<br>• Reserved word conflicts |
| **Commodore 2.0** | • Microsoft compatibility<br>• Stable interpreter | • **No graphics/sound commands**<br>• Poor disk integration<br>• Directory destroys program |
| **BBC BASIC** | • **First structured BASIC**<br>• Inline assembly<br>• Procedures/functions<br>• Educational focus | • Limited to BBC hardware initially<br>• Higher memory requirements |

## Performance & Development Notes

| Feature | TRS-80 Level I | TRS-80 Level II | Applesoft I/II | Commodore 2.0 | BBC BASIC |
|---------|---------------|----------------|----------------|---------------|-----------|
| **Execution Speed** | Slow | Moderate | Moderate | Slow | **Fast** ("fastest interpreted BASIC") |
| **Memory Efficiency** | Very compact | Compact | Moderate | Compact | Efficient |
| **Development Tools** | Minimal | Basic debugger | Minimal | Minimal | **Advanced** (libraries, etc.) |
| **Educational Suitability** | Poor | Basic | Poor | Poor | **Excellent** |

## Market Impact Summary

| System | Market Position | Legacy |
|---------|----------------|---------|
| **TRS-80** | Dominated early market (1977-1982) | Proved demand for home computers |
| **Applesoft** | Strong in education and home market | Foundation for Apple II ecosystem |
| **Commodore 2.0** | Most units sold (C64) but limited BASIC | Gaming success despite BASIC limitations |
| **BBC BASIC** | UK education standard, 80% of schools | Influenced modern BASIC design |

---
*Note: This table represents the state of each BASIC as originally released. Many received updates and extensions over time.*