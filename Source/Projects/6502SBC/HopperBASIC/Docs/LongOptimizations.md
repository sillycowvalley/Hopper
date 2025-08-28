# Long Operations Optimization Plan
## HopperBASIC 32-bit Long Optimizations

### Overview
This document outlines optimizations for the Long unit that will reduce code size by ~200+ bytes while improving performance for common operations.

---

## 1. Long.Print() Optimization

### Current Implementation
- **Code size**: ~150 bytes
- **Lookup table**: 40 bytes (PrDec32Tens at 0x855B)
- **Method**: Repeated subtraction for each power of 10
- **Total**: ~190 bytes

### Proposed Implementation
Eliminate the lookup table entirely by using division and modulo operations.

```assembly
Long.Print()
{
    PHA
    PHX
    
    // Handle negative numbers
    BIT ZP.TOPT
    if (MI)  // Signed type
    {
        BIT ZP.TOP3
        if (MI)  // Actually negative
        {
            LDA #'-'
            Serial.WriteChar();
            
            // Negate in place
            SEC
            LDA #0
            SBC ZP.TOP0
            STA ZP.TOP0
            LDA #0
            SBC ZP.TOP1
            STA ZP.TOP1
            LDA #0
            SBC ZP.TOP2
            STA ZP.TOP2
            LDA #0
            SBC ZP.TOP3
            STA ZP.TOP3
        }
    }
    
    // Count digits and push them to stack
    LDX #0  // Digit counter
    loop
    {
        // Save TOP
        LDA ZP.TOP0
        PHA
        LDA ZP.TOP1
        PHA
        LDA ZP.TOP2
        PHA
        LDA ZP.TOP3
        PHA
        
        // Set up for MOD 10
        LDA #10
        STA ZP.ACC0
        STZ ZP.ACC1
        STZ ZP.ACC2
        STZ ZP.ACC3
        
        // Get remainder (last digit)
        JSR Long.ModInternal  // Result in ACC, quotient in TOP
        
        // Convert digit to ASCII and save
        LDA ZP.ACC0
        ORA #'0'
        PHA  // Push ASCII digit
        INX  // Count digit
        
        // Restore TOP (now has quotient)
        PLA
        STA ZP.TOP3
        PLA
        STA ZP.TOP2
        PLA
        STA ZP.TOP1
        PLA
        STA ZP.TOP0
        
        // Divide by 10 (reuse the optimized ÷10)
        JSR utilityDiv10
        
        // Check if quotient is zero
        LDA ZP.TOP0
        ORA ZP.TOP1
        ORA ZP.TOP2
        ORA ZP.TOP3
        if (Z) { break; }
    }
    
    // Special case: if no digits pushed, number was 0
    CPX #0
    if (Z)
    {
        LDA #'0'
        Serial.WriteChar();
    }
    else
    {
        // Pop and print digits in reverse order
        loop
        {
            PLA
            Serial.WriteChar();
            DEX
            if (Z) { break; }
        }
    }
    
    PLX
    PLA
}
```

### Benefits
- **Saves**: ~100 bytes (eliminates 40-byte table, simpler code)
- **Reuses**: Existing Long.Mod and utilityDiv10 routines
- **Stack usage**: Maximum 10 bytes for digits (4,294,967,295 has 10 digits)

---

## 2. Long.Mul() Optimization

### Key Strategy
Maximize code reuse with a single `shiftNEXTleft()` routine called multiple times.

### Implementation

```assembly
utilityLongMUL()
{
    // Check if both operands are 8-bit - use fast 8x8
    LDA ZP.TOP1
    ORA ZP.TOP2
    ORA ZP.TOP3
    if (Z)  // TOP is 8-bit
    {
        LDA ZP.NEXT1
        ORA ZP.NEXT2
        ORA ZP.NEXT3
        if (Z)  // NEXT is also 8-bit
        {
            JMP fast8x8multiply  // Existing optimized routine
        }
    }
    
    // Check if NEXT has a special multiplier - swap if so
    LDA ZP.NEXT1
    ORA ZP.NEXT2
    ORA ZP.NEXT3
    if (Z)  // NEXT is 8-bit
    {
        LDA ZP.NEXT0
        CMP #1
        if (Z) { swapNEXTandTOP(); }
        CMP #2
        if (Z) { swapNEXTandTOP(); }
        CMP #4
        if (Z) { swapNEXTandTOP(); }
        CMP #8
        if (Z) { swapNEXTandTOP(); }
        CMP #10
        if (Z) { swapNEXTandTOP(); }
        CMP #16
        if (Z) { swapNEXTandTOP(); }
    }
    
    // Check for zero (only once, after potential swap)
    LDA ZP.TOP0
    ORA ZP.TOP1
    ORA ZP.TOP2
    ORA ZP.TOP3
    if (Z)
    {
        // Clear result (8 bytes)
        STZ ZP.RESULT0
        STZ ZP.RESULT1
        STZ ZP.RESULT2
        STZ ZP.RESULT3
        STZ ZP.RESULT4
        STZ ZP.RESULT5
        STZ ZP.RESULT6
        STZ ZP.RESULT7
        return;
    }
    
    LDA ZP.NEXT0
    ORA ZP.NEXT1
    ORA ZP.NEXT2
    ORA ZP.NEXT3
    if (Z)
    {
        // Clear result (8 bytes)
        STZ ZP.RESULT0
        STZ ZP.RESULT1
        STZ ZP.RESULT2
        STZ ZP.RESULT3
        STZ ZP.RESULT4
        STZ ZP.RESULT5
        STZ ZP.RESULT6
        STZ ZP.RESULT7
        return;
    }
    
    // Check if TOP has special multiplier
    LDA ZP.TOP1
    ORA ZP.TOP2
    ORA ZP.TOP3
    if (Z)  // TOP is 8-bit
    {
        LDA ZP.TOP0
        
        CMP #1
        if (Z) { copyNEXTtoRESULT(); return; }
        
        CMP #2
        if (Z) 
        { 
            shiftNEXTleft();
            copyNEXTtoRESULT();
            return;
        }
        
        CMP #4
        if (Z)
        {
            shiftNEXTleft();
            shiftNEXTleft();
            copyNEXTtoRESULT();
            return;
        }
        
        CMP #8
        if (Z)
        {
            shiftNEXTleft();
            shiftNEXTleft();
            shiftNEXTleft();
            copyNEXTtoRESULT();
            return;
        }
        
        CMP #10
        if (Z)
        {
            // x10 = x8 + x2
            // Save original NEXT (4 bytes)
            LDA ZP.NEXT0
            PHA
            LDA ZP.NEXT1
            PHA
            LDA ZP.NEXT2
            PHA
            LDA ZP.NEXT3
            PHA
            
            // Calculate x8
            shiftNEXTleft();
            shiftNEXTleft();
            shiftNEXTleft();
            
            // Save x8 to RESULT
            copyNEXTtoRESULT();
            
            // Save overflow
            LDA ZP.RESULT4
            PHA
            LDA ZP.RESULT5
            PHA
            
            // Restore original NEXT
            PLA
            STA ZP.NEXT3
            PLA
            STA ZP.NEXT2
            PLA
            STA ZP.NEXT1
            PLA
            STA ZP.NEXT0
            
            // Calculate x2
            shiftNEXTleft();
            
            // Add x2 to x8 (in RESULT)
            CLC
            LDA ZP.RESULT0
            ADC ZP.NEXT0
            STA ZP.RESULT0
            LDA ZP.RESULT1
            ADC ZP.NEXT1
            STA ZP.RESULT1
            LDA ZP.RESULT2
            ADC ZP.NEXT2
            STA ZP.RESULT2
            LDA ZP.RESULT3
            ADC ZP.NEXT3
            STA ZP.RESULT3
            
            // Handle overflow
            PLA
            STA ZP.RESULT5
            PLA
            ADC ZP.RESULT4
            STA ZP.RESULT4
            LDA #0
            ADC ZP.RESULT5
            STA ZP.RESULT5
            
            // Check for 32-bit overflow
            LDA ZP.RESULT4
            ORA ZP.RESULT5
            ORA ZP.RESULT6
            ORA ZP.RESULT7
            if (NZ)
            {
                Error.NumericOverflow(); 
                BIT ZP.EmulatorPCL
            }
            return;
        }
        
        CMP #16
        if (Z)
        {
            shiftNEXTleft();
            shiftNEXTleft();
            shiftNEXTleft();
            shiftNEXTleft();
            copyNEXTtoRESULT();
            return;
        }
    }
    
    // Fall through to general multiplication
    // [existing general multiplication code]
}

// Core routines - maximum reuse
shiftNEXTleft()  // 11 bytes
{
    ASL ZP.NEXT0
    ROL ZP.NEXT1
    ROL ZP.NEXT2
    ROL ZP.NEXT3
    ROL ZP.RESULT4
    ROL ZP.RESULT5
    ROL ZP.RESULT6
    ROL ZP.RESULT7
    
    if (C)
    {
        Error.NumericOverflow(); 
        BIT ZP.EmulatorPCL
    }
}

copyNEXTtoRESULT()  // 16 bytes
{
    LDA ZP.NEXT0
    STA ZP.RESULT0
    LDA ZP.NEXT1
    STA ZP.RESULT1
    LDA ZP.NEXT2
    STA ZP.RESULT2
    LDA ZP.NEXT3
    STA ZP.RESULT3
}

swapNEXTandTOP()  // 24 bytes
{
    LDA ZP.NEXT0
    LDX ZP.TOP0
    STA ZP.TOP0
    STX ZP.NEXT0
    
    LDA ZP.NEXT1
    LDX ZP.TOP1
    STA ZP.TOP1
    STX ZP.NEXT1
    
    LDA ZP.NEXT2
    LDX ZP.TOP2
    STA ZP.TOP2
    STX ZP.NEXT2
    
    LDA ZP.NEXT3
    LDX ZP.TOP3
    STA ZP.TOP3
    STX ZP.NEXT3
}
```

### Special Multiplier Performance
- **x1**: Direct copy
- **x2**: 1 shift
- **x4**: 2 shifts  
- **x8**: 3 shifts
- **x10**: 3 shifts + save/restore + 1 shift + addition
- **x16**: 4 shifts

---

## 3. Long.Div() Optimization

### Special Cases for Common Divisors

```assembly
Long.Div()
{
    utilityDoLongSigns();
    
    // Check for special divisors
    LDA ZP.TOP1
    ORA ZP.TOP2
    ORA ZP.TOP3
    if (Z)  // Divisor is 8-bit
    {
        LDA ZP.TOP0
        
        CMP #1
        if (Z) { goto finishDiv; }  // ÷1
        
        CMP #2
        if (Z)
        {
            shiftNEXTright();
            goto finishDiv;
        }
        
        CMP #4
        if (Z)
        {
            shiftNEXTright();
            shiftNEXTright();
            goto finishDiv;
        }
        
        CMP #8
        if (Z)
        {
            shiftNEXTright();
            shiftNEXTright();
            shiftNEXTright();
            goto finishDiv;
        }
        
        CMP #16
        if (Z)
        {
            shiftNEXTright();
            shiftNEXTright();
            shiftNEXTright();
            shiftNEXTright();
            goto finishDiv;
        }
        
        CMP #10
        if (Z)
        {
            utilityDiv10();
            goto finishDiv;
        }
        
        CMP #100
        if (Z)
        {
            utilityDiv10();
            utilityDiv10();
            goto finishDiv;
        }
        
        CMP #5
        if (Z)
        {
            // ÷5 = ÷10 * 2
            utilityDiv10();
            shiftNEXTleft();
            goto finishDiv;
        }
        
        CMP #20
        if (Z)
        {
            // ÷20 = ÷10 ÷2
            utilityDiv10();
            shiftNEXTright();
            goto finishDiv;
        }
        
        CMP #25
        if (Z)
        {
            // ÷25 = ÷100 * 4
            utilityDiv10();
            utilityDiv10();
            shiftNEXTleft();
            shiftNEXTleft();
            goto finishDiv;
        }
        
        CMP #50
        if (Z)
        {
            // ÷50 = ÷100 * 2
            utilityDiv10();
            utilityDiv10();
            shiftNEXTleft();
            goto finishDiv;
        }
    }
    
    // General division
    LDX #0
    DivMod();
    
finishDiv:
    // Handle sign
    LDA ZP.FSIGN
    CMP #1
    if (Z)
    {
        negateLongNEXT();
    }
    Long.PushNext();
}

shiftNEXTright()  // 8 bytes
{
    LSR ZP.NEXT3
    ROR ZP.NEXT2
    ROR ZP.NEXT1
    ROR ZP.NEXT0
}
```

### Division Optimizations
- **÷1**: No operation
- **÷2, ÷4, ÷8, ÷16**: Simple shifts
- **÷5**: ÷10 then ×2
- **÷10**: Existing optimized routine
- **÷20**: ÷10 then ÷2
- **÷25**: ÷100 then ×4
- **÷50**: ÷100 then ×2
- **÷100**: Two ÷10 operations

---

## 4. Tokenizer.GetTokenNumber() Optimization

### Current Implementation
- Uses custom `MultiplyBy10()` and `AddDigit()` methods
- Duplicates functionality already in Long unit

### Proposed Implementation
Use standard Long operations instead of custom methods:

```assembly
GetTokenNumber()
{
    Long.ZeroTop();
    
    // Setup IDX pointer to number string
    CLC
    LDA ZP.TokenBufferL
    ADC ZP.TokenLiteralPosL
    STA ZP.IDXL
    LDA ZP.TokenBufferH
    ADC ZP.TokenLiteralPosH
    STA ZP.IDXH
    
    LDY #0  // Index into number string
    
    // Check for hex prefix (0x or 0X)
    LDA [ZP.IDX], Y
    CMP #'0'
    if (Z)
    {
        INY
        LDA [ZP.IDX], Y
        CMP #'x'
        if (Z) { parseHexNumber(); return; }
        CMP #'X'
        if (Z) { parseHexNumber(); return; }
        DEY  // Back up
    }
    
    // Parse decimal number
    loop
    {
        LDA [ZP.IDX], Y
        if (Z) { break; }  // Hit null terminator
        
        // Check if character is a digit
        Char.IsDigit();
        if (NC) { break; }  // Not a digit
        
        // Save digit
        LDA [ZP.IDX], Y
        SEC
        SBC #'0'
        PHA
        
        // Multiply current value by 10
        // Push current TOP
        Long.PushTop();
        
        // Set TOP to 10
        LDA #10
        STA ZP.TOP0
        STZ ZP.TOP1
        STZ ZP.TOP2
        STZ ZP.TOP3
        Long.PushTop();
        
        // Multiply (using optimized x10)
        Long.Mul();
        Long.PopTop();
        
        // Check for overflow (high 32 bits should be zero)
        // This is handled by the optimized Mul
        
        // Add the digit
        PLA  // Get digit back
        CLC
        ADC ZP.TOP0
        STA ZP.TOP0
        LDA #0
        ADC ZP.TOP1
        STA ZP.TOP1
        LDA #0
        ADC ZP.TOP2
        STA ZP.TOP2
        LDA #0
        ADC ZP.TOP3
        STA ZP.TOP3
        if (C)  // Overflow
        {
            Error.NumericOverflow();
            BIT ZP.EmulatorPCL
            return;
        }
        
        INY
    }
    
    // Determine type based on value
    LDA ZP.TOP3
    ORA ZP.TOP2
    if (NZ)
    {
        LDA #BASICType.LONG
        STA ZP.TOPT
    }
    else
    {
        LDA ZP.TOP1
        if (NZ)
        {
            BIT ZP.TOP1
            if (MI)
            {
                LDA #BASICType.WORD  // 32768-65535
                STA ZP.TOPT
            }
            else
            {
                LDA #BASICType.INT   // 256-32767
                STA ZP.TOPT
            }
        }
        else
        {
            LDA #BASICType.BYTE  // 0-255
            STA ZP.TOPT
        }
    }
}
```

### Benefits of Standard Long Operations
- Eliminates `MultiplyBy10()` and `AddDigit()` custom methods
- Reuses optimized multiplication (which includes x10 optimization)
- More maintainable - single point of optimization
- Automatic overflow checking from optimized Mul

---

## Summary

### Total Code Size Savings

| Component | Current | Proposed | Savings |
|-----------|---------|----------|---------|
| Long.Print() | ~190 bytes | ~90 bytes | ~100 bytes |
| Long.Mul() optimizations | 0 bytes | ~50 bytes | -50 bytes (new) |
| Long.Div() optimizations | partial | ~40 bytes | -20 bytes (new) |
| Tokenizer (remove custom) | ~60 bytes | 0 bytes | ~60 bytes |
| **Total** | | | **~90 bytes saved** |

### Performance Improvements

| Operation | Improvement |
|-----------|------------|
| Multiply by 2, 4, 8, 16 | ~10x faster (shifts vs loops) |
| Multiply by 10 | ~5x faster (optimized) |
| Divide by 2, 4, 8, 16 | ~10x faster (shifts vs loops) |
| Divide by 5, 10, 20, 25, 50, 100 | ~3x faster (optimized) |
| Print numbers | Similar speed, less memory |
| Parse numbers | Slightly faster, cleaner code |

### Key Design Principles

1. **Maximum code reuse** - Single `shiftNEXTleft()` routine used everywhere
2. **Handle symmetry** - Swap operands to normalize special cases
3. **Common cases first** - 8x8 multiply, powers of 2, base-10 operations
4. **Eliminate redundancy** - Remove custom tokenizer methods
5. **Stack over tables** - Use stack for temporary storage vs lookup tables

### Implementation Priority

1. **First**: Implement Long.Mul() optimizations (biggest performance win)
2. **Second**: Implement Long.Print() (biggest space saving)
3. **Third**: Update Tokenizer to use standard Long operations
4. **Fourth**: Add Long.Div() optimizations

This plan provides significant space savings while improving performance for the most common operations in BASIC programs.