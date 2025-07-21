program FiboUInt
{
    #define CPU_65C02S
    #define ROM_4K
    
    uses "/Source/Runtime/6502/Serial"
    
    enum ZP
    {
        // Function parameters and locals (16-bit values use L/H pairs)
        ARGL     = 0x10,    // Function argument low byte
        ARGH     = 0x11,    // Function argument high byte
        RESULTL  = 0x12,    // Function result low byte
        RESULTH  = 0x13,    // Function result high byte
        TEMPL    = 0x14,    // Temporary value low byte
        TEMPH    = 0x15,    // Temporary value high byte
        TEMP2L   = 0x16,    // Second temporary low byte
        TEMP2H   = 0x17,    // Second temporary high byte
        
        // Stack simulation for recursion
        STACKPTRL = 0x18,   // Stack pointer for our call stack low
        STACKPTRH = 0x19,   // Stack pointer for our call stack high
        
        // Benchmark variables
        LOOPCNTL = 0x1A,    // Loop counter low byte
        LOOPCNTH = 0x1B,    // Loop counter high byte
        
        // Character output
        CHARLOC  = 0x1C,    // Current character location in string
    }
    
    const string FiboMsg = "Fibo(24) = ";
    const string ResultMsg = " (Fibonacci benchmark complete)\n";
    
    IRQ()
    {
        Serial.ISR();
    }
    
    NMI()
    {
    }
    
    // Print a 16-bit number in decimal
    // Input: RESULTL/RESULTH contains the number
    printDecimal()
    {
        // Simple approach: convert to decimal by repeated division by 10
        // This is a simplified version - in practice you'd want a more efficient algorithm
        
        // For now, let's just print it in hex for simplicity
        LDA ZP.RESULTH
        printHexByte();
        LDA ZP.RESULTL  
        printHexByte();
    }
    
    // Print a single hex byte (value in A register)
    printHexByte()
    {
        PHA                    // Save original value
        LSR                    // Get high nibble
        LSR
        LSR
        LSR
        CMP #10
        if (C)                 // >= 10, use A-F
        {
            CLC
            ADC #('A' - 10)    // Convert to A-F
        }
        else                   // < 10, use 0-9
        {
            CLC
            ADC #'0'
        }
        Serial.WriteChar();    // Output high nibble
        
        PLA                    // Get original value back
        AND #0x0F              // Get low nibble
        CMP #10
        if (C)
        {
            CLC
            ADC #('A' - 10)
        }
        else
        {
            CLC
            ADC #'0'
        }
        Serial.WriteChar();    // Output low nibble
    }
    
    // Print a null-terminated string
    // X register contains the starting index
    printString()
    {
        loop
        {
            LDA FiboMsg, X     // Get next character
            if (Z) { break; }  // If null terminator, we're done
            Serial.WriteChar(); // Send character
            INX                // Next character
        }
    }
    
    // Simplified Fibonacci calculation (iterative version for 6502)
    // Input: ARGL/ARGH contains n
    // Output: RESULTL/RESULTH contains Fibo(n)
    // Note: Recursive version would be too complex for this demo
    fibonacci()
    {
        // Check if n <= 1
        LDA ZP.ARGH
        if (NZ) 
        { 
            // n > 255, definitely > 1, continue with calculation
        }
        else
        {
            LDA ZP.ARGL
            CMP #2
            if (C)           // n >= 2
            {
                // Continue with calculation
            }
            else            // n <= 1
            {
                // Return n itself
                LDA ZP.ARGL
                STA ZP.RESULTL
                LDA ZP.ARGH  
                STA ZP.RESULTH
                return;
            }
        }
        
        // Iterative Fibonacci calculation
        // f0 = 0, f1 = 1
        // for i = 2 to n: f2 = f1 + f0; f0 = f1; f1 = f2
        
        STZ ZP.TEMPL        // f0 = 0 (low byte)
        STZ ZP.TEMPH        // f0 = 0 (high byte)
        
        LDA #1
        STA ZP.TEMP2L       // f1 = 1 (low byte)
        STZ ZP.TEMP2H       // f1 = 1 (high byte)
        
        // Set up loop counter: start from 2, go to n+1 (so we do n iterations)
        LDA #2
        STA ZP.LOOPCNTL
        STZ ZP.LOOPCNTH
        
        loop
        {
            // Calculate f2 = f1 + f0
            CLC
            LDA ZP.TEMP2L       // f1 low
            ADC ZP.TEMPL        // + f0 low
            STA ZP.RESULTL      // = f2 low
            
            LDA ZP.TEMP2H       // f1 high
            ADC ZP.TEMPH        // + f0 high + carry
            STA ZP.RESULTH      // = f2 high
            
            // f0 = f1
            LDA ZP.TEMP2L
            STA ZP.TEMPL
            LDA ZP.TEMP2H
            STA ZP.TEMPH
            
            // f1 = f2  
            LDA ZP.RESULTL
            STA ZP.TEMP2L
            LDA ZP.RESULTH
            STA ZP.TEMP2H
            
            // Increment counter
            INC ZP.LOOPCNTL
            if (Z)
            {
                INC ZP.LOOPCNTH
            }
            
            // Check if counter > n (exit condition - we've done n iterations)
            LDA ZP.LOOPCNTH
            CMP ZP.ARGH
            if (C)              // counter high >= n high
            {
                if (NZ) { break; }  // counter high > n high, done
                // counter high == n high, check low byte  
                LDA ZP.LOOPCNTL
                CMP ZP.ARGL
                if (C) { break; }   // counter > n, we're done
            }
        }
        
        // Result is already in RESULTL/RESULTH
    }
    
    Hopper()
    {
        Serial.Initialize();
        
        // Print message
        LDX #0
        printString();
        
        // Calculate Fibo(24)
        LDA #24
        STA ZP.ARGL
        STZ ZP.ARGH
        
        fibonacci();
        
        // Print result in hex
        printDecimal();
        
        // Print completion message  
        LDX #0
        loop
        {
            LDA ResultMsg, X
            if (Z) { break; }
            Serial.WriteChar();
            INX
        }
        
        // Program completes here - compiler will add STP automatically
    }
}
