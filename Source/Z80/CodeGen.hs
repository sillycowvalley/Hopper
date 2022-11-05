unit CodeGen
{
    uses "/Source/System/System"
    uses "/Source/System/Screen"
    uses "/Source/System/Diagnostics"
    uses "/Source/Compiler/Tokens/Token"
    uses "/Source/Compiler/Tokens/Scanner"
    uses "/Source/Z80/Chunks"
    uses "/Source/Z80/Symbols"
    
    const bool newBoolCodeGen = false;


    // Optimal add A to HL (no DE needed)
    // https://wikiti.brandonw.net/index.php?title=Z80_Routines:Optimized:addAtoHL
    //
    // Multiply, the long story;
    // http://cpctech.cpc-live.com/docs/mult.html
    //
    // itoa:
    // https://wikiti.brandonw.net/index.php?title=Z80_Routines:Other:itoa
        
    PushNotHL(<string,string> sourceToken)
    {
        // used by   !(...) and ==   (boolean result left on stack)
        
        
        // if HL == 0, we push 1
        // if HL != 0, we push 0
        
        // is HL zero? -> Z
        WriteChunk(OpCode.LDAH, sourceToken);    
        WriteChunk(OpCode.ORAL, sourceToken);
        WriteChunk(OpCode.JRZ, sourceToken);     // JPZ inc
        WriteChunk(+5, sourceToken);
        WriteChunk(OpCode.LDHL, sourceToken);
        WriteChunk(0, sourceToken);
        WriteChunk(0, sourceToken);
        WriteChunk(OpCode.JR, sourceToken);      // JP end
        WriteChunk(+1, sourceToken);
// inc:
        WriteChunk(OpCode.INCL, sourceToken);  // HL = HL + 1
// end:        
        WriteChunk(OpCode.PUSHHL, sourceToken);
    }
    

    WritePort(<string,string> sourceToken)
    {   
        // [top]  -> value
        // [next] -> index
        WriteChunk(OpCode.POPDE, sourceToken);  // value in DE
        WriteChunk(OpCode.POPHL, sourceToken); // index in L
        WriteChunk(OpCode.LDAC, sourceToken);
        WriteChunk(OpCode.LDCL, sourceToken);
        WriteChunk(OpCode.OUTCE, sourceToken);
        WriteChunk(OpCode.LDCA, sourceToken);
    }
    
    ReadPort(<string,string> sourceToken)
    {
        // [top] -> index
        // value -> [top]
        WriteChunk(OpCode.POPHL, sourceToken); // index in L
        WriteChunk(OpCode.LDD, sourceToken);
        WriteChunk(0, sourceToken);
        WriteChunk(OpCode.LDAC, sourceToken);
        WriteChunk(OpCode.LDCL, sourceToken);
        WriteChunk(OpCode.INEC, sourceToken);
        WriteChunk(OpCode.LDCA, sourceToken);
        WriteChunk(OpCode.PUSHDE, sourceToken);
    }
    
    
    WriteMemory(<string,string> sourceToken, bool isWord)
    {
        // [top]  -> value
        // [next] -> address
        WriteChunk(OpCode.POPDE, sourceToken);  // value in DE
        WriteChunk(OpCode.POPHL, sourceToken);  // address in HL
        if (isWord)
        {
            WriteChunk(OpCode.LDHLE, sourceToken);  // LD (HL), E
            WriteChunk(OpCode.INCHL, sourceToken);
            WriteChunk(OpCode.LDHLD, sourceToken);  // LD (HL), D
        }
        else
        {
            WriteChunk(OpCode.LDHLE, sourceToken);  // LD (HL), E
        }
    }
    
    WriteMemoryIncDec(<string,string> sourceToken, bool isWord, bool increment)
    {
        // [top] -> address
        WriteChunk(OpCode.POPHL, sourceToken);  // address in HL
        if (isWord)
        {
            WriteChunk(OpCode.LDEHL, sourceToken);  // LD E, (HL)
            WriteChunk(OpCode.INCHL, sourceToken);
            WriteChunk(OpCode.LDDHL, sourceToken);  // LD D, (HL)
            if (increment)
            {
                WriteChunk(OpCode.INCDE, sourceToken);
            }
            else
            {
                WriteChunk(OpCode.DECDE, sourceToken);
            }
            WriteChunk(OpCode.LDHLD, sourceToken);  // LD (HL), D
            WriteChunk(OpCode.DECHL, sourceToken);
            WriteChunk(OpCode.LDHLE, sourceToken);  // LD (HL), E
        }
        else if (increment)
        {
            WriteChunk(OpCode.INCHLIND, sourceToken);  // INC (HL)
        }
        else
        {
            WriteChunk(OpCode.LDAHL, sourceToken);  // LD A, (HL)
            WriteChunk(OpCode.INCA, sourceToken);
            WriteChunk(OpCode.LDHLA, sourceToken);  // LD (HL), A
        }
    }
    
    WriteMemoryIY(<string,string> sourceToken, bool isWord, int delta)
    {
        if (delta < 0)
        {   
            delta = delta + 256; // -1 -> 0xFF
        }
        byte bdelta = byte(delta);
        
        // [top]  -> value
        WriteChunk(OpCode.POPDE, sourceToken);  // value in DE
        WriteChunk(OpCode.LDIYINDE, sourceToken); // LD (IY+d), E
        WriteChunk(bdelta, sourceToken);
        if (isWord)
        {
            WriteChunk(OpCode.LDIYINDD, sourceToken); // LD (IY+d), D
            WriteChunk(bdelta+1, sourceToken);
        }
    }
    
    ReadMemoryIY(<string,string> sourceToken, bool isWord, int delta)
    {
        if (delta < 0)
        {   
            delta = delta + 256; // -1 -> 0xFF
        }
        byte bdelta = byte(delta);
        
        // value -> [top]
        WriteChunk(OpCode.LDEIYIND, sourceToken); // LD E, (IY+d)
        WriteChunk(bdelta, sourceToken);
        if (isWord)
        {
            WriteChunk(OpCode.LDDIYIND, sourceToken); // LD D, (IY+d)
            WriteChunk(bdelta+1, sourceToken);
        }
        else
        {
            WriteChunk(OpCode.LDD, sourceToken);    // LD D, 0x00
            WriteChunk(0, sourceToken);
        }
        WriteChunk(OpCode.PUSHDE, sourceToken);
    }
    
    
    ReadMemory(<string,string> sourceToken, bool isWord)
    {
        // [top] -> address
        // value -> [top]
        WriteChunk(OpCode.POPHL, sourceToken);  // index
        if (isWord)
        {
            WriteChunk(OpCode.LDEHL, sourceToken);  // LD E, (HL)
            WriteChunk(OpCode.INCHL, sourceToken);
            WriteChunk(OpCode.LDDHL, sourceToken);  // LD D, (HL)
            WriteChunk(OpCode.PUSHDE, sourceToken); 
        }
        else
        {
            WriteChunk(OpCode.LDEHL, sourceToken);  // LD E, (HL)
            WriteChunk(OpCode.LDD, sourceToken);    // LD D, 0x00
            WriteChunk(0, sourceToken);
            WriteChunk(OpCode.PUSHDE, sourceToken); 
        }
    }
    
    
    PushByte(byte value, <string,string> sourceToken) // used by "byte", "char"
    {
        // byte -> [top]
        WriteChunk(OpCode.LDDE, sourceToken);
        WriteChunk(value, sourceToken);
        WriteChunk(0, sourceToken);
        WriteChunk(OpCode.PUSHDE, sourceToken);
    }
    
    PushWord(byte lsb, byte msb, <string,string> sourceToken) // used by "int", "uint"
    {
        // word -> [top]
        WriteChunk(OpCode.LDDE, sourceToken);
        WriteChunk(lsb, sourceToken);
        WriteChunk(msb, sourceToken);
        WriteChunk(OpCode.PUSHDE, sourceToken);
    }
    
    IntegerUnaryMinus(<string,string> sourceToken)
    {
        // (0 - [top]) -> [top]
        WriteChunk(OpCode.POPDE, sourceToken);
        WriteChunk(OpCode.LDHL, sourceToken);
        WriteChunk(0, sourceToken);
        WriteChunk(0, sourceToken);
        WriteChunk(OpCode.ANDA, sourceToken);    // clear the carry flag
        WriteChunk(OpCode.SBCHLDE, sourceToken); // SUB HL, DE : HL = HL - DE
        WriteChunk(OpCode.PUSHHL, sourceToken);
    }
    
    BitwiseShiftRight(string rvalue, <string,string> sourceToken)
    {
        // https://www.chilliant.com/z80shift.html
        // https://map.grauw.nl/articles/mult_div_shifts.php#16bitsh
        
        WriteChunk(OpCode.POPHL, sourceToken); 
        switch (rvalue)
        {
            case "1":
            {
                WriteChunk(OpCode.SRLH, sourceToken); 
                WriteChunk(OpCode.RRL, sourceToken); 
            }
            case "2":
            {
                WriteChunk(OpCode.SRLH, sourceToken); 
                WriteChunk(OpCode.RRL, sourceToken); 
                WriteChunk(OpCode.SRLH, sourceToken); 
                WriteChunk(OpCode.RRL, sourceToken); 
            }
            case "4":
            {
                WriteChunk(OpCode.SRLH, sourceToken); 
                WriteChunk(OpCode.RRL, sourceToken); 
                WriteChunk(OpCode.SRLH, sourceToken); 
                WriteChunk(OpCode.RRL, sourceToken); 
                WriteChunk(OpCode.SRLH, sourceToken); 
                WriteChunk(OpCode.RRL, sourceToken); 
                WriteChunk(OpCode.SRLH, sourceToken); 
                WriteChunk(OpCode.RRL, sourceToken); 
            }
            case "8":
            {
                WriteChunk(OpCode.LDLH, sourceToken); 
                WriteChunk(OpCode.LDH, sourceToken); 
                WriteChunk(0, sourceToken); 
            }
            default:
            {
                ErrorAtCurrent("'>>' only implemented for 1, 2, 4 and 8");
            }
        }
        WriteChunk(OpCode.PUSHHL, sourceToken); 
    }
    BitwiseShiftLeft(string rvalue, <string,string> sourceToken)
    {
        // https://www.chilliant.com/z80shift.html
        // https://map.grauw.nl/articles/mult_div_shifts.php#16bitsh
        
        WriteChunk(OpCode.POPHL, sourceToken); 
        switch (rvalue)
        {
            case "1":
            {
                WriteChunk(OpCode.ADDHLHL, sourceToken); 
            }
            case "2":
            {
                WriteChunk(OpCode.ADDHLHL, sourceToken); 
                WriteChunk(OpCode.ADDHLHL, sourceToken); 
            }
            case "4":
            {
                WriteChunk(OpCode.ADDHLHL, sourceToken); 
                WriteChunk(OpCode.ADDHLHL, sourceToken); 
                WriteChunk(OpCode.ADDHLHL, sourceToken); 
                WriteChunk(OpCode.ADDHLHL, sourceToken); 
            }
            case "8":
            {
                WriteChunk(OpCode.LDHCL, sourceToken); 
                WriteChunk(OpCode.LDL, sourceToken); 
                WriteChunk(0, sourceToken); 
            }
            default:
            {
                ErrorAtCurrent("'<<' only implemented for 1, 2, 4 and 8");
            }
        }
        WriteChunk(OpCode.PUSHHL, sourceToken); 
    }
    IntegerAdd(<string,string> sourceToken)
    {
        WriteChunk(OpCode.POPHL, sourceToken);   // right operand
        WriteChunk(OpCode.POPDE, sourceToken);   // left operand
        WriteChunk(OpCode.ADDHLDE, sourceToken); // ADD HL, DE : HL = HL + DE
        WriteChunk(OpCode.PUSHHL, sourceToken);
    }
    IntegerSubtract(<string,string> sourceToken)
    {
        WriteChunk(OpCode.POPDE, sourceToken);   // right operand
        WriteChunk(OpCode.POPHL, sourceToken);   // left operand
        WriteChunk(OpCode.ANDA, sourceToken);    // clear carry flag
        WriteChunk(OpCode.SBCHLDE, sourceToken); // SUB HL, DE : HL = HL - DE
        WriteChunk(OpCode.PUSHHL, sourceToken);
    }
    
    IntegerMultiplyConstant(string constantValue, <string,string> sourceToken)
    {
        // https://www.chilliant.com/z80shift.html
        // https://map.grauw.nl/articles/mult_div_shifts.php#16bitsh
        
        if (constantValue != "1")
        {
            WriteChunk(OpCode.POPHL, sourceToken); 
        }
        switch (constantValue)
        {
            case "0":
            {
                WriteChunk(OpCode.LDHL, sourceToken); 
                WriteChunk(0, sourceToken); 
                WriteChunk(0, sourceToken); 
            }
            case "2":
            {
                WriteChunk(OpCode.ADDHLHL, sourceToken); 
            }
            case "3":
            {
                WriteChunk(OpCode.LDEL, sourceToken);
                WriteChunk(OpCode.LDDH, sourceToken);
                WriteChunk(OpCode.ADDHLHL, sourceToken);
                WriteChunk(OpCode.ADDHLDE, sourceToken); 
            }
            case "4":
            {
                WriteChunk(OpCode.ADDHLHL, sourceToken); 
                WriteChunk(OpCode.ADDHLHL, sourceToken); 
            }
            case "5":
            {
                WriteChunk(OpCode.LDEL, sourceToken);
                WriteChunk(OpCode.LDDH, sourceToken);
                WriteChunk(OpCode.ADDHLHL, sourceToken); 
                WriteChunk(OpCode.ADDHLHL, sourceToken); 
                WriteChunk(OpCode.ADDHLDE, sourceToken); 
            }
            default:
            {
                ErrorAtCurrent("IntegerMultiplyConstant only implemented for 0, 1, 2, 3, 4, 5");
            }
        }
        if (constantValue != "1")
        {
            WriteChunk(OpCode.PUSHHL, sourceToken); 
        }
    }
    
    IntegerMultiply(<string,string> sourceToken)
    {
        // https://map.grauw.nl/articles/mult_div_shifts.php#div
        //
        // Multiply 16-bit values (with 16-bit result)
        // In: Multiply BC with DE
        // Out: HL = result
        //
        WriteChunk(OpCode.POPDE, sourceToken);  // [top]
        WriteChunk(OpCode.POPHL, sourceToken);  // [next]
        WriteChunk(OpCode.PUSHBC, sourceToken);
        WriteChunk(OpCode.LDBH, sourceToken);
        WriteChunk(OpCode.LDCL, sourceToken);
//Mult16:
        WriteChunk(OpCode.LDAB, sourceToken);
        WriteChunk(OpCode.LDB, sourceToken);
        WriteChunk(16, sourceToken); 
//Mult16_Loop:
        WriteChunk(OpCode.ADDHLHL, sourceToken);
        WriteChunk(OpCode.SLAC, sourceToken);
        WriteChunk(OpCode.RLA, sourceToken);
        WriteChunk(OpCode.JRNC, sourceToken); // Mult16_NoAdd
        WriteChunk(1, sourceToken); 
        WriteChunk(OpCode.ADDHLDE, sourceToken);
//Mult16_NoAdd:
        WriteChunk(OpCode.DJNZ, sourceToken); // Mult16_Loop
        WriteChunk(0xF7, sourceToken); 
        WriteChunk(OpCode.POPBC, sourceToken);
        WriteChunk(OpCode.PUSHHL, sourceToken);
    }
    IntegerDivideModulus(<string,string> sourceToken, bool modulus)
    {
        // https://map.grauw.nl/articles/mult_div_shifts.php#div
        //
        // Divide 16-bit values (with 16-bit result)
        // In: Divide BC [next] by divider DE [top]
        // Out: BC = result, HL = rest
        //
        WriteChunk(OpCode.POPDE, sourceToken);  // [top]
        WriteChunk(OpCode.POPHL, sourceToken);  // [next]
        WriteChunk(OpCode.PUSHBC, sourceToken);
        WriteChunk(OpCode.LDBH, sourceToken);
        WriteChunk(OpCode.LDCL, sourceToken);
//Div16:
        WriteChunk(OpCode.LDHL, sourceToken);
        WriteChunk(0, sourceToken); 
        WriteChunk(0, sourceToken); 
        WriteChunk(OpCode.LDAB, sourceToken);
        WriteChunk(OpCode.LDB, sourceToken);
        WriteChunk(8, sourceToken); 
//Div16_Loop1:
        WriteChunk(OpCode.RLA, sourceToken);
        WriteChunk(OpCode.ADCHLHL, sourceToken);
        WriteChunk(OpCode.SBCHLDE, sourceToken);
        WriteChunk(OpCode.JRNC, sourceToken); // Div16_NoAdd1
        WriteChunk(1, sourceToken); 
        WriteChunk(OpCode.ADDHLDE, sourceToken);
//Div16_NoAdd1:
        WriteChunk(OpCode.DJNZ, sourceToken); // Div16_Loop1
        WriteChunk(0xF6, sourceToken); 
        WriteChunk(OpCode.RLA, sourceToken);
        WriteChunk(OpCode.CPL, sourceToken);
        WriteChunk(OpCode.LDBA, sourceToken);
        WriteChunk(OpCode.LDAC, sourceToken);
        WriteChunk(OpCode.LDCB, sourceToken);
        WriteChunk(OpCode.LDB, sourceToken);
        WriteChunk(8, sourceToken); 
//Div16_Loop2:
        WriteChunk(OpCode.RLA, sourceToken);
        WriteChunk(OpCode.ADCHLHL, sourceToken);
        WriteChunk(OpCode.SBCHLDE, sourceToken);
        WriteChunk(OpCode.JRNC, sourceToken); // Div16_NoAdd2
        WriteChunk(1, sourceToken); 
        WriteChunk(OpCode.ADDHLDE, sourceToken);
//Div16_NoAdd2:
        WriteChunk(OpCode.DJNZ, sourceToken); // Div16_Loop2
        WriteChunk(0xF6, sourceToken); 
        WriteChunk(OpCode.RLA, sourceToken);
        WriteChunk(OpCode.CPL, sourceToken);
        WriteChunk(OpCode.LDBC, sourceToken);
        WriteChunk(OpCode.LDCA, sourceToken);
        if (modulus)
        {
            WriteChunk(OpCode.POPBC, sourceToken);
            WriteChunk(OpCode.PUSHHL, sourceToken); // remainder
        }
        else
        {
            WriteChunk(OpCode.POPHL, sourceToken);
            WriteChunk(OpCode.PUSHBC, sourceToken); // result
            WriteChunk(OpCode.LDBH, sourceToken);
            WriteChunk(OpCode.LDCL, sourceToken);
        }
    }
    
    
    BitwiseAnd(<string,string> sourceToken)
    {
        WriteChunk(OpCode.POPHL, sourceToken);   // right operand
        WriteChunk(OpCode.POPDE, sourceToken);   // left operand
        WriteChunk(OpCode.LDAL, sourceToken);    // LD A, L
        WriteChunk(OpCode.ANDAE, sourceToken);   // AND A, E
        WriteChunk(OpCode.LDLA, sourceToken);    // LD L, A
        WriteChunk(OpCode.LDAH, sourceToken);    // LD A, H
        WriteChunk(OpCode.ANDAD, sourceToken);   // AND A, D
        WriteChunk(OpCode.LDHA, sourceToken);    // LD H, A
        WriteChunk(OpCode.PUSHHL, sourceToken);
    }
    BitwiseOr(<string,string> sourceToken)
    {
        WriteChunk(OpCode.POPHL, sourceToken);   // right operand
        WriteChunk(OpCode.POPDE, sourceToken);   // left operand
        WriteChunk(OpCode.LDAL, sourceToken);    // LD A, L
        WriteChunk(OpCode.ORAE, sourceToken);    // OR A, E
        WriteChunk(OpCode.LDLA, sourceToken);    // LD L, A
        WriteChunk(OpCode.LDAH, sourceToken);    // LD A, H
        WriteChunk(OpCode.ORAD, sourceToken);    // OR A, D
        WriteChunk(OpCode.LDHA, sourceToken);    // LD H, A
        WriteChunk(OpCode.PUSHHL, sourceToken);
    }

    LessThan(<string,string> sourceToken)
    {
        WriteChunk(OpCode.POPDE, sourceToken);   // right operand
        WriteChunk(OpCode.POPHL, sourceToken);   // left operand
        WriteChunk(OpCode.ANDA, sourceToken);    // clear the carry flag
        WriteChunk(OpCode.SBCHLDE, sourceToken); // SUB HL, DE : HL = HL - DE

        // right > left -> left - right < 0
        WriteChunk(OpCode.LDHL, sourceToken);  // HL = 0
        Chunks.WriteChunk(1, sourceToken);
        Chunks.WriteChunk(0, sourceToken);
        WriteChunk(OpCode.JPM, sourceToken);   // JP M jpmPatch - jump if the result of the substraction is negative
        uint jpmPatch = Chunks.GetNextAddress();
        Chunks.WriteChunk(0, sourceToken);
        Chunks.WriteChunk(0, sourceToken);
        Chunks.UpdatePeepholeBoundary();
        // was not negative or zero so >= 0
        WriteChunk(OpCode.DECL, sourceToken);  // L = L - 1
// jpmPatch:        
        Chunks.PatchJumpToHere(jpmPatch);
        Chunks.UpdatePeepholeBoundary();
// past:
        WriteChunk(OpCode.PUSHHL, sourceToken);
    }
    LessThanOrEqual(<string,string> sourceToken)
    {
        WriteChunk(OpCode.POPHL, sourceToken);   // right operand
        WriteChunk(OpCode.POPDE, sourceToken);   // left operand
        WriteChunk(OpCode.ANDA, sourceToken);    // clear the carry flag
        WriteChunk(OpCode.SBCHLDE, sourceToken); // SUB HL, DE : HL = HL - DE
        
        // left <= right -> right - left >= 0
        WriteChunk(OpCode.LDHL, sourceToken);  // HL = 0
        Chunks.WriteChunk(0, sourceToken);
        Chunks.WriteChunk(0, sourceToken);
        WriteChunk(OpCode.JPM, sourceToken); // JP M jpmPatch - jump if the result of the substraction is negative
        uint jpmPatch = Chunks.GetNextAddress();
        Chunks.WriteChunk(0, sourceToken);
        Chunks.WriteChunk(0, sourceToken);
        Chunks.UpdatePeepholeBoundary();
        // was not negative or zero so >= 0
        WriteChunk(OpCode.INCL, sourceToken);  // L = L + 1
//jpmPatch:        
        Chunks.PatchJumpToHere(jpmPatch);
        Chunks.UpdatePeepholeBoundary();
//past:        
        WriteChunk(OpCode.PUSHHL, sourceToken);
    }
    GreaterThan(<string,string> sourceToken)
    {
        WriteChunk(OpCode.POPHL, sourceToken);   // right operand
        WriteChunk(OpCode.POPDE, sourceToken);   // left operand
        WriteChunk(OpCode.ANDA, sourceToken);    // clear the carry flag
        WriteChunk(OpCode.SBCHLDE, sourceToken); // SUB HL, DE : HL = HL - DE

        // left > right -> right - left < 0
        WriteChunk(OpCode.LDHL, sourceToken);  // HL = 1
        Chunks.WriteChunk(1, sourceToken);
        Chunks.WriteChunk(0, sourceToken);
        WriteChunk(OpCode.JPM, sourceToken);   // JP M jpmPatch - jump if the result of the substraction is negative
        uint jpmPatch = Chunks.GetNextAddress();
        Chunks.WriteChunk(0, sourceToken);
        Chunks.WriteChunk(0, sourceToken);
        Chunks.UpdatePeepholeBoundary();
        // was not negative or zero so >= 0
        WriteChunk(OpCode.DECL, sourceToken);  // L = L - 1
// jpmPatch:        
        Chunks.PatchJumpToHere(jpmPatch);
        Chunks.UpdatePeepholeBoundary();
// past:
        WriteChunk(OpCode.PUSHHL, sourceToken);
    }
    GreaterThanOrEqual(<string,string> sourceToken)
    {
        WriteChunk(OpCode.POPHL, sourceToken);   // right operand
        WriteChunk(OpCode.POPDE, sourceToken);   // left operand
        WriteChunk(OpCode.ANDA, sourceToken);    // clear the carry flag
        WriteChunk(OpCode.SBCHLDE, sourceToken); // SUB HL, DE : HL = HL - DE

        // left >= right -> right - left <= 0
        WriteChunk(OpCode.LDHL, sourceToken);  // HL = 1
        Chunks.WriteChunk(1, sourceToken);
        Chunks.WriteChunk(0, sourceToken);
        WriteChunk(OpCode.JPM, sourceToken); // JP M - jump if the result of the substraction is negative
        uint jpmPatch = Chunks.GetNextAddress();
        Chunks.WriteChunk(0, sourceToken);
        Chunks.WriteChunk(0, sourceToken);
        Chunks.UpdatePeepholeBoundary();
        WriteChunk(OpCode.JRZ, sourceToken); // JR Z - jump if the result of the substraction is zero
        Chunks.WriteChunk(1, sourceToken);
        Chunks.UpdatePeepholeBoundary();
        // was not negative or zero so > 0
        WriteChunk(OpCode.DECL, sourceToken);  // L = L - 1
        Chunks.PatchJumpToHere(jpmPatch);
        Chunks.UpdatePeepholeBoundary();
// past:        
        WriteChunk(OpCode.PUSHHL, sourceToken);
    }
    
    PushBoolean(bool bvalue, <string,string> sourceToken) // used by "bool"
    {
        // byte -> [top]
        byte value = 0;
        if (bvalue)
        {
            value = 1;
        }
        WriteChunk(OpCode.LDDE, sourceToken);
        WriteChunk(value, sourceToken);
        WriteChunk(0, sourceToken);
        WriteChunk(OpCode.PUSHDE, sourceToken);
    }
    
    CompareEqualBoolean(<string,string> sourceToken)
    {
        WriteChunk(OpCode.POPDE, sourceToken);   // right operand
        WriteChunk(OpCode.POPHL, sourceToken);   // left operand
        WriteChunk(OpCode.LDAL, sourceToken);    // LD A, L
        WriteChunk(OpCode.XORE, sourceToken);    // XOR A, E
        WriteChunk(OpCode.XOR, sourceToken);     // XOR A, 0x01
        WriteChunk(1, sourceToken);
        WriteChunk(OpCode.LDLA, sourceToken);    // LD L, A
        WriteChunk(OpCode.PUSHHL, sourceToken);
    }
    CompareNotEqualBoolean(<string,string> sourceToken)
    {
        WriteChunk(OpCode.POPDE, sourceToken);   // right operand
        WriteChunk(OpCode.POPHL, sourceToken);   // left operand
        WriteChunk(OpCode.LDAL, sourceToken);    // LD A, L
        WriteChunk(OpCode.XORE, sourceToken);    // XOR A, E
        WriteChunk(OpCode.LDLA, sourceToken);    // LD L, A
        WriteChunk(OpCode.PUSHHL, sourceToken);
    }
    
    CompareEqual(<string,string> sourceToken)
    {
        // integer Subtract
        WriteChunk(OpCode.POPDE, sourceToken);   // right operand
        WriteChunk(OpCode.POPHL, sourceToken);   // left operand
        WriteChunk(OpCode.ANDA, sourceToken);    // clear the carry flag
        WriteChunk(OpCode.SBCHLDE, sourceToken); // SUB HL, DE : HL = HL - DE
        
        // if HL == 0, we push 1
        // if HL != 0, we push 0
        
        // is HL zero? -> Z
        WriteChunk(OpCode.LDAH, sourceToken);    // LD A, H
        WriteChunk(OpCode.ORAL, sourceToken);    // OR A, L
        WriteChunk(OpCode.JRZ, sourceToken);     // JR Z inc
        WriteChunk(+2, sourceToken);
        WriteChunk(OpCode.LDL, sourceToken);
        WriteChunk(0xFF, sourceToken);
// inc:
        WriteChunk(OpCode.INCL, sourceToken);  // HL = HL + 1
// end:        
        WriteChunk(OpCode.PUSHHL, sourceToken);
    }
    CompareNotEqual(<string,string> sourceToken)
    {
        // integer Subtract
        WriteChunk(OpCode.POPDE, sourceToken);   // right operand
        WriteChunk(OpCode.POPHL, sourceToken);   // left operand
        WriteChunk(OpCode.ANDA, sourceToken);    // clear the carry flag
        WriteChunk(OpCode.SBCHLDE, sourceToken); // SUB HL, DE : HL = HL - DE
        WriteChunk(OpCode.JRZ, sourceToken);     // JR Z end
        WriteChunk(+2, sourceToken);
        WriteChunk(OpCode.LDL, sourceToken);
        WriteChunk(1, sourceToken);
        
// end:        
        WriteChunk(OpCode.PUSHHL, sourceToken);
    }
    
    BooleanNot(<string,string> sourceToken)
    {
        // !([Top]) -> [Top]
        WriteChunk(OpCode.POPHL, sourceToken);
        WriteChunk(OpCode.LDAL, sourceToken);    // LD A, L
        WriteChunk(OpCode.XOR, sourceToken);     // XOR A, 0x01
        WriteChunk(1, sourceToken);
        WriteChunk(OpCode.LDLA, sourceToken);    // LD L, A
        WriteChunk(OpCode.PUSHHL, sourceToken);
    }
    
    BooleanOr(<string,string> sourceToken)
    {
        // if (HL == 0) and (DE == 0), we push 0
        // if not, we push 1
        WriteChunk(OpCode.POPHL, sourceToken);   // right operand
        WriteChunk(OpCode.POPDE, sourceToken);   // left operand
        WriteChunk(OpCode.ADDHLDE, sourceToken); // ADD HL, DE
        WriteChunk(OpCode.JRZ, sourceToken);     // JR Z end
        WriteChunk(+3, sourceToken);
        WriteChunk(OpCode.LDHL, sourceToken);
        WriteChunk(1, sourceToken);
        WriteChunk(0, sourceToken);
// end
        WriteChunk(OpCode.PUSHHL, sourceToken);
    }
    BooleanAnd(<string,string> sourceToken)
    {
        WriteChunk(OpCode.POPHL, sourceToken);   // right operand
        WriteChunk(OpCode.POPDE, sourceToken);   // left operand
        // is HL zero? -> Z
        WriteChunk(OpCode.LDAH, sourceToken);    // if HL is zero, don't bother looking at DE
        WriteChunk(OpCode.ORAL, sourceToken);
        WriteChunk(OpCode.JRZ, sourceToken);     // JR Z end
        WriteChunk(+2, sourceToken);             
        WriteChunk(OpCode.LDHD, sourceToken);    // if HL not zero, the result of the && is whateve DE happens to be
        WriteChunk(OpCode.LDLE, sourceToken);    
// end:        
        WriteChunk(OpCode.PUSHHL, sourceToken);
    }
    ZeroCheck(<string,string> sourceToken)
    {
        WriteChunk(OpCode.POPHL, sourceToken);
        
        WriteChunk(OpCode.LDAH, sourceToken);
        WriteChunk(OpCode.ORAL, sourceToken);
        WriteChunk(OpCode.LDHL, sourceToken);  // HL = 0
        WriteChunk(0, sourceToken);
        WriteChunk(0, sourceToken);
        WriteChunk(OpCode.JRNZ, sourceToken);  // JR NZ end
        WriteChunk(+1, sourceToken);
        WriteChunk(OpCode.INCL, sourceToken);  // HL = HL + 1
// end:                
        WriteChunk(OpCode.PUSHHL, sourceToken);
    }
    
    NotZeroCheck(<string,string> sourceToken)
    {
        WriteChunk(OpCode.POPHL, sourceToken);
        
        WriteChunk(OpCode.LDAH, sourceToken);
        WriteChunk(OpCode.ORAL, sourceToken);
        WriteChunk(OpCode.LDHL, sourceToken);  // HL = 0
        WriteChunk(0, sourceToken);
        WriteChunk(0, sourceToken);
        WriteChunk(OpCode.JRZ, sourceToken);  // JR Z end
        WriteChunk(+1, sourceToken);
        WriteChunk(OpCode.INCL, sourceToken);  // HL = HL + 1
// end:                
        WriteChunk(OpCode.PUSHHL, sourceToken);
    }
}
