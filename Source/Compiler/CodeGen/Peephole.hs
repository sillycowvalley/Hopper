unit Peephole
{
    uses "Instructions"
    uses "CodeStream"
    
    uint lastInstruction0;
    uint lastInstruction1; // -1
    uint lastInstruction2; // -2
    uint lastInstruction3; // -3
    uint lastInstruction4; // -4
    
    uint peephholeBoundary;
    
    Initialize()
    {
        lastInstruction0 = 0; lastInstruction1 = 0; lastInstruction2 = 0; lastInstruction3 = 0; lastInstruction4 = 0; // not really ..
        peephholeBoundary = 0; // reset to start
    }
    UpdatePeepholeBoundary(uint codeLength)
    {
        peephholeBoundary = 0;
        if (codeLength > 0)
        {
            peephholeBoundary = codeLength-1; // current last instruction is out of bounds
        }
    }
    uint PeepholeBoundary
    {
        get
        {
            return peephholeBoundary;
        }
    }

    uint LastInstructionIndex 
    { 
       get 
       { 
           return lastInstruction0; 
       }
       set
       {
           lastInstruction4 = lastInstruction3;
           lastInstruction3 = lastInstruction2;
           lastInstruction2 = lastInstruction1;
           lastInstruction1 = lastInstruction0;
           lastInstruction0 = value;
       }
    }
    
    popAndTrim(<byte> currentStream, byte instructionsToPop, uint bytestoTrim)
    {
        while (bytestoTrim != 0)
        {
            currentStream.Remove(currentStream.Count-1);
            bytestoTrim--;
        }
        while (instructionsToPop != 0)
        {
            lastInstruction0 = lastInstruction1;
            lastInstruction1 = lastInstruction2;
            lastInstruction2 = lastInstruction3;
            lastInstruction3 = lastInstruction4;
            lastInstruction4 = 0;
            instructionsToPop--;
        }
    }
    
    PeepholeOptimize(<byte> currentStream)
    {
        // peepholeOptimization takes place before boundary is updated after jump instructions
        // but after operands have been emitted
        
        if ((!CodeStream.CheckedBuild || PeepHole) && !NoPackedInstructions)
        {
            loop
            {
                // at least 1 instruction
                if (lastInstruction0 > peephholeBoundary) 
                {
                    if (peepholeOptimize1(currentStream))
                    {
                        continue; // hunt for more
                    }
                }
                // at least 2 instructions
                if (lastInstruction1 > peephholeBoundary) 
                {
                    if (lastInstruction1 != lastInstruction0) // why?! it does happen
                    {
                        if (peepholeOptimize2(currentStream))
                        {
                            continue; // hunt for more
                        }
                    }
                }
                // at least 3 instructions
                if (lastInstruction2 > peephholeBoundary) 
                {
                    if (peepholeOptimize3(currentStream))
                    {
                        continue; // hunt for more
                    }
                }
                // at least 4 instructions
                if (lastInstruction3 > peephholeBoundary) 
                {
                    if (lastInstruction1 != lastInstruction0) // why?!
                    {
                        if (peepholeOptimize4(currentStream))
                        {
                            continue; // hunt for more
                        }
                    }
                }
                break;
            }
        }
    }
    
    bool IsPushGlobalB(<byte> currentStream, uint index, ref byte offset, ref byte length)
    {
        Instruction opCode = Instruction(currentStream[index]);
        if (opCode == Instruction.PUSHGLOBALB)
        {
            offset = currentStream[index+1];   
            length = 2;    
            return true;
        }
        return false;
    }
    
    bool IsPopGlobalB(<byte> currentStream, uint index, ref byte offset, ref byte length)
    {
        Instruction opCode = Instruction(currentStream[index]);
        if (opCode == Instruction.POPGLOBALB)
        {
            offset = currentStream[index+1];   
            length = 2;    
            return true;
        }
        return false;
    }
    
    bool IsPushLocalB(<byte> currentStream, uint index, ref byte offset, ref byte length)
    {
        Instruction opCode = Instruction(currentStream[index]);
        length = 1;
        switch (opCode)
        {
            case Instruction.PUSHLOCALB:
            {
                offset = currentStream[index+1];   
                length = 2;    
                return true;
            }
            case Instruction.PUSHLOCALB00:
            {
                offset = 0;   
                return true;
            }
            case Instruction.PUSHLOCALB01:
            {
                offset = 1;   
                return true;
            }
        }
        return false;
    }
    
    bool IsPopLocalB(<byte> currentStream, uint index, ref byte offset, ref byte length)
    {
        Instruction opCode = Instruction(currentStream[index]);
        length = 1; 
        switch (opCode)
        {
            case Instruction.POPLOCALB:
            {
                offset = currentStream[index+1];
                length = 2; 
                return true;
            }
            case Instruction.POPLOCALB00:
            {
                offset = 0;
                return true;
            }
            case Instruction.POPLOCALB01:
            {
                offset = 1;
                return true;
            }
        }
        return false;
    }
    bool peepholeOptimize3(<byte> currentStream)
    {
        if (    (currentStream[lastInstruction2] == byte(Instruction.PUSHI0))
             && (currentStream[lastInstruction1] == byte(Instruction.PUSHI1))
             && (currentStream[lastInstruction0] == byte(Instruction.SUBI))
           ) 
        {
            // PUSHI0 PUSHI1 SUBI -> PUSHIM1
            // i2     i1     i0      i2
            currentStream.SetItem(lastInstruction2,   byte(Instruction.PUSHIM1));
            popAndTrim(currentStream, 2, 2);
            return true; // hunt for more
        }
        if (currentStream[lastInstruction2] == byte(Instruction.PUSHLOCALBB))
        {
            byte offset2 = currentStream[lastInstruction2+1]; 
            byte offset1 = currentStream[lastInstruction2+2]; 
            byte length1 = 1;
            
            byte offset0;
            byte length0;
            bool isPopLocalB0  = IsPopLocalB (currentStream, lastInstruction0, ref offset0, ref length0);
            if (isPopLocalB0)
            {
                if (((offset2 == offset0) || (offset1 == offset0))
                 && (currentStream[lastInstruction1] == byte(Instruction.ADD))
                   )
                {
                    // PUSHLOCALBB ADD POPLOCALB -> INCLOCALBB
                    // i2          i1  i0           i2
                    currentStream.SetItem(lastInstruction2, byte(Instruction.INCLOCALBB));
                    currentStream.SetItem(lastInstruction2+1, offset0);
                    if (offset2 == offset0)
                    {
                        currentStream.SetItem(lastInstruction2+2, offset1);
                    }
                    else
                    {
                        currentStream.SetItem(lastInstruction2+2, offset2);
                    }
                    
                    // 6 -> 3 = -3
                    uint remove = length0 + length1;
                    popAndTrim(currentStream, 2, remove);
                    return true; // hunt for more
                }
                if (((offset2 == offset0) || (offset1 == offset0))
                 && (currentStream[lastInstruction1] == byte(Instruction.ADDI))
                   )
                {
                    // PUSHLOCALBB ADDI POPLOCALB -> INCLOCALIBB
                    // i2          i1   i0           i3
                    currentStream.SetItem(lastInstruction2, byte(Instruction.INCLOCALIBB));
                    currentStream.SetItem(lastInstruction2+1, offset0);
                    if (offset2 == offset0)
                    {
                        currentStream.SetItem(lastInstruction2+2, offset1);
                    }
                    else
                    {
                        currentStream.SetItem(lastInstruction2+2, offset2);
                    }
                    
                    // 6 -> 3 = -3
                    uint remove = length0 + length1;
                    popAndTrim(currentStream, 2, remove);
                    return true; // hunt for more
                }
            }
        }
        return false;
    }
    bool peepholeOptimize4(<byte> currentStream)
    {
        byte offset3 = 0;
        byte length3 = 0;
        bool isPushLocalB3  = IsPushLocalB (currentStream, lastInstruction3, ref offset3, ref length3);
        bool isPushGlobalB3 = IsPushGlobalB(currentStream, lastInstruction3, ref offset3, ref length3);
        
        byte offset0 = 0;
        byte length0 = 0;
        bool isPopLocalB0  = IsPopLocalB (currentStream, lastInstruction0, ref offset0, ref length0);
        bool isPopGlobalB0 = IsPopGlobalB(currentStream, lastInstruction0, ref offset0, ref length0);
        
        if (isPushGlobalB3 && isPopGlobalB0)
        {
            if ((offset3 == offset0)
             && (currentStream[lastInstruction2] == byte(Instruction.PUSHI1))
             && (currentStream[lastInstruction1] == byte(Instruction.ADD)) 
               )
            {
                // PUSHGLOBALB PUSHI1 ADD POPGLOBALB -> INCGLOBALB
                // i3          i2     i1  i0            i3
                currentStream.SetItem(lastInstruction3,   byte(Instruction.INCGLOBALB));
                currentStream.SetItem(lastInstruction3+1, offset0);
                // 6 -> 2 = -4
                uint remove = (length0-1) + (length3-1) + 2;
                popAndTrim(currentStream, 3, remove);
                return true; // hunt for more
            }
            if ((offset3 == offset0)
             && (currentStream[lastInstruction2] == byte(Instruction.PUSHI1))
             && (currentStream[lastInstruction1] == byte(Instruction.SUB)) 
               )
            {
                // PUSHGLOBALB PUSHI1 ADD POPGLOBALB -> DECGLOBALB
                // i3          i2     i1  i0            i3
                currentStream.SetItem(lastInstruction3,   byte(Instruction.DECGLOBALB));
                currentStream.SetItem(lastInstruction3+1, offset0);
                // 6 -> 2 = -4
                uint remove = (length0-1) + (length3-1) + 2;
                popAndTrim(currentStream, 3, remove);
                return true; // hunt for more
            }
        }
        if (isPushLocalB3 && isPopLocalB0)
        {
            if ((offset3 == offset0)
             && (currentStream[lastInstruction2] == byte(Instruction.PUSHI1))
             && (currentStream[lastInstruction1] == byte(Instruction.ADD)) 
               )
            {
                // PUSHLOCALB PUSHI1 ADD POPLOCALB -> INCLOCALB
                // i3         i2     i1  i0           i3
                currentStream.SetItem(lastInstruction3,   byte(Instruction.INCLOCALB));
                currentStream.SetItem(lastInstruction3+1, offset0);
                // 6 -> 2 = -4
                uint remove = (length0-1) + (length3-1) + 2;
                popAndTrim(currentStream, 3, remove);
                return true; // hunt for more
            }
            byte offset2 = 0;
            byte length2 = 0;
            bool isPushLocalB2 = IsPushLocalB(currentStream, lastInstruction2, ref offset2, ref length2);
        
            if (isPushLocalB2 
             && ((offset3 == offset0) || (offset2 == offset0))
             && (currentStream[lastInstruction1] == byte(Instruction.ADD))
               )
            {
                // PUSHLOCALB PUSHLOCALB ADD POPLOCALB -> INCLOCALBB
                // i3         i2         i1  i0           i3
                currentStream.SetItem(lastInstruction3, byte(Instruction.INCLOCALBB));
                currentStream.SetItem(lastInstruction3+1, offset0);
                if (offset3 == offset0)
                {
                    currentStream.SetItem(lastInstruction3+2, offset2);
                }
                else
                {
                    currentStream.SetItem(lastInstruction3+2, offset3);
                }
                
                // 7 -> 3 = -4
                uint remove = (length0-1) + (length2-1) + (length3-1) + 1;
                popAndTrim(currentStream, 3, remove);
                return true; // hunt for more
            }
            if (isPushLocalB2
             && ((offset3 == offset0) || (offset2 == offset0))
             && (currentStream[lastInstruction1] == byte(Instruction.ADDI))
               )
            {
                // PUSHLOCALB PUSHLOCALB ADDI POPLOCALB -> INCLOCALIBB
                // i3         i2         i1   i0           i3
                currentStream.SetItem(lastInstruction3, byte(Instruction.INCLOCALIBB));
                currentStream.SetItem(lastInstruction3+1, offset0);
                if (offset3 == offset0)
                {
                    currentStream.SetItem(lastInstruction3+2, offset2);
                }
                else
                {
                    currentStream.SetItem(lastInstruction3+2, offset3);
                }
                
                // 7 -> 3 = -4
                uint remove = (length0-1) + (length2-1) + (length3-1) + 1;
                popAndTrim(currentStream, 3, remove);
                return true; // hunt for more
            }
        }
        return false;   
    }
   	
    bool peepholeOptimize1(<byte> currentStream)
    {
        Instruction instruction = Instruction.NOP;
        if (currentStream[lastInstruction0] == byte(Instruction.PUSHI))
        {
            uint operand = currentStream[lastInstruction0+1] + (currentStream[lastInstruction0+2] << 8);
            if (operand <= 255)
            {
                instruction = Instruction.PUSHIB;
            }
        }
        if (currentStream[lastInstruction0] == byte(Instruction.PUSHIB))
        {
            byte operand = currentStream[lastInstruction0+1];
            if (operand == 0)
            {
                instruction = Instruction.PUSHI0;
            }
            if (operand == 1)
            {
                instruction = Instruction.PUSHI1;
            }
        }
        
        byte offset0 = 0;
        byte length0 = 0;
        bool isPushLocalB = IsPushLocalB(currentStream, lastInstruction0, ref offset0, ref length0);
        
        if (isPushLocalB && (length0 == 2) && ((offset0 == 0) || (offset0 == 1)))
        {
            // PUSHLOCALB -> PUSHLOCALB00 | PUSHLOCALB01
            // i0         -> i0
            instruction = (offset0 == 0) ? Instruction.PUSHLOCALB00 : Instruction.PUSHLOCALB01;
        }
        
        bool isPopLocalB = IsPopLocalB(currentStream, lastInstruction0, ref offset0, ref length0);
        if (isPopLocalB && (length0 == 2) && ((offset0 == 0) || (offset0 == 1)))
        {
            // POPLOCALB -> POPLOCALB00 | POPLOCALB01
            // i0         -> i0
            instruction = (offset0 == 0) ? Instruction.POPLOCALB00 : Instruction.POPLOCALB01;
        }
        if (instruction != Instruction.NOP)
        {
            currentStream.SetItem(lastInstruction0, byte(instruction));
            popAndTrim(currentStream, 0, 1); // MSB
            return true; // hunt for more
        }
        return false;
    }
    
    bool peepholeOptimize2(<byte> currentStream)
    {    
        if (    (currentStream[lastInstruction1] == byte(Instruction.BITANDFF)) 
             || (currentStream[lastInstruction1] == byte(Instruction.BITANDB)) 
             || (currentStream[lastInstruction1] == byte(Instruction.BITSHR8)) 
           )
        {
            if ((currentStream[lastInstruction0] == byte(Instruction.CAST)))
            {
                byte value = currentStream[lastInstruction0+1];
                if (value == byte(type(byte)))
                {
                    popAndTrim(currentStream, 1, 2);
                    return true; // hunt for more
                }
            }
        }
        if (
            (currentStream[lastInstruction1] == byte(Instruction.PUSHI1)) 
         && ( (currentStream[lastInstruction0] == byte(Instruction.MUL)) || 
              (currentStream[lastInstruction0] == byte(Instruction.MULI)) || 
              (currentStream[lastInstruction0] == byte(Instruction.DIV)) || 
              (currentStream[lastInstruction0] == byte(Instruction.DIVI))
            )
           )
        {
            // PUSHI1 MUL|DIV    -> NOP
            popAndTrim(currentStream, 2, 2);
            return true; // hunt for more
        }
        if (
            (currentStream[lastInstruction1] == byte(Instruction.PUSHI0)) 
         && ( (currentStream[lastInstruction0] == byte(Instruction.ADD)) || 
              (currentStream[lastInstruction0] == byte(Instruction.SUB)) || 
              (currentStream[lastInstruction0] == byte(Instruction.ADDI)) || 
              (currentStream[lastInstruction0] == byte(Instruction.SUBI))
            )
           )
        {
            // PUSHI0 ADD|SUB    -> NOP
            popAndTrim(currentStream, 2, 2);
            return true; // hunt for more
        }
        if ( 
            (currentStream[lastInstruction1] == byte(Instruction.PUSHIB)) 
         && ( (currentStream[lastInstruction0] == byte(Instruction.MUL)) || (currentStream[lastInstruction0] == byte(Instruction.DIV)))
           )
        {
            byte shift;
            byte value = currentStream[lastInstruction1+1];
            switch (value)
            {
                case 2:   { shift = 1; }
                case 4:   { shift = 2; }
                case 8:   { shift = 3; }
                case 16:  { shift = 4; }
                case 32:  { shift = 5; }
                case 64:  { shift = 6; }
                case 128: { shift = 7; }
            }
            if (shift != 0)
            {
                if (currentStream[lastInstruction0] == byte(Instruction.MUL))
                {
                    // PUSHIB MUL    -> BITSHLB
                    // i1     i0     -> i0
                    currentStream.SetItem(lastInstruction1,   byte(Instruction.BITSHLB));
                    currentStream.SetItem(lastInstruction1+1, shift);
                }
                else
                {
                    // PUSHIB DIV    -> BITSHRB
                    // i1     i0     -> i0
                    currentStream.SetItem(lastInstruction1,   byte(Instruction.BITSHRB));
                    currentStream.SetItem(lastInstruction1+1, shift);
                }
                popAndTrim(currentStream, 1, 1);
                return true; // hunt for more
            }
            
        }
        if (
            (currentStream[lastInstruction1] == byte(Instruction.PUSHIB)) 
         && (currentStream[lastInstruction0] == byte(Instruction.BITSHL))
           )
        {
            byte value = currentStream[lastInstruction1+1];
            if (value == 8)
            {
                currentStream.SetItem(lastInstruction1, byte(Instruction.BITSHL8));
                popAndTrim(currentStream, 1, 2);
                return true; // hunt for more
            }
            else
            {
                // PUSHIB BITSHL -> BITSHLB
                // i1     i0     -> i0
                currentStream.SetItem(lastInstruction1, byte(Instruction.BITSHLB));
                popAndTrim(currentStream, 1, 1);
                return true; // hunt for more
            }
        }
        if (
            (currentStream[lastInstruction1] == byte(Instruction.PUSHI1)) 
         && (currentStream[lastInstruction0] == byte(Instruction.BITSHL))
           )
        {
            // PUSHI1 BITSHL -> BITSHLB
            // i1     i0     -> i0
            currentStream.SetItem(lastInstruction1, byte(Instruction.BITSHLB));
            currentStream.SetItem(lastInstruction0, byte(1));
            popAndTrim(currentStream, 1, 0);
            return true; // hunt for more
        }
        if (
            (currentStream[lastInstruction1] == byte(Instruction.PUSHIB)) 
         && (currentStream[lastInstruction0] == byte(Instruction.BITSHR))
           )
        {
            byte value = currentStream[lastInstruction1+1];
            if (value == 8)
            {
                currentStream.SetItem(lastInstruction1, byte(Instruction.BITSHR8));
                popAndTrim(currentStream, 1, 2);
                return true; // hunt for more
            }
            else
            {
                // PUSHIB BITSHR -> BITSHRB
                // i1     i0     -> i0
                currentStream.SetItem(lastInstruction1, byte(Instruction.BITSHRB));
                popAndTrim(currentStream, 1, 1);
                return true; // hunt for more
            }
        }
        if (
            (currentStream[lastInstruction1] == byte(Instruction.PUSHI1)) 
         && (currentStream[lastInstruction0] == byte(Instruction.BITSHR))
           )
        {
            // PUSHI1 BITSHR -> BITSHRB
            // i1     i0     -> i0
            currentStream.SetItem(lastInstruction1, byte(Instruction.BITSHRB));
            currentStream.SetItem(lastInstruction0, byte(1));
            popAndTrim(currentStream, 1, 0);
            return true; // hunt for more
        }
        if (
            (currentStream[lastInstruction1] == byte(Instruction.PUSHIB)) 
         && (currentStream[lastInstruction0] == byte(Instruction.BITAND))
           )
        {
            byte value = currentStream[lastInstruction1+1];
            if (value == 0xFF)
            {
                currentStream.SetItem(lastInstruction1, byte(Instruction.BITANDFF));
                popAndTrim(currentStream, 1, 2);
                return true; // hunt for more
            }
            else
            {
                // PUSHIB BITAND -> BITANDB
                // i1     i0     -> i0
                currentStream.SetItem(lastInstruction1, byte(Instruction.BITANDB));
                popAndTrim(currentStream, 1, 1);
                return true; // hunt for more
            }
        }
        if (
            (currentStream[lastInstruction1] == byte(Instruction.PUSHI1)) 
         && (currentStream[lastInstruction0] == byte(Instruction.BITAND))
           )
        {
            // PUSHI1 BITAND -> BITANDB
            // i1     i0     -> i0
            currentStream.SetItem(lastInstruction1, byte(Instruction.BITANDB));
            currentStream.SetItem(lastInstruction0, byte(1));
            popAndTrim(currentStream, 1, 0);
            return true; // hunt for more
        }
        if (
            (currentStream[lastInstruction1] == byte(Instruction.PUSHIB)) 
         && (currentStream[lastInstruction0] == byte(Instruction.BITOR))
           )
        {
            byte value = currentStream[lastInstruction1+1];
            // PUSHIB BITOR -> BITORB
            // i1     i0    -> i0
            currentStream.SetItem(lastInstruction1, byte(Instruction.BITORB));
            popAndTrim(currentStream, 1, 1);
            return true; // hunt for more
        }
        if (
            (currentStream[lastInstruction1] == byte(Instruction.PUSHI1)) 
         && (currentStream[lastInstruction0] == byte(Instruction.BITOR))
           )
        {
            // PUSHI1 BITOR -> BITORB
            // i1     i0     -> i0
            currentStream.SetItem(lastInstruction1, byte(Instruction.BITORB));
            currentStream.SetItem(lastInstruction0, byte(1));
            popAndTrim(currentStream, 1, 0);
            return true; // hunt for more
        }
        if (
            (currentStream[lastInstruction1] == byte(Instruction.PUSHI)) 
         && (currentStream[lastInstruction0] == byte(Instruction.LE))
           )
        {
            // PUSHI LE -> PUSHILE
            // i1     i0 -> i0
            currentStream.SetItem(lastInstruction1, byte(Instruction.PUSHILE));
            popAndTrim(currentStream, 1, 1);
            return true; // hunt for more
        }
        if (
            (currentStream[lastInstruction1] == byte(Instruction.PUSHIB)) 
         && (currentStream[lastInstruction0] == byte(Instruction.LE))
           )
        {
            // PUSHIB LE -> PUSHIBLE
            // i1     i0 -> i0
            currentStream.SetItem(lastInstruction1, byte(Instruction.PUSHIBLE));
            popAndTrim(currentStream, 1, 1);
            return true; // hunt for more
        }
        if (
            (currentStream[lastInstruction1] == byte(Instruction.PUSHIB)) 
         && (currentStream[lastInstruction0] == byte(Instruction.EQ))
           )
        {
            // PUSHIB EQ -> PUSHIBEQ
            // i1     i0 -> i0
            currentStream.SetItem(lastInstruction1, byte(Instruction.PUSHIBEQ));
            popAndTrim(currentStream, 1, 1);
            return true; // hunt for more
        }
        if (
            (currentStream[lastInstruction1] == byte(Instruction.PUSHIB)) 
         && (currentStream[lastInstruction0] == byte(Instruction.ADD))
           )
        {
            // PUSHIB ADD -> ADDB
            // i1     i0  -> i0
            currentStream.SetItem(lastInstruction1, byte(Instruction.ADDB));
            popAndTrim(currentStream, 1, 1);
            return true; // hunt for more
        }
        if (
            (currentStream[lastInstruction1] == byte(Instruction.PUSHIB)) 
         && (currentStream[lastInstruction0] == byte(Instruction.SUB))
           )
        {
            // PUSHIB SUB -> SUBB
            // i1     i0  -> i0
            currentStream.SetItem(lastInstruction1, byte(Instruction.SUBB));
            popAndTrim(currentStream, 1, 1);
            return true; // hunt for more
        }
        if (
            (currentStream[lastInstruction1] == byte(Instruction.PUSHI)) 
         && (currentStream[lastInstruction0] == byte(Instruction.LEI))
           )
        {
            // PUSHI LEI -> PUSHILEI
            // i1     i0 -> i0
            currentStream.SetItem(lastInstruction1, byte(Instruction.PUSHILEI));
            popAndTrim(currentStream, 1, 1);
            return true; // hunt for more
        }
        if (
            (currentStream[lastInstruction1] == byte(Instruction.PUSHI)) 
         && (currentStream[lastInstruction0] == byte(Instruction.LT))
           )
        {
            // PUSHI LT -> PUSHILT
            // i1     i0 -> i0
            currentStream.SetItem(lastInstruction1, byte(Instruction.PUSHILT));
            popAndTrim(currentStream, 1, 1);
            return true; // hunt for more
        }
        if (
            (currentStream[lastInstruction1] == byte(Instruction.PUSHGLOBALB)) 
         && (currentStream[lastInstruction0] == byte(Instruction.PUSHGLOBALB))
           )
        {
            // PUSHGLOBALB PUSHGLOBALB -> PUSHGLOBALBB
            // i1          i0          -> i0
            byte offset0 = currentStream[lastInstruction0+1];   
            byte offset1 = currentStream[lastInstruction1+1];   
            currentStream.SetItem(lastInstruction1, byte(Instruction.PUSHGLOBALBB));
            currentStream.SetItem(lastInstruction1+2, offset0);
            popAndTrim(currentStream, 1, 1);
            return true; // hunt for more
        }
    
        if (
            (currentStream[lastInstruction1] == byte(Instruction.PUSHLOCALB)) 
         && (currentStream[lastInstruction0] == byte(Instruction.PUSHLOCALB))
           )
        {
            // PUSHLOCALB PUSHLOCALB -> PUSHLOCALBB
            // i1         i0         -> i0
            byte offset0 = currentStream[lastInstruction0+1];   
            byte offset1 = currentStream[lastInstruction1+1];   
            currentStream.SetItem(lastInstruction1, byte(Instruction.PUSHLOCALBB));
            currentStream.SetItem(lastInstruction1+2, offset0);
            popAndTrim(currentStream, 1, 1);
            return true; // hunt for more
        }
        return false;
    }
    
    
}
