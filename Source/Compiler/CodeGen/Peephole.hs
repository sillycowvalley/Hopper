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
    
    TrimTail(<byte> currentStream, uint remove)
    {
        while (remove > 0)
        {
            currentStream.Remove(currentStream.Count-1);
            remove--;
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
                length = 1;    
                return true;
            }
            case Instruction.PUSHLOCALB01:
            {
                offset = 1;   
                length = 1;    
                return true;
            }
        }
        return false;
    }
    
    bool IsPopLocalB(<byte> currentStream, uint index, ref byte offset, ref byte length)
    {
        Instruction opCode = Instruction(currentStream[index]);
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
                length = 1; 
                return true;
            }
            case Instruction.POPLOCALB01:
            {
                offset = 1;
                length = 1; 
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
            TrimTail(currentStream, 2);
            lastInstruction0 = lastInstruction2;
            lastInstruction1 = lastInstruction3;
            lastInstruction2 = lastInstruction4;
            lastInstruction3 = 0;
            lastInstruction4 = 0;
            return true; // hunt for more
        }
        if (currentStream[lastInstruction2] == byte(Instruction.PUSHLOCALBB))
        {
            byte offset2 = currentStream[lastInstruction2+1]; 
            byte offset1 = currentStream[lastInstruction2+2]; 
            byte length1 = 1;
            
            byte offset0 = 0;
            byte length0 = 0;
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
                    TrimTail(currentStream, remove);
                    lastInstruction0 = lastInstruction2;
                    lastInstruction1 = lastInstruction3;
                    lastInstruction2 = lastInstruction4;
                    lastInstruction3 = 0;
                    lastInstruction4 = 0;
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
                    TrimTail(currentStream, remove);
                    lastInstruction0 = lastInstruction2;
                    lastInstruction1 = lastInstruction3;
                    lastInstruction2 = lastInstruction4;
                    lastInstruction3 = 0;
                    lastInstruction4 = 0;
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
                TrimTail(currentStream, remove);
                lastInstruction0 = lastInstruction3;
                lastInstruction1 = lastInstruction4;
                lastInstruction2 = 0;
                lastInstruction3 = 0;
                lastInstruction4 = 0;
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
                TrimTail(currentStream, remove);
                lastInstruction0 = lastInstruction3;
                lastInstruction1 = lastInstruction4;
                lastInstruction2 = 0;
                lastInstruction3 = 0;
                lastInstruction4 = 0;
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
                TrimTail(currentStream, remove);
                lastInstruction0 = lastInstruction3;
                lastInstruction1 = lastInstruction4;
                lastInstruction2 = 0;
                lastInstruction3 = 0;
                lastInstruction4 = 0;
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
                TrimTail(currentStream, remove);
                lastInstruction0 = lastInstruction3;
                lastInstruction1 = lastInstruction4;
                lastInstruction2 = 0;
                lastInstruction3 = 0;
                lastInstruction4 = 0;
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
                TrimTail(currentStream, remove);
                lastInstruction0 = lastInstruction3;
                lastInstruction1 = lastInstruction4;
                lastInstruction2 = 0;
                lastInstruction3 = 0;
                lastInstruction4 = 0;
                return true; // hunt for more
            }
        }
        return false;   
    }
   	
    bool peepholeOptimize1(<byte> currentStream)
    {
        if (currentStream[lastInstruction0] == byte(Instruction.PUSHI))
        {
            uint operand = currentStream[lastInstruction0+1] + (currentStream[lastInstruction0+2] << 8);
            if (operand <= 255)
            {
                currentStream.SetItem(lastInstruction0, byte(Instruction.PUSHIB));
                TrimTail(currentStream, 1); // MSB
                return true; // hunt for more
            }
        }
        if (currentStream[lastInstruction0] == byte(Instruction.PUSHIB))
        {
            byte operand = currentStream[lastInstruction0+1];
            if (operand == 0)
            {
                currentStream.SetItem(lastInstruction0, byte(Instruction.PUSHI0));
                TrimTail(currentStream, 1); // '0'
                return true; // hunt for more
            }
            if (operand == 1)
            {
                currentStream.SetItem(lastInstruction0, byte(Instruction.PUSHI1));
                TrimTail(currentStream, 1); // '1'
                return true; // hunt for more
            }
        }
        
        byte offset0 = 0;
        byte length0 = 0;
        bool isPushLocalB = IsPushLocalB(currentStream, lastInstruction0, ref offset0, ref length0);
        
        if (isPushLocalB && (length0 == 2) && ((offset0 == 0) || (offset0 == 1)))
        {
            if (offset0 == 0)
            {
                // PUSHLOCALB -> PUSHLOCALB00
                // i0         -> i0
                currentStream.SetItem(lastInstruction0, byte(Instruction.PUSHLOCALB00));
            }
            if (offset0 == 1)
            {
                // PUSHLOCALB -> PUSHLOCALB01
                // i0         -> i0
                currentStream.SetItem(lastInstruction0, byte(Instruction.PUSHLOCALB01));
            }
            TrimTail(currentStream, 1);
            return true; // hunt for more
        }
        
        bool isPopLocalB = IsPopLocalB(currentStream, lastInstruction0, ref offset0, ref length0);
        if (isPopLocalB && (length0 == 2) && ((offset0 == 0) || (offset0 == 1)))
        {
            if (offset0 == 0)
            {
                // POPLOCALB -> POPLOCALB00
                // i0         -> i0
                currentStream.SetItem(lastInstruction0, byte(Instruction.POPLOCALB00));
            }
            if (offset0 == 1)
            {
                // POPLOCALB -> POPLOCALB01
                // i0         -> i0
                currentStream.SetItem(lastInstruction0, byte(Instruction.POPLOCALB01));
            }
            TrimTail(currentStream, 1);
            return true; // hunt for more
        }
        return false;
    }
    
    bool peepholeOptimize2(<byte> currentStream)
    {    
        if (
            (currentStream[lastInstruction1] == byte(Instruction.PUSHIB)) 
         && (currentStream[lastInstruction0] == byte(Instruction.BITSHL))
           )
        {
            byte value = currentStream[lastInstruction1+1];
            if (value == 8)
            {
                currentStream.SetItem(lastInstruction1, byte(Instruction.BITSHL8));
                TrimTail(currentStream, 2);
                lastInstruction0 = lastInstruction1;
                lastInstruction1 = lastInstruction2;
                lastInstruction2 = lastInstruction3;
                lastInstruction3 = lastInstruction4;
                lastInstruction4 = 0;
                return true; // hunt for more
            }
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
                TrimTail(currentStream, 2);
                lastInstruction0 = lastInstruction1;
                lastInstruction1 = lastInstruction2;
                lastInstruction2 = lastInstruction3;
                lastInstruction3 = lastInstruction4;
                lastInstruction4 = 0;
                return true; // hunt for more
            }
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
                TrimTail(currentStream, 2);
                lastInstruction0 = lastInstruction1;
                lastInstruction1 = lastInstruction2;
                lastInstruction2 = lastInstruction3;
                lastInstruction3 = lastInstruction4;
                lastInstruction4 = 0;
                return true; // hunt for more
            }
            
        }
        if (
            (currentStream[lastInstruction1] == byte(Instruction.PUSHI)) 
         && (currentStream[lastInstruction0] == byte(Instruction.LE))
           )
        {
            // PUSHI LE -> PUSHILE
            // i1     i0 -> i0
            currentStream.SetItem(lastInstruction1, byte(Instruction.PUSHILE));
            TrimTail(currentStream, 1);
            lastInstruction0 = lastInstruction1;
            lastInstruction1 = lastInstruction2;
            lastInstruction2 = lastInstruction3;
            lastInstruction3 = lastInstruction4;
            lastInstruction4 = 0;
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
            TrimTail(currentStream, 1);
            lastInstruction0 = lastInstruction1;
            lastInstruction1 = lastInstruction2;
            lastInstruction2 = lastInstruction3;
            lastInstruction3 = lastInstruction4;
            lastInstruction4 = 0;
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
            TrimTail(currentStream, 1);
            lastInstruction0 = lastInstruction1;
            lastInstruction1 = lastInstruction2;
            lastInstruction2 = lastInstruction3;
            lastInstruction3 = lastInstruction4;
            lastInstruction4 = 0;
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
            TrimTail(currentStream, 1);
            lastInstruction0 = lastInstruction1;
            lastInstruction1 = lastInstruction2;
            lastInstruction2 = lastInstruction3;
            lastInstruction3 = lastInstruction4;
            lastInstruction4 = 0;
            return true; // hunt for more
        }
        if (
            (currentStream[lastInstruction1] == byte(Instruction.PUSHIB)) 
         && (currentStream[lastInstruction0] == byte(Instruction.SUB))
           )
        {
            // PUSHIB ADD -> ADDB
            // i1     i0  -> i0
            currentStream.SetItem(lastInstruction1, byte(Instruction.SUBB));
            TrimTail(currentStream, 1);
            lastInstruction0 = lastInstruction1;
            lastInstruction1 = lastInstruction2;
            lastInstruction2 = lastInstruction3;
            lastInstruction3 = lastInstruction4;
            lastInstruction4 = 0;
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
            TrimTail(currentStream, 1);
            lastInstruction0 = lastInstruction1;
            lastInstruction1 = lastInstruction2;
            lastInstruction2 = lastInstruction3;
            lastInstruction3 = lastInstruction4;
            lastInstruction4 = 0;
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
            TrimTail(currentStream, 1);
            lastInstruction0 = lastInstruction1;
            lastInstruction1 = lastInstruction2;
            lastInstruction2 = lastInstruction3;
            lastInstruction3 = lastInstruction4;
            lastInstruction4 = 0;
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
            
            TrimTail(currentStream, 1);
            lastInstruction0 = lastInstruction1;
            lastInstruction1 = lastInstruction2;
            lastInstruction2 = lastInstruction3;
            lastInstruction3 = lastInstruction4;
            lastInstruction4 = 0;
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
            
            TrimTail(currentStream, 1);
            lastInstruction0 = lastInstruction1;
            lastInstruction1 = lastInstruction2;
            lastInstruction2 = lastInstruction3;
            lastInstruction3 = lastInstruction4;
            lastInstruction4 = 0;
            return true; // hunt for more
        }
        return false;
    }
    
    
}
