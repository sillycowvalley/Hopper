unit TCGen
{
    uses "TCCode"
    uses "TCType"
    uses "TCExpression"
    
    record Instruction
    {
        string Name;
        bool   IsByte;
        uint   Operand;
        int    Offset; // used by CALL, PUSHM, POPM as bool
        int    Offset2;
        string Data;
    }
    int  nestedStreamMode;
    bool capturingMode;
    bool enablePeephole;
    
    // Instructions:
    //
    //    PUSHL isByte, offset
    //    POPL  isByte, offset
    //    PUSHG isByte, offset
    //    POPG  isByte, offset
    //    PUSHM isByte ([top] could be byte or word address)
    //    POPM  isByte ([next] could be byte or word address)
    //    PUSHI isByte operand
    //    ZEROG isByte offset
    //    PUSHC address (address needs to be added to Resources.StrConsts)
    //
    //    PADUNDER (cast [next] to word by inserting a zero MSB (assuming [top] is a word))
    //
    //    REM data  (comment)
    //
    //    CALL data (function name)
    //    DECSP operand
    //
    //    IF      // if (NZ) {
    //    ELSE    // } else {
    //    ENDIF   // }
    //    LOOPEXIT // PLA if (Z) { break; }
    //
    //    DUP isByte
    //    ADD isByte
    //    SUB isByte
    //    MUL isByte
    //    DIV isByte
    //    MOD isByte
    //    MULI
    //    DIVI
    //    MODI
    //
    //    SHL isByte
    //    SHR isByte
    //    AND isByte
    //    OR  isByte
    //    XOR isByte
    //
    //    NOT isByte
    //    BOOLNOT
    //
    //    EQ isByte
    //    NE isByte
    //    LT isByte
    //    LE isByte
    //    GT isByte
    //    GE isByte
    //    LTI
    //    LEI
    //    GTI
    //    GEI
    //
    // Instructions that block/reset the peephole optimizer:
    //    IF, ELSE, ENDIF, CALL
    //
    // 'Packed' peephole optimization opcodes:
    //
    //    LLADD   : PUSHL offset0, PUSHL offset1, ADD  (offset0 may be the same as offset1)
    //    2L:     : PUSHL offset0, PUSHL offset1, ADD  (offset0 == offset1)
    //    GGADD   : PUSHG offset0, PUSHG offset1, ADD  (offset0 may be the same as offset1)
    //    2G:     : PUSHG offset0, PUSHG offset1, ADD  (offset0 == offset1)
    //    LGADD   : PUSHL offset0, PUSHG offset1, ADD
    //    LGADDM  : PUSHL offset0, PUSHG offset1, ADD PUSHMB
    //    GGADDM  : PUSHG offset0, PUSHG offset1, ADD PUSHMB
    //    LGADDI  : PUSHL offset0, PUSHG offset1, ADD PUSHIB
    //    GGADDI  : PUSHL offset0, PUSHG offset1, ADD PUSHIB
    //    LGADDIM : PUSHL offset0, PUSHG offset1, ADD PUSHIB POPMB
    //    GGADDIM : PUSHL offset0, PUSHG offset1, ADD PUSHIB POPMB
    //    LLADDL  : PUSHL offset0, PUSHL offset1, ADD, POPL offset0 (offset0 may be the same as offset1)
    //    GGADDG  : PUSHG offset0, PUSHG offset1, ADD, POPG offset0 (offset0 may be the same as offset1)
    //    INCLI   : PUSHL offset0, PUSHI, ADD, POPL offset0
    //    INCGI   : PUSHG offset0, PUSHI, ADD, POPG offset0
    //    STLI    : PUSHI, POPL offset0
    //    LISHR   : PUSHL offset, PUSHI, SHR
    //    LISHR8  : PUSHL offset, PUSHI 0x0008, SHR
    //    LISHL   : PUSHL offset, PUSHI, SHL
    //    LIAND   : PUSHL offset, PUSHI, AND
    //    LIANDFF : PUSHL offset, PUSHI 0x00FF, AND
    //    ILADD   : PUSHI, PUSHL offset, ADD
    //    ILSUB   : PUSHI, PUSHL offset, SUB
    //    IADD    : PUSHI, ADD
    //    IADDL   : PUSHI, ADD, POPL offset0
    //    LIADD   : PUSHL offset, PUSHI, ADD
    //    GIADD   : PUSHG offset, PUSHI, ADD
    //    LISUB   : PUSHL offset, PUSHI, SUB
    //    LTX     : LT, LOOPEXIT
    //    LEX     : LE, LOOPEXIT
    //    LILE    : PUSHL offset, PUSHI, LE
    //    LILT    : PUSHL offset, PUSHI, LT
    //    GILT    : PUSHG offset, PUSHI, LT
    //    LIGTI   : PUSHL offset, PUSHI, GTI
    //    LILEX   : PUSHL offset, PUSHI, LE, LOOPEXIT
    //    LILTX   : PUSHL offset, PUSHI, LT, LOOPEXIT
    //    GILTX   : PUSHG offset, PUSHI, LT, LOOPEXIT
    //
    
     
    <Instruction> currentStream;
    
    DeleteInstruction(uint index)
    {
        currentStream.Remove(index);
    }
    
    bool OptimizeTwo()
    {
        bool modified;
        Instruction instruction1 = currentStream[currentStream.Count-2];
        Instruction instruction0 = currentStream[currentStream.Count-1];
        
        loop
        {
            if ((instruction0.Name == "LOOPEXIT"))
            {
                if (instruction1.Name == "LE") 
                {
                    //Print(" LEX");
                    instruction1.Name    = "LEX";
                    instruction1.Data = instruction0.Data;
                    currentStream[currentStream.Count-2] = instruction1;
                    DeleteInstruction(currentStream.Count-1);
                    modified = true;
                    break;
                }
                if (instruction1.Name == "LT") 
                {
                    //Print(" LTX");
                    instruction1.Name    = "LTX";
                    instruction1.Data = instruction0.Data;
                    currentStream[currentStream.Count-2] = instruction1;
                    DeleteInstruction(currentStream.Count-1);
                    modified = true;
                    break;
                }
                if (instruction1.Name == "LILE") 
                {
                    //Print(" LILEX");
                    instruction1.Name    = "LILEX";
                    instruction1.Data = instruction0.Data;
                    currentStream[currentStream.Count-2] = instruction1;
                    DeleteInstruction(currentStream.Count-1);
                    modified = true;
                    break;
                }    
                if (instruction1.Name == "LILT")
                {
                    //Print(" LILTX");
                    instruction1.Name    = "LILTX";
                    instruction1.Data = instruction0.Data;
                    currentStream[currentStream.Count-2] = instruction1;
                    DeleteInstruction(currentStream.Count-1);
                    modified = true;
                    break;
                }
                if (instruction1.Name == "GILT")
                {
                    //Print(" GILTX");
                    instruction1.Name    = "GILTX";
                    instruction1.Data = instruction0.Data;
                    currentStream[currentStream.Count-2] = instruction1;
                    DeleteInstruction(currentStream.Count-1);
                    modified = true;
                    break;
                }
            }
            if ((instruction0.Name == "ADD"))
            {
                if (instruction1.IsByte == instruction0.IsByte)
                {
                    if (instruction1.Name == "PUSHI")
                    {
                        //Print(" IADD");
                        instruction1.Name    = "IADD";
                        currentStream[currentStream.Count-2] = instruction1;
                        DeleteInstruction(currentStream.Count-1);
                        modified = true;
                        break;
                    }    
                }
            }
            if ((instruction0.Name == "IADD"))
            {
                if (instruction1.IsByte == instruction0.IsByte)
                {
                    if (instruction1.Name == "PUSHL")
                    {
                        //Print(" LIADD");
                        instruction1.Name    = "LIADD";
                        instruction1.Operand = instruction0.Operand;
                        currentStream[currentStream.Count-2] = instruction1;
                        DeleteInstruction(currentStream.Count-1);
                        modified = true;
                        break;
                    }  
                    if (instruction1.Name == "PUSHG")
                    {
                        //Print(" GIADD");
                        instruction1.Name    = "GIADD";
                        instruction1.Operand = instruction0.Operand;
                        currentStream[currentStream.Count-2] = instruction1;
                        DeleteInstruction(currentStream.Count-1);
                        modified = true;
                        break;
                    }   
                }
            }
            if ((instruction0.Name == "POPL"))
            {
                if (instruction1.IsByte == instruction0.IsByte)
                {
                    if (instruction1.Name == "IADD")
                    {
                        //Print(" IADDL");
                        instruction1.Name    = "IADDL";
                        instruction1.Offset = instruction0.Offset;
                        currentStream[currentStream.Count-2] = instruction1;
                        DeleteInstruction(currentStream.Count-1);
                        modified = true;
                        break;
                    }    
                }
            }
            if ((instruction0.Name == "PUSHI"))
            {
                if (instruction0.IsByte)
                { 
                    if (instruction1.Name == "PUSHI")
                    {
                        if (instruction1.IsByte)
                        {
                            instruction0.Operand = (instruction0.Operand << 8) + instruction1.Operand;
                            instruction0.IsByte = false;
                            currentStream[currentStream.Count-2] = instruction0;
                            DeleteInstruction(currentStream.Count-1);
                            modified = true;
                            break;
                        }
                    }
                    if (!instruction1.IsByte)
                    {
                        if (instruction1.Name == "LGADD")
                        {
                            //Print(" LGADDI");
                            instruction1.Name    = "LGADDI";
                            instruction1.Operand = instruction0.Operand;
                            currentStream[currentStream.Count-2] = instruction1;
                            DeleteInstruction(currentStream.Count-1);
                            modified = true;
                            break;
                        }
                        if (instruction1.Name == "GGADD")
                        {
                            //Print(" GGADDI");
                            instruction1.Name    = "GGADDI";
                            instruction1.Operand = instruction0.Operand;
                            currentStream[currentStream.Count-2] = instruction1;
                            DeleteInstruction(currentStream.Count-1);
                            modified = true;
                            break;
                        }
                    }
                }
            }
            if ((instruction0.Name == "POPL"))
            {
                if (instruction0.IsByte == instruction1.IsByte)
                {
                    if (instruction1.Name == "PUSHI") 
                    {
                        //Print(" STLI");
                        instruction0.Operand = instruction1.Operand;
                        instruction0.Name = "STLI";
                        currentStream[currentStream.Count-1] = instruction0;
                        DeleteInstruction(currentStream.Count-2);
                        modified = true;
                        break;
                    }
                    if ((instruction1.Name == "LLADD") && (instruction1.Offset == instruction0.Offset))
                    {   
                        //Print(" LLADDL");
                        instruction1.Name    = "LLADDL";
                        currentStream[currentStream.Count-2] = instruction1;
                        DeleteInstruction(currentStream.Count-1);
                        modified = true;
                        break;
                    }
                    if ((instruction1.Name == "ILADD") && (instruction1.Offset == instruction0.Offset))
                    {   
                        //Print(" INCLI");
                        instruction1.Name    = "INCLI";
                        currentStream[currentStream.Count-2] = instruction1;
                        DeleteInstruction(currentStream.Count-1);
                        modified = true;
                        break;
                    }
                    if ((instruction1.Name == "LIADD") && (instruction1.Offset == instruction0.Offset))
                    {
                        //Print(" INCLI");   
                        instruction1.Name    = "INCLI";
                        currentStream[currentStream.Count-2] = instruction1;
                        DeleteInstruction(currentStream.Count-1);
                        modified = true;
                        break;
                    }
                }
            }
            if ((instruction0.Name == "POPG"))
            {
                if (instruction0.IsByte == instruction1.IsByte)
                {
                    if ((instruction1.Name == "IGADD") && (instruction1.Offset == instruction0.Offset))
                    {   
                        //Print(" INCGI");
                        instruction1.Name    = "INCGI";
                        currentStream[currentStream.Count-2] = instruction1;
                        DeleteInstruction(currentStream.Count-1);
                        modified = true;
                        break;
                    }
                    if ((instruction1.Name == "GIADD") && (instruction1.Offset == instruction0.Offset))
                    {
                        //Print(" INCGI");   
                        instruction1.Name    = "INCGI";
                        currentStream[currentStream.Count-2] = instruction1;
                        DeleteInstruction(currentStream.Count-1);
                        modified = true;
                        break;
                    }
                    if (instruction1.Name == "GGADD")
                    {   
                        //Print(" GGADDG");
                        instruction1.Name    = "GGADDG";
                        instruction1.Operand = UInt.FromBytes((instruction0.Offset).GetByte(0), (instruction0.Offset).GetByte(1));
                        currentStream[currentStream.Count-2] = instruction1;
                        DeleteInstruction(currentStream.Count-1);
                        modified = true;
                        break;
                    }
                }
            }
            if ((instruction0.Name == "PUSHM"))
            {
                if (instruction0.IsByte && !instruction1.IsByte)
                {
                    if (instruction1.Name == "LGADD")
                    {
                        //Print(" LGADDM");
                        instruction1.Name    = "LGADDM";
                        currentStream[currentStream.Count-2] = instruction1;
                        DeleteInstruction(currentStream.Count-1);
                        modified = true;
                        break;
                    }
                }
            }
            if ((instruction0.Name == "PUSHM"))
            {
                if (instruction0.IsByte && !instruction1.IsByte)
                {
                    if (instruction1.Name == "GGADD")
                    {
                        //Print(" GGADDM");
                        instruction1.Name    = "GGADDM";
                        currentStream[currentStream.Count-2] = instruction1;
                        DeleteInstruction(currentStream.Count-1);
                        modified = true;
                        break;
                    }
                }
            }
            
            if (instruction0.Name == "POPM")
            {
                if (instruction0.IsByte && !instruction1.IsByte)
                {
                    if (instruction1.Name == "LGADDI")
                    {
                        //Print(" LGADDIM");
                        instruction1.Name    = "LGADDIM";
                        currentStream[currentStream.Count-2] = instruction1;
                        DeleteInstruction(currentStream.Count-1);
                        modified = true;
                        break;
                    }
                    if (instruction1.Name == "GGADDI")
                    {
                        //Print(" GGADDIM");
                        instruction1.Name    = "GGADDIM";
                        currentStream[currentStream.Count-2] = instruction1;
                        DeleteInstruction(currentStream.Count-1);
                        modified = true;
                        break;
                    }
                }
            }
            break;
        } // loop
        return modified;
    }
    
    bool OptimizeThree()
    {
        bool modified;
        Instruction instruction2 = currentStream[currentStream.Count-3];
        Instruction instruction1 = currentStream[currentStream.Count-2];
        Instruction instruction0 = currentStream[currentStream.Count-1];
        loop
        {
            if ((instruction2.Name == "PUSHL") && (instruction1.Name == "INCLI") && (instruction0.Name == "DECSP"))
            { 
                if (instruction2.IsByte && instruction1.IsByte && (instruction0.Operand == 1))
                {          
                    DeleteInstruction(currentStream.Count-3);
                    DeleteInstruction(currentStream.Count-1);
                    modified = true;
                    break;
                }
                if (!instruction2.IsByte && !instruction1.IsByte && (instruction0.Operand == 2))
                {
                    DeleteInstruction(currentStream.Count-3);
                    DeleteInstruction(currentStream.Count-1);
                    modified = true;
                    break;
                }
            }
            if ((instruction2.Name == "PUSHG") && (instruction1.Name == "INCGI") && (instruction0.Name == "DECSP"))
            { 
                if (instruction2.IsByte && instruction1.IsByte && (instruction0.Operand == 1))
                {          
                    DeleteInstruction(currentStream.Count-3);
                    DeleteInstruction(currentStream.Count-1);
                    modified = true;
                    break;
                }
                if (!instruction2.IsByte && !instruction1.IsByte && (instruction0.Operand == 2))
                {
                    DeleteInstruction(currentStream.Count-3);
                    DeleteInstruction(currentStream.Count-1);
                    modified = true;
                    break;
                }
            }
            if ((instruction2.Name == "PUSHL") && (instruction1.Name == "PUSHL"))
            {
                if (instruction0.Name == "ADD")
                {
                    if ((instruction2.IsByte == instruction1.IsByte) && (instruction1.IsByte == instruction0.IsByte))
                    {                        
                        if (instruction2.Offset == instruction1.Offset)
                        {
                            //Print(" 2L3");
                            instruction2.Name    = "2L";
                            currentStream[currentStream.Count-3] = instruction2;
                            DeleteInstruction(currentStream.Count-1);
                            DeleteInstruction(currentStream.Count-1);
                            modified = true;
                            break;
                        }
                        else
                        {
                            //Print(" LLADD3");
                            instruction2.Offset2 = instruction1.Offset;
                            instruction2.Name    = "LLADD";
                            currentStream[currentStream.Count-3] = instruction2;
                            DeleteInstruction(currentStream.Count-1);
                            DeleteInstruction(currentStream.Count-1);
                            modified = true;
                            break;
                        }
                    }
                }
            }
            if ((instruction2.Name == "PUSHG") && (instruction1.Name == "PUSHG"))
            {
                if (instruction0.Name == "ADD")
                {
                    if ((instruction2.IsByte == instruction1.IsByte) && (instruction1.IsByte == instruction0.IsByte))
                    {                        
                        if (instruction2.Offset == instruction1.Offset)
                        {
                            //Print(" 2G3");
                            instruction2.Name    = "2G";
                            currentStream[currentStream.Count-3] = instruction2;
                            DeleteInstruction(currentStream.Count-1);
                            DeleteInstruction(currentStream.Count-1);
                            modified = true;
                            break;
                        }
                        else
                        {
                            //Print(" GGADD3");
                            instruction2.Offset2 = instruction1.Offset;
                            instruction2.Name    = "GGADD";
                            currentStream[currentStream.Count-3] = instruction2;
                            DeleteInstruction(currentStream.Count-1);
                            DeleteInstruction(currentStream.Count-1);
                            modified = true;
                            break;
                        }
                    }
                }
            }
            if ((instruction2.Name == "PUSHL") && (instruction1.Name == "PUSHG") && (instruction0.Name == "ADD"))
            {
                if ((instruction2.IsByte == instruction1.IsByte) && (instruction1.IsByte == instruction0.IsByte))
                {
                    //Print(" LGADD3");
                    instruction2.Offset2 = instruction1.Offset;
                    instruction2.Name    = "LGADD";
                    currentStream[currentStream.Count-3] = instruction2;
                    DeleteInstruction(currentStream.Count-1);
                    DeleteInstruction(currentStream.Count-1);
                    modified = true;
                    break;
                }
            }
            if ((instruction2.Name == "PUSHG") && (instruction1.Name == "PUSHI"))
            {
                if (instruction2.IsByte == instruction1.IsByte)
                {
                    if (instruction1.IsByte == instruction0.IsByte)
                    {
                        if (instruction0.Name == "LT")
                        {
                            //Print(" GILT3");
                            instruction2.Operand = instruction1.Operand;
                            instruction2.Name = "GILT";
                            currentStream[currentStream.Count-3] = instruction2;
                            DeleteInstruction(currentStream.Count-2);
                            DeleteInstruction(currentStream.Count-1);
                            modified = true;
                            break;
                        }
                    }
                }
            }
            if ((instruction2.Name == "PUSHL") && (instruction1.Name == "PUSHI"))
            {
                if (instruction2.IsByte == instruction1.IsByte)
                {
                    if (instruction1.IsByte == instruction0.IsByte)
                    {
                        if (instruction0.Name == "LT")
                        {
                            //Print(" LILT3");
                            instruction2.Operand = instruction1.Operand;
                            instruction2.Name = "LILT";
                            currentStream[currentStream.Count-3] = instruction2;
                            DeleteInstruction(currentStream.Count-2);
                            DeleteInstruction(currentStream.Count-1);
                            modified = true;
                            break;
                        }
                        if (instruction0.Name == "LE")
                        {
                            //Print(" LILE3");
                            instruction2.Operand = instruction1.Operand;
                            instruction2.Name = "LILE";
                            currentStream[currentStream.Count-3] = instruction2;
                            DeleteInstruction(currentStream.Count-2);
                            DeleteInstruction(currentStream.Count-1);
                            modified = true;
                            break;
                        }
                    }
                    if (instruction0.Name == "AND")
                    {
                        if (instruction1.Operand == 0x00FF)
                        {
                            //Print(" LIANDFF3");
                            instruction2.Name = "LIANDFF";
                            currentStream[currentStream.Count-3] = instruction2;
                            DeleteInstruction(currentStream.Count-2);
                            DeleteInstruction(currentStream.Count-1);
                        }
                        else
                        {
                            //Print(" LIAND3");
                            instruction2.Operand = instruction1.Operand;
                            instruction2.Name = "LIAND";
                            currentStream[currentStream.Count-3] = instruction2;
                            DeleteInstruction(currentStream.Count-2);
                            DeleteInstruction(currentStream.Count-1);
                        }
                        modified = true;
                        break;
                    }
                    if (instruction0.Name == "SHR")
                    {
                        if (instruction1.Operand == 0x0008)
                        {
                            //Print(" LISHR83");
                            instruction2.Name = "LISHR8";
                            currentStream[currentStream.Count-3] = instruction2;
                            DeleteInstruction(currentStream.Count-2);
                            DeleteInstruction(currentStream.Count-1);
                        }
                        else
                        {
                            //Print(" LISHR3");
                            instruction2.Operand = instruction1.Operand;
                            instruction2.Name = "LISHR";
                            currentStream[currentStream.Count-3] = instruction2;
                            DeleteInstruction(currentStream.Count-2);
                            DeleteInstruction(currentStream.Count-1);
                        }
                        modified = true;
                        break;
                    }
                    if (instruction0.Name == "SHL")
                    {    
                        if (instruction1.Operand == 0x0008)
                        {
                            Print(" LISHL83");
                            instruction2.Name = "LISHL8";
                            currentStream[currentStream.Count-3] = instruction2;
                            DeleteInstruction(currentStream.Count-2);
                            DeleteInstruction(currentStream.Count-1);
                        }
                        else
                        {
                            Print(" LISHL3");
                            instruction2.Operand = instruction1.Operand;
                            instruction2.Name = "LISHL";
                            currentStream[currentStream.Count-3] = instruction2;
                            DeleteInstruction(currentStream.Count-2);
                            DeleteInstruction(currentStream.Count-1);
                        }
                        modified = true;
                        break;
                    }
                    if (instruction1.IsByte == instruction0.IsByte)
                    {
                        if ((instruction0.Name == "ADD"))
                        {
                            Print(" ILADD3");
                            instruction2.Operand = instruction1.Operand;
                            instruction2.Name = "ILADD";
                            currentStream[currentStream.Count-3] = instruction2;
                            DeleteInstruction(currentStream.Count-2);
                            DeleteInstruction(currentStream.Count-1);
                            modified = true;
                            break;
                        }
                        if (instruction0.Name == "SUB")
                        {
                            //Print(" LISUB3");
                            instruction2.Operand = instruction1.Operand;
                            instruction2.Name = "LISUB";
                            currentStream[currentStream.Count-3] = instruction2;
                            DeleteInstruction(currentStream.Count-2);
                            DeleteInstruction(currentStream.Count-1);
                            modified = true;
                            break;
                        }
                    }
                    if (instruction0.Name == "GTI")
                    {
                        if (!instruction2.IsByte && (instruction2.IsByte == instruction1.IsByte) && (instruction1.IsByte == instruction0.IsByte))
                        {
                            //Print(" LIGTI3");
                            instruction2.Operand = instruction1.Operand;
                            instruction2.Name = "LIGTI";
                            currentStream[currentStream.Count-3] = instruction2;
                            DeleteInstruction(currentStream.Count-2);
                            DeleteInstruction(currentStream.Count-1);
                            modified = true;
                            break;
                        }
                    }
                }
            }
            
            if ((instruction2.Name == "PUSHI") && (instruction1.Name == "PUSHL"))
            {
                if ((instruction2.IsByte == instruction1.IsByte) && (instruction1.IsByte == instruction0.IsByte))
                {
                    if (instruction0.Name == "ADD")
                    {
                        //Print(" ILADD3");
                        instruction2.Offset = instruction1.Offset;
                        instruction2.Name = "ILADD";
                        currentStream[currentStream.Count-3] = instruction2;
                        DeleteInstruction(currentStream.Count-2);
                        DeleteInstruction(currentStream.Count-1);
                        modified = true;
                        break;
                    }
                    if (instruction0.Name == "SUB")
                    {
                        //Print(" ILSUB3");
                        instruction2.Offset = instruction1.Offset;
                        instruction2.Name = "ILSUB";
                        currentStream[currentStream.Count-3] = instruction2;
                        DeleteInstruction(currentStream.Count-2);
                        DeleteInstruction(currentStream.Count-1);
                        modified = true;
                        break;
                    }
                }
            }
            break;
        } // loop
        return modified;
    }
    
    bool OptimizeFour()
    {
        bool modified;
        Instruction instruction3 = currentStream[currentStream.Count-4];
        Instruction instruction2 = currentStream[currentStream.Count-3];
        Instruction instruction1 = currentStream[currentStream.Count-2];
        Instruction instruction0 = currentStream[currentStream.Count-1];
        loop
        {
            if (instruction3.Offset == instruction0.Offset)
            {
                if (instruction3.IsByte && instruction2.IsByte && instruction1.IsByte && instruction0.IsByte)
                {
                    if ((instruction3.Name == "PUSHL") && (instruction2.Name == "PUSHI") && (instruction1.Name == "ADD") && (instruction0.Name == "POPL"))
                    {
                        Print(" INCLI4");
                        instruction3.Operand = instruction2.Operand;
                        instruction3.IsByte  = true;
                        instruction3.Name    = "INCLI";
                        currentStream[currentStream.Count-4] = instruction3;
                        DeleteInstruction(currentStream.Count-1);
                        DeleteInstruction(currentStream.Count-1);
                        DeleteInstruction(currentStream.Count-1);
                        modified = true;
                        break;
                    }
                    if ((instruction3.Name == "PUSHL") && (instruction2.Name == "PUSHL") && (instruction1.Name == "ADD") && (instruction0.Name == "POPL")) // UNUSED OPT 6                   
                    {   
                        Print(" LLADDL4");
                        instruction3.Offset2 = instruction2.Offset;
                        instruction3.IsByte  = true;
                        instruction3.Name    = "LLADDL";
                        currentStream[currentStream.Count-4] = instruction3;
                        DeleteInstruction(currentStream.Count-1);
                        DeleteInstruction(currentStream.Count-1);
                        DeleteInstruction(currentStream.Count-1);
                        modified = true;
                        break;
                    }
                }
                if (!instruction3.IsByte && !instruction2.IsByte && !instruction1.IsByte && !instruction0.IsByte)
                {
                    if ((instruction3.Name == "PUSHL") && (instruction2.Name == "PUSHI") && (instruction1.Name == "ADD") && (instruction0.Name == "POPL"))
                    {
                        Print(" INCLI4");
                        instruction3.Operand = instruction2.Operand;                   
                        instruction3.IsByte  = false;
                        instruction3.Name    = "INCLI";
                        currentStream[currentStream.Count-4] = instruction3;
                        DeleteInstruction(currentStream.Count-1);
                        DeleteInstruction(currentStream.Count-1);
                        DeleteInstruction(currentStream.Count-1);
                        modified = true;
                        break;
                    }
                    if ((instruction3.Name == "PUSHL") && (instruction2.Name == "PUSHL") && (instruction1.Name == "ADD") && (instruction0.Name == "POPL")) // LLADDL4
                    {
                        Print(" LLADDL4");
                        instruction3.Offset2 = instruction2.Offset;
                        instruction3.IsByte  = false;
                        instruction3.Name    = "LLADDL";
                        currentStream[currentStream.Count-4] = instruction3;
                        DeleteInstruction(currentStream.Count-1);
                        DeleteInstruction(currentStream.Count-1);
                        DeleteInstruction(currentStream.Count-1);
                        modified = true;
                        break;
                    }
                }
                
            }
            
            break;
        } // loop
        return modified;
    }
    
    Append(Instruction instruction)
    {
        if (!FirstPass && Compiling)
        {
            currentStream.Append(instruction);
            if (IsOptimized)
            {
                if (enablePeephole)
                {
                    // peephole optimizer
                    loop
                    {
                        if (currentStream.Count >= 2)
                        {
                            if (OptimizeTwo())   { continue; } // modified, loop again
                        }
                        if (currentStream.Count >= 3)
                        {
                            if (OptimizeThree()) { continue; } // modified, loop again
                        }
                        if (currentStream.Count >= 4)
                        {
                            if (OptimizeFour())  { continue; } // modified, loop again
                        }
                        break; // made it here with no modifications, exit
                    }
                }
            }
        }
    }
        
    Append(string name, bool isByte, uint operand)
    {
        Instruction instruction;
        instruction.Name = name;
        instruction.IsByte = isByte;
        instruction.Operand = operand;
        Append(instruction);
    }
    Append(string name, bool isByte, int offset)
    {
        Instruction instruction;
        instruction.Name = name;
        instruction.IsByte = isByte;
        instruction.Offset = offset;
        Append(instruction);
    }
    Append(string name, bool isByte)
    {
        Instruction instruction;
        instruction.Name = name;
        instruction.IsByte = isByte;
        Append(instruction);
    }
    Append(string name)
    {
        Instruction instruction;
        instruction.Name = name;
        Append(instruction);
    }
    Append(string name, string data)
    {
        Instruction instruction;
        instruction.Name = name;
        instruction.Data = data;
        Append(instruction);
    }
    Call(string functionName, bool isReturnByte, bool notVoid, uint argumentBytesToPop)
    {
        if (FirstPass)
        {
            TCSymbols.AddFunctionCall(CurrentFunction, functionName);
        }
        
        Instruction instruction;
        instruction.Name = "CALL";
        instruction.Data = functionName;
        instruction.IsByte = isReturnByte; // used by PushTop
        instruction.Operand = argumentBytesToPop;
        instruction.Offset  = (notVoid ? 1 : 0); // returnType != "void"
        Append(instruction);
        
        if (!capturingMode)
        {
            Generate(); // reset peephole
        }
        else
        {
            enablePeephole = false; // no more peephole for the remainder of this stream
        }
    }
       
    PushVariable(int offset, bool isByte, bool isGlobal)
    {
        Append((isGlobal ? "PUSHG" : "PUSHL"), isByte, offset);
    }
    PopVariable(int offset, bool isByte, bool isGlobal)
    {
        Append((isGlobal ? "POPG" : "POPL"), isByte, offset);
    }
    PushMemory(bool isByteIndex, bool isByte)
    {
        Append("PUSHM", isByte, int(isByteIndex ? 1 : 0));
    }
    PopMemory(bool isByteIndex, bool isByte)
    {
        Append("POPM", isByte, int(isByteIndex ? 1 : 0));
    }
    ZeroGlobal(bool isByte, uint operand)
    {
        Append("ZEROG", isByte, operand);
    }
    PushImmediate(bool isByte, uint literal)
    {
        Append("PUSHI", isByte, literal);
    }
    PadUnder()
    {
        Append("PADUNDER");
    }
    PushConst(uint literal)
    {
        Append("PUSHC", false, literal);
    }
    Comment(string comment)
    {
        if (!IsOptimized)
        {
            Append("REM", comment);
        }
    }
    Dup(bool isByte)
    {
        Append("DUP", isByte);
    }
    Add(bool isByte)
    {
        Append("ADD", isByte);
    }
    Sub(bool isByte)
    {
        Append("SUB", isByte);
    }
    
    Shl(bool isByte)
    {
        Append("SHL", isByte);
    }
    Shr(bool isByte)
    {
        Append("SHR", isByte);
    }
    
    And(bool isByte)
    {
        Append("AND", isByte);
    }
    Or(bool isByte)
    {
        Append("OR", isByte);
    }
    Xor(bool isByte)
    {
        Append("XOR", isByte);
    }
    Not(bool isByte)
    {
        Append("NOT", isByte);
    }
    BoolNot()
    {
        Append("BOOLNOT", true);
    }
    
    Mul(bool isByte)
    {
        Append("MUL", isByte);
    }
    Div(bool isByte)
    {
        Append("DIV", isByte);
    }
    Mod(bool isByte)
    {
        Append("MOD", isByte);
    }
    MulI()
    {
        Append("MULI");
    }
    DivI()
    {
        Append("DIVI");
    }
    ModI()
    {
        Append("MODI");
    }
    
    EQ(bool isByte)
    {
        Append("EQ", isByte);
    }
    NE(bool isByte)
    {
        Append("NE", isByte);
    }
    LT(bool isByte)
    {
        Append("LT", isByte);
    }
    LE(bool isByte)
    {
        Append("LE", isByte);
    }
    GT(bool isByte)
    {
        Append("GT", isByte);
    }
    GE(bool isByte)
    {
        Append("GE", isByte);
    }
    LTI()
    {
        Append("LTI");
    }
    LEI()
    {
        Append("LEI");
    }
    GTI()
    {
        Append("GTI");
    }
    GEI()
    {
        Append("GEI");
    }
    
    DecSP(byte bytes)
    {
        Append("DECSP", false, bytes);
    }
    
    IF()
    {
        Append("IF");
        if (!capturingMode)
        {
            Generate(); // reset peephole
        }
        else
        {
            enablePeephole = false; // no more peephole for the remainder of this stream
        }
    }
    ELSE()
    {
        Append("ELSE");
        if (!capturingMode)
        {
            Generate(); // reset peephole
        }
        else
        {
            enablePeephole = false; // no more peephole for the remainder of this stream
        }
    }
    ENDIF()
    {
        Append("ENDIF");
        if (!capturingMode)
        {
            Generate(); // reset peephole
        }
        else
        {
            enablePeephole = false; // no more peephole for the remainder of this stream
        }
    }
    
    BeginStream(bool capturing)
    {
        nestedStreamMode++;
        if (nestedStreamMode != 1)
        {
            return;
        }
        if (InStreamMode)
        {
            Die(0x0B);
        }
        capturingMode = capturing;
        enablePeephole = true;
        InStreamMode = true;
        currentStream.Clear();
    }
    
    <Instruction> CaptureStream()
    {
        if (!InStreamMode)
        {
            Die(0x0B);
        }
        nestedStreamMode--;
        if (nestedStreamMode != 0)
        {
            Die(0x0B);
        }
        InStreamMode = false;
        <Instruction> captured = currentStream;
        currentStream.Clear();
        return captured;
    }
    EmitStream(<Instruction> captured)
    {
        if (InStreamMode)
        {
            Die(0x0B);
        }
        if (nestedStreamMode != 0)
        {
            Die(0x0B);
        }
        nestedStreamMode++;
        InStreamMode = true;
        currentStream = captured;
        FlushStream();
    }
    FlushStream()
    {
        if (!InStreamMode)
        {
            Die(0x0B);
        }
        nestedStreamMode--;
        if (nestedStreamMode != 0)
        {
            return;
        }
        Generate();
        InStreamMode = false;
    }
    
    LoopExit(string comment)
    {
        Append("LOOPEXIT", comment);
    }
    
    
    string ToString(<Instruction> instructions)
    {
        string content;
        string comma;
        foreach (var vi in instructions)
        {
            Instruction instruction = vi;    
            if (instruction.Name == "REM") 
            {
                content += comma + " REM"; 
            }
            else
            {
                content += comma + " " + ToString(instruction);
            }           
            comma = ",";   
        }
        return content;
    }
    string OffsetToHex(int offset)
    {
        byte b = offset.GetByte(0);
        return "0x" + b.ToHexString(2);
    }
    
    string ToString(Instruction instruction)
    {
        string content;
        string width = (instruction.IsByte ? "B" : "");
        switch (instruction.Name)
        {
            case "REM":
            {
                content = instruction.Data;
            }
            case "LOOPEXIT":
            {
                content = "LOOPEXIT";
            }
            case "PUSHI":
            {
                content = "PUSHI"+width+" 0x" + (instruction.Operand).ToHexString(instruction.IsByte ? 2 : 4);
            }
            case "ZEROG":
            {
                content = "ZEROG"+width+"  0x" + (instruction.Operand).ToHexString(2);
            }
            case "PUSHC":
            {
                content = "PUSHC Resources.StrConsts + 0x" + (instruction.Operand).ToHexString(instruction.IsByte ? 2 : 4);
            }
            case "PUSHL":
            {
                content = "PUSHL"+width+" [BP+" + OffsetToHex(instruction.Offset) + "]";
            }
            case "POPL":
            {
                content = "POPL"+width+" [BP+" + OffsetToHex(instruction.Offset) + "]";
            }
            case "PUSHG":
            {
                content = "PUSHG"+width+" [" + GlobalOperand(instruction.Offset) + "]";
            }
            case "POPG":
            {
                content = "POPG"+width+" [" + GlobalOperand(instruction.Offset) + "]";
            }
            case "PUSHM":
            {
                content = "PUSHM"+width;
            }
            case "POPM":
            {
                content = "POPM"+width;
            }
            case "CALL":
            {
                content = "CALL '" + instruction.Data + "'";
                if (instruction.Operand != 0)
                {
                    content += ", DECSP " + (instruction.Operand).ToString();
                }
                if (instruction.Offset != 0)
                {
                    content += ", PUSH TOP" + width;
                }
            }
            case "DECSP":
            {
                content = "DECSP " + (instruction.Operand).ToString();
            }
            case "ADD":
            case "SUB":
            case "MUL":
            case "DIV":
            case "MOD":
            
            case "SHL":
            case "SHR":
            case "AND":
            case "OR":
            case "XOR":
            
            case "EQ":
            case "NE":
            case "GT":
            case "GE":
            case "LT":
            case "LE":
            {
                content = instruction.Name + width;
            }
            
            case "NOT":
            {
                content = instruction.Name + width;
            }
            case "BOOLNOT":
            {
                content = instruction.Name;
            }
            
            case "MULI":
            case "DIVI":
            case "MODI":
            
            case "GTI":
            case "GEI":
            case "LTI":
            case "LEI":
            {
                content = instruction.Name;
            }
            
            case "IF":
            case "ELSE":
            case "ENDIF":
            case "PADUNDER":
            {
                content = instruction.Name;
            }
            
            
            case "LGADD":
            case "LGADDM":
            {
                content = instruction.Name + width + " [BP+" + OffsetToHex(instruction.Offset) + "] [" + GlobalOperand(instruction.Offset2) + "]";
            }
            case "LGADDI":
            case "LGADDIM":
            {
                content = instruction.Name + width + " [BP+" + OffsetToHex(instruction.Offset) + "] [" + GlobalOperand(instruction.Offset2) + "] # 0x" + (instruction.Operand).ToHexString(2);
            }
            case "2L":
            {
                content = instruction.Name + width + " [BP+" + OffsetToHex(instruction.Offset) + "] [BP+0x" + (instruction.Offset).ToHexString(2) + "]";
            }
            case "LLADD":
            case "LLADDL":
            {
                content = instruction.Name + width + " [BP+" + OffsetToHex(instruction.Offset) + "] [BP+0x" + (instruction.Offset2).ToHexString(2) + "]";
            }
            case "2G":
            {
                content = instruction.Name + width + " [" + GlobalOperand(instruction.Offset) + "] [" +  GlobalOperand(instruction.Offset2) + "]";
            }
            case "GGADD":
            case "GGADDM":
            {
                content = instruction.Name + width + " [" + GlobalOperand(instruction.Offset) + "] [" +GlobalOperand(instruction.Offset2) + "]";
            }
            case "GGADDG":
            {
                content = instruction.Name + width + " [" + GlobalOperand(instruction.Offset) + "] [" +GlobalOperand(instruction.Offset2) + "] [" +GlobalOperand(int(instruction.Operand)) + "]";
            }
            
            case "GGADDI":
            case "GGADDIM":
            {
                content = instruction.Name + width + " [" + GlobalOperand(instruction.Offset) + "] [" +GlobalOperand(instruction.Offset2) + "] # 0x" + (instruction.Operand).ToHexString(2);
            }
            case "INCLI":
            case "STLI":
            case "LILT":
            case "LILE":
            case "LIGTI":
            case "LIAND":
            case "LISHR":
            case "LISHL":
            case "LIADD":
            case "LISUB":
            case "LILTX":
            case "LILEX":
            {
                content = instruction.Name + width + " [BP+" + OffsetToHex(instruction.Offset) + "] # 0x" + (instruction.Operand).ToHexString(instruction.IsByte ? 2 : 4);
            }
            case "GILT":
            case "INCGI":
            case "GIADD":
            case "GILTX":
            {
                content = instruction.Name + width + " [" + GlobalOperand(instruction.Offset) + "] # 0x" + (instruction.Operand).ToHexString(instruction.IsByte ? 2 : 4);
            }
            case "IADD":
            {
                content = instruction.Name + width + " # 0x" + (instruction.Operand).ToHexString(instruction.IsByte ? 2 : 4);
            }
            case "IADDL":
            {
                content = instruction.Name + width + " # 0x" + (instruction.Operand).ToHexString(instruction.IsByte ? 2 : 4) + " [BP+" + OffsetToHex(instruction.Offset) + "]";
            }
            case "LTX":
            case "LEX":
            {
                content = instruction.Name + width + " " +instruction.Data;
            }
                       
            case "LIANDFF":
            {
                content = instruction.Name + width + " [BP+" + OffsetToHex(instruction.Offset) + "] # 0x00FF";
            }
            case "LISHR8":
            case "LISHL8":
            {
                content = instruction.Name + width + " [BP+" + OffsetToHex(instruction.Offset) + "] # 0x0008";
            }
            case "ILADD":
            case "ILSUB":
            {
                content = instruction.Name + width + " # 0x" + (instruction.Operand).ToHexString(instruction.IsByte ? 2 : 4) + " [BP+" + OffsetToHex(instruction.Offset) + "]";
            }
            
            default:
            {
                Print(" ToString() Not Implemented: '" + instruction.Name + "'");
                Die(0x0A);
            }
        }
        return content;
    }
    Generate()
    {   
        if (FirstPass || (currentStream.Count == 0) || !Compiling) { return; }
        
        InStreamMode = false;
        bool wasRem;
        if (IsExperimental && (currentStream.Count != 1))
        {
            string content = ToString(currentStream);
            PadOut("", 0);
            PadOut("// ### " + content, 0);
        }
        
        foreach (var vi in currentStream)
        {
            Instruction instruction = vi;
            
            if (!wasRem) { PadOut("", 0); }
            wasRem = false;
            TCCode.PadOut("// " + ToString(instruction), 0);
            string name = instruction.Name;
            if (instruction.IsByte && (name != "CALL"))
            {
                name += "B";
            }
            uint operand = instruction.Operand;
            switch (name)
            {
                case "REM":     { wasRem = true; } // ToString() above
                
                case "PUSHI":   
                { 
                    TCCode.PushWord(operand);
                }
                case "PUSHIB":
                { 
                    TCCode.PushByte(operand.GetByte(0));
                }
                
                case "ZEROG":
                case "ZEROGB":
                {
                    int offset = Int.FromBytes(operand.GetByte(0), operand.GetByte(1));
                    TCCode.PadOut("STZ " + GlobalOperand(offset), 0);
                    if (!instruction.IsByte)
                    {
                        TCCode.PadOut("STZ " + GlobalOperand(offset+1), 0);
                    }
                }
                
                case "POPL":
                case "POPLB":
                {
                    TCCode.PopVariable(instruction.Data, instruction.Offset, instruction.IsByte, false);
                }
                case "POPG":
                case "POPGB":
                {
                    TCCode.PopVariable(instruction.Data, instruction.Offset, instruction.IsByte, true);
                }
                case "PUSHL":
                case "PUSHLB":
                {
                    TCCode.PushVariable(instruction.Data, instruction.Offset, instruction.IsByte, false);
                }
                case "PUSHG":
                case "PUSHGB":
                {
                    TCCode.PushVariable(instruction.Data, instruction.Offset, instruction.IsByte, true);
                }
                case "PUSHM":
                case "PUSHMB":
                {
                    ReadMemory(instruction.Offset == 1, instruction.IsByte);  
                }
                case "POPM":
                case "POPMB":
                {
                    WriteMemory(instruction.Offset == 1, instruction.IsByte);
                }
                
                case "PUSHC":
                {
                    TCCode.PushConst(instruction.Operand);
                }
                
                case "ADD":
                case "ADDB":
                {
                    TCOps.Add(instruction.IsByte);
                }
                case "SUB":
                case "SUBB":
                {
                    TCOps.Sub(instruction.IsByte);
                }
                case "MUL":
                case "MULB":
                {
                    TCOps.Mul(instruction.IsByte);
                }
                case "DIV":
                case "DIVB":
                {
                    TCOps.Div(instruction.IsByte);
                }
                case "MOD":
                case "MODB":
                {
                    TCOps.Mod(instruction.IsByte);
                }
                
                case "MULI":
                {
                    TCOps.MulI();
                }
                case "DIVI":
                {
                    TCOps.DivI();
                }
                case "MODI":
                {
                    TCOps.ModI();
                }
                
                case "EQ":
                case "EQB":
                {
                    TCOps.CompareEQ(instruction.IsByte);
                }
                case "NE":
                case "NEB":
                {
                    TCOps.CompareNE(instruction.IsByte);
                }
                case "GT":
                case "GTB":
                {
                    TCOps.CompareGT(instruction.IsByte);
                }
                case "GE":
                case "GEB":
                {
                    TCOps.CompareGE(instruction.IsByte);
                }
                
                case "LTI":
                case "LTIB":
                {
                    TCOps.CompareLTI();
                }
                case "LEI":
                case "LEIB":
                {
                    TCOps.CompareLEI();
                }
                case "GTI":
                case "GTIB":
                {
                    TCOps.CompareGTI();
                }
                case "GEI":
                case "GEIB":
                {
                    TCOps.CompareGEI();
                }
                case "SHL":
                case "SHLB":
                {
                    TCOps.Shl(instruction.IsByte);
                }
                case "SHR":
                case "SHRB":
                {
                    TCOps.Shr(instruction.IsByte);
                }
                case "AND":
                case "ANDB":
                {
                    TCOps.And(instruction.IsByte);
                }
                case "OR":
                case "ORB":
                {
                    TCOps.Or(instruction.IsByte);
                }
                case "XOR":
                case "XORB":
                {
                    TCOps.Xor(instruction.IsByte);
                }
                case "NOT":
                case "NOTB":
                {
                    TCOps.BitNot(instruction.IsByte);
                }
                case "BOOLNOTB":
                {
                    TCOps.BoolNot();
                }
                case "LOOPEXIT":
                {
                    TCCode.PadOut("PLA", 0);
                    TCCode.PadOut("if (Z) // " + instruction.Data, 0);
                    TCCode.PadOut("{", 0);
                    TCCode.PadOut("break;", 1);
                    TCCode.PadOut("}", 0);
                }
                
                case "PADUNDER":
                {
                    TCCode.CastPad(true);
                }
                case "DECSP":
                {
                    TCCode.PopBytes(operand.GetByte(0), "");
                }
                case "CALL":
                {
                    TCCode.Call(instruction.Data);
                    if (instruction.Operand != 0)
                    {
                        TCCode.PopBytes(operand.GetByte(0), "");
                    }
                    if (instruction.Offset != 0)
                    {
                        TCOps.PushTop(instruction.IsByte);
                    }
                }
                case "IF":
                {
                    TCCode.If("if");
                    TCCode.PadOut("{", 0);
                }
                case "ELSE":
                {
                    TCCode.PadOut("}", 0);
                    TCCode.Else();
                    TCCode.PadOut("{", 0);
                }
                case "ENDIF":
                {
                    TCCode.PadOut("}", 0);
                }
                
                case "INCLI":
                case "INCLIB":
                {
                    if (instruction.Operand != 0)
                    {
                        TCCode.BPOffset(instruction.Offset, "X", instruction.IsByte); 
                        if (instruction.Operand == 1)
                        {
                            if (instruction.IsByte)
                            {
                                TCCode.PadOut("INC 0x0100, X // LSB", 0);
                            }
                            else
                            {
                                TCCode.PadOut("INC 0x0101, X // LSB", 0);
                                TCCode.PadOut("if (Z)", 0);
                                TCCode.PadOut("{", 0);
                                TCCode.PadOut("INC 0x0100, X // MSB", 1);
                                TCCode.PadOut("}", 0);
                            }
                        }
                        else
                        {
                            TCCode.PadOut("CLC", 0);
                            if (instruction.IsByte)
                            {
                                TCCode.PadOut("LDA 0x0100, X // LSB", 0);
                                TCCode.PadOut("ADC # 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2), 0);
                                TCCode.PadOut("STA 0x0100, X // LSB", 0);
                            }
                            else
                            {
                                TCCode.PadOut("LDA 0x0101, X // LSB", 0);
                                TCCode.PadOut("ADC # 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2), 0);
                                TCCode.PadOut("STA 0x0101, X // LSB", 0);
                                TCCode.PadOut("LDA 0x0100, X // MSB", 0);
                                TCCode.PadOut("ADC # 0x" + ((instruction.Operand).GetByte(1)).ToHexString(2), 0);
                                TCCode.PadOut("STA 0x0100, X // MSB", 0);
                            }
                        }
                    }               
                }
                case "INCGI":
                case "INCGIB":
                {
                    if (instruction.Operand != 0)
                    {
                        int goffset = instruction.Offset;
                        if (instruction.Operand == 1)
                        {
                            TCCode.PadOut("INC " + GlobalOperand(goffset), 0);
                            if (!instruction.IsByte)
                            {
                                TCCode.PadOut("if (Z)", 0);
                                TCCode.PadOut("{", 0);
                                TCCode.PadOut("INC " + GlobalOperand(goffset+1), 1);
                                TCCode.PadOut("}", 0);
                            }
                        }
                        else
                        {
                            TCCode.PadOut("CLC", 0);
                            TCCode.PadOut("LDA " + GlobalOperand(goffset), 0);
                            TCCode.PadOut("ADC # 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2), 0);
                            TCCode.PadOut("STA " + GlobalOperand(goffset), 0);
                            if (!instruction.IsByte)
                            {
                                TCCode.PadOut("LDA " + GlobalOperand(goffset+1), 0);
                                TCCode.PadOut("ADC # 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2), 0);
                                TCCode.PadOut("STA " + GlobalOperand(goffset+1), 0);
                            }
                        }
                    }               
                }
                case "LT":
                case "LTB":
                {
                    TCOps.CompareLT(instruction.IsByte);
                }
                case "LE":
                case "LEB":
                {
                    TCOps.CompareLE(instruction.IsByte);
                }
                case "LEX":
                case "LEXB":
                {
                    // TODO : optimize
                    TCOps.CompareLE(instruction.IsByte);
                    TCCode.PadOut("PLA", 0);
                    TCCode.PadOut("if (Z) // " + instruction.Data, 0);
                    TCCode.PadOut("{", 0);
                    TCCode.PadOut("break;", 1);
                    TCCode.PadOut("}", 0);
                }
                case "LTX":
                case "LTXB":
                {
                    // TODO : optimize
                    TCOps.CompareLT(instruction.IsByte);
                    TCCode.PadOut("PLA", 0);
                    TCCode.PadOut("if (Z) // " + instruction.Data, 0);
                    TCCode.PadOut("{", 0);
                    TCCode.PadOut("break;", 1);
                    TCCode.PadOut("}", 0);
                }
                
                case "LILE":
                case "LILEB":
                {
                    TCCode.BPOffset(instruction.Offset, "Y", instruction.IsByte);
                    // arguments in NEXT and TOP
                    string top = (instruction.IsByte ? ("# 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2)) :  ("# 0x" + (instruction.Operand).ToHexString(4)));
                    string next = "[BP+" + OffsetToHex(instruction.Offset) + "]";
                    TCCode.PadOut("LDX #1 // " + next + " <= " + top, 0);
                    if (instruction.IsByte)
                    {
                        TCCode.PadOut("LDA 0x0100, Y // LSB", 0); // NEXTL
                        TCCode.PadOut("CMP # 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2), 0); // TOPL
                    }
                    else
                    {
                        TCCode.PadOut("LDA 0x0100, Y // MSB", 0); // NEXTH
                        TCCode.PadOut("CMP # 0x" + ((instruction.Operand).GetByte(1)).ToHexString(2), 0); // TOPH
                        TCCode.PadOut("if (Z)", 0);
                        TCCode.PadOut("{", 0);
                        TCCode.PadOut("LDA 0x0101, Y // LSB", 1); // NEXTL
                        TCCode.PadOut("CMP # 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2), 1); // TOPL
                        TCCode.PadOut("}", 0);
                    }
                    TCCode.PadOut("if (NZ) // " + next + " == " + top + " (not >)?", 0);
                    TCCode.PadOut("{", 0);
                    TCCode.PadOut("if (C) // " + next + " <  " + top + " (not >)?", 1);
                    TCCode.PadOut("{", 1);
                    TCCode.PadOut("LDX #0  // " + next + " > " + top + "", 2);
                    TCCode.PadOut("}", 1);
                    TCCode.PadOut("}", 0);
                    TCCode.PadOut("// result in X", 0);
                    TCCode.PadOut("PHX", 0);
                }
                
                
                case "LILT":
                case "LILTB":
                {
                    TCCode.BPOffset(instruction.Offset, "Y", instruction.IsByte);
                    // arguments in NEXT and TOP
                    string top = (instruction.IsByte ? ("# 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2)) :  ("# 0x" + (instruction.Operand).ToHexString(4)));
                    string next = "[BP+" + OffsetToHex(instruction.Offset) + "]";
                    TCCode.PadOut("LDX #1 // " + next + " < " + top, 0);
                    if (instruction.IsByte)
                    {
                        TCCode.PadOut("LDA 0x0100, Y // LSB", 0); // NEXTL
                        TCCode.PadOut("CMP # 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2), 0); // TOPL
                    }
                    else
                    {
                        TCCode.PadOut("DEY", 0);
                        TCCode.PadOut("LDA 0x0100, Y // MSB", 0); // NEXTH
                        TCCode.PadOut("CMP # 0x" + ((instruction.Operand).GetByte(1)).ToHexString(2), 0); // TOPH
                        TCCode.PadOut("if (Z)", 0);
                        TCCode.PadOut("{", 0);
                        TCCode.PadOut("LDA 0x0101, Y // LSB", 1); // NEXTL
                        TCCode.PadOut("CMP # 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2), 1); // TOPL
                        TCCode.PadOut("}", 0);
                    }
                    TCCode.PadOut("if (C) // " + top + " <= " + next + " ?", 0);
                    TCCode.PadOut("{", 0);
                    TCCode.PadOut("LDX #0  // " + top + " <= " + next + "", 1);
                    TCCode.PadOut("}", 0);
                    TCCode.PadOut("// result in X", 0);
                    TCCode.PadOut("PHX", 0);
                }
                case "GILT":
                case "GILTB":
                {
                    // arguments in NEXT and TOP
                    string top = (instruction.IsByte ? ("# 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2)) :  ("# 0x" + (instruction.Operand).ToHexString(4)));
                    int goffset = instruction.Offset;
                    string next = "[" + GlobalOperand(goffset) + "]";
                    TCCode.PadOut("LDX #1 // " + next + " < " + top, 0);
                    if (instruction.IsByte)
                    {
                        TCCode.PadOut("LDA " + GlobalOperand(goffset), 0); // NEXTL
                        TCCode.PadOut("CMP # 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2), 0); // TOPL
                    }
                    else
                    {
                        TCCode.PadOut("LDA " + GlobalOperand(goffset+1), 0); // NEXTH
                        TCCode.PadOut("CMP # 0x" + ((instruction.Operand).GetByte(1)).ToHexString(2), 0); // TOPH
                        TCCode.PadOut("if (Z)", 0);
                        TCCode.PadOut("{", 0);
                        TCCode.PadOut("LDA " + GlobalOperand(goffset), 1); // NEXTL
                        TCCode.PadOut("CMP # 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2), 1); // TOPL
                        TCCode.PadOut("}", 0);
                    }
                    TCCode.PadOut("if (C) // " + top + " <= " + next + " ?", 0);
                    TCCode.PadOut("{", 0);
                    TCCode.PadOut("LDX #0  // " + top + " <= " + next + "", 1);
                    TCCode.PadOut("}", 0);
                    TCCode.PadOut("// result in X", 0);
                    TCCode.PadOut("PHX", 0);
                }
                case "GILTX":
                case "GILTXB":
                {
                    // arguments in NEXT and TOP
                    int goffset = instruction.Offset;
                    string top  = (instruction.IsByte ? ("# 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2)) :  ("# 0x" + (instruction.Operand).ToHexString(4)));
                    string next = "[" + GlobalOperand(goffset) + "]";
                    if (instruction.IsByte)
                    {
                        TCCode.PadOut("LDA " + GlobalOperand(goffset), 0); // NEXTL
                        TCCode.PadOut("CMP # 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2), 0); // TOPL
                    }
                    else
                    {
                        TCCode.PadOut("LDA " + GlobalOperand(goffset+1), 0); // NEXTH
                        TCCode.PadOut("CMP # 0x" + ((instruction.Operand).GetByte(1)).ToHexString(2), 0); // TOPH
                        TCCode.PadOut("if (Z)", 0);
                        TCCode.PadOut("{", 0);
                        TCCode.PadOut("LDA " + GlobalOperand(goffset), 1); // NEXTL
                        TCCode.PadOut("CMP # 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2), 1); // TOPL
                        TCCode.PadOut("}", 0);
                    }
                    TCCode.PadOut("if (C) // " + top + " <= " + next + " ?", 0);
                    TCCode.PadOut("{", 0);
                    TCCode.PadOut("break; // " + instruction.Data, 1);
                    TCCode.PadOut("}", 0);
                }
                case "LILEX":
                case "LILEXB":
                {
                    TCCode.BPOffset(instruction.Offset, "Y", instruction.IsByte);
                    // arguments in NEXT and TOP
                    string top = (instruction.IsByte ? ("# 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2)) :  ("# 0x" + (instruction.Operand).ToHexString(4)));
                    string next = "[BP+" + OffsetToHex(instruction.Offset) + "]";
                    if (instruction.IsByte)
                    {
                        TCCode.PadOut("LDA 0x0100, Y // LSB", 0); // NEXTL
                        TCCode.PadOut("CMP # 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2), 0); // TOPL
                    }
                    else
                    {
                        TCCode.PadOut("LDA 0x0100, Y // MSB", 0); // NEXTH
                        TCCode.PadOut("CMP # 0x" + ((instruction.Operand).GetByte(1)).ToHexString(2), 0); // TOPH
                        TCCode.PadOut("if (Z)", 0);
                        TCCode.PadOut("{", 0);
                        TCCode.PadOut("LDA 0x0101, Y // LSB", 1); // NEXTL
                        TCCode.PadOut("CMP # 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2), 1); // TOPL
                        TCCode.PadOut("}", 0);
                    }
                    TCCode.PadOut("if (NZ) // " + next + " == " + top + " (not >)?", 0);
                    TCCode.PadOut("{", 0);
                    TCCode.PadOut("if (C) // " + next + " <  " + top + " (not >)?", 1);
                    TCCode.PadOut("{", 1);
                    TCCode.PadOut("break; // " + instruction.Data, 2);
                    TCCode.PadOut("}", 1);
                    TCCode.PadOut("}", 0);
                }
                
                case "LILTX":
                case "LILTXB":
                {
                    TCCode.BPOffset(instruction.Offset, "Y", instruction.IsByte);
                    // arguments in NEXT and TOP
                    string top = (instruction.IsByte ? ("# 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2)) :  ("# 0x" + (instruction.Operand).ToHexString(4)));
                    string next = "[BP+" + OffsetToHex(instruction.Offset) + "]";
                    if (instruction.IsByte)
                    {
                        TCCode.PadOut("LDA 0x0100, Y // LSB", 0); // NEXTL
                        TCCode.PadOut("CMP # 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2), 0); // TOPL
                    }
                    else
                    {
                        TCCode.PadOut("LDA 0x0100, Y // MSB", 0); // NEXTH
                        TCCode.PadOut("CMP # 0x" + ((instruction.Operand).GetByte(1)).ToHexString(2), 0); // TOPH
                        TCCode.PadOut("if (Z)", 0);
                        TCCode.PadOut("{", 0);
                        TCCode.PadOut("LDA 0x0101, Y // LSB", 1); // NEXTL
                        TCCode.PadOut("CMP # 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2), 1); // TOPL
                        TCCode.PadOut("}", 0);
                    }
                    TCCode.PadOut("if (C) // " + top + " <= " + next + " ?", 0);
                    TCCode.PadOut("{", 0);
                    TCCode.PadOut("break; // " + instruction.Data, 1);
                    TCCode.PadOut("}", 0);
                }
                case "STLI":
                case "STLIB":
                {
                    TCCode.BPOffset(instruction.Offset, "X", instruction.IsByte);
                    if (instruction.Operand == 0)
                    {
                        if (instruction.IsByte)
                        {
                            TCCode.PadOut("STZ 0x0100, X", 0);
                        }
                        else
                        {
                            TCCode.PadOut("STZ 0x0101, X // LSB", 0);
                            TCCode.PadOut("STZ 0x0100, X // MSB", 0);
                        }
                    }
                    else
                    {
                        
                        if (instruction.IsByte)
                        {
                            TCCode.PadOut("LDA # 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2), 0);
                            TCCode.PadOut("STA 0x0100, X", 0);
                        }
                        else
                        {
                            TCCode.PadOut("LDA # 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2), 0);
                            TCCode.PadOut("STA 0x0101, X // LSB", 0);
                            if ((instruction.Operand).GetByte(1) != (instruction.Operand).GetByte(0))
                            {
                                TCCode.PadOut("LDA # 0x" + ((instruction.Operand).GetByte(1)).ToHexString(2), 0);
                            }
                            TCCode.PadOut("STA 0x0100, X // MSB", 0);
                        }
                    }
                }
                case "2L":
                case "2LB":
                {
                    TCCode.BPOffset(instruction.Offset, "X", instruction.IsByte);
                    if (instruction.IsByte)
                    {
                        TCCode.PadOut("LDA 0x0100, X", 0);
                        TCCode.PadOut("ASL", 0);
                        TCCode.PadOut("PHA", 0);
                    }
                    else
                    {
                        TCCode.PadOut("LDA 0x0101, X // LSB", 0);
                        TCCode.PadOut("ASL", 0);
                        TCCode.PadOut("PHA", 0);
                        TCCode.PadOut("LDA 0x0100, X // MSB", 0);
                        TCCode.PadOut("ROL", 0);
                        TCCode.PadOut("PHA", 0);
                    }
                }
                case "2G":
                case "2GB":
                {
                    int goffset = instruction.Offset;
                    TCCode.PadOut("LDA " + GlobalOperand(goffset), 0);                          
                    TCCode.PadOut("ASL", 0);
                    TCCode.PadOut("PHA", 0);
                    if (!instruction.IsByte)
                    {
                        TCCode.PadOut("LDA " + GlobalOperand(goffset+1), 0);
                        TCCode.PadOut("ROL", 0);
                        TCCode.PadOut("PHA", 0);
                    }
                }
                case "GGADD":
                case "GGADDB":
                {
                    int goffset  = instruction.Offset;
                    int goffset2 = instruction.Offset2;
                    TCCode.PadOut("CLC", 0);
                    TCCode.PadOut("LDA " + GlobalOperand(goffset), 0);
                    TCCode.PadOut("ADC " + GlobalOperand(goffset2), 0);
                    TCCode.PadOut("PHA", 0);
                    if (!instruction.IsByte)
                    {
                        TCCode.PadOut("LDA " + GlobalOperand(goffset+1), 0);
                        TCCode.PadOut("ADC " + GlobalOperand(goffset2+1), 0);
                        TCCode.PadOut("PHA", 0);
                    }
                }
                case "LLADD":
                case "LLADDB":
                {
                    TCCode.BPOffset(instruction.Offset, "X", instruction.IsByte);
                    bool sameL = (instruction.Offset2 ==  instruction.Offset);
                    string y = "X";
                    if (!sameL)
                    {
                        TCCode.BPOffset(instruction.Offset2, "Y", instruction.IsByte); 
                        y = "Y";
                    }
                    if (instruction.IsByte)
                    {
                        TCCode.PadOut("CLC", 0);
                        TCCode.PadOut("LDA 0x0100, X", 0);
                        TCCode.PadOut("ADC 0x0100, " + y, 0);
                        TCCode.PadOut("PHA", 0);
                    }
                    else
                    {
                        TCCode.PadOut("CLC", 0);
                        TCCode.PadOut("LDA 0x0101, X", 0);
                        TCCode.PadOut("ADC 0x0101, " + y, 0);
                        TCCode.PadOut("PHA", 0);
                        TCCode.PadOut("LDA 0x0100, X", 0);
                        TCCode.PadOut("ADC 0x0100, " + y, 0);
                        TCCode.PadOut("PHA", 0);
                    }
                }
                case "GGADDM":
                {
                    if (instruction.IsByte)
                    {
                        Die(0x0A);
                    }
                    TCCode.PadOut("CLC", 0);
                    int goffset  = instruction.Offset;
                    int goffset2 = instruction.Offset2;
                    TCCode.PadOut("LDA " + GlobalOperand(goffset), 0);
                    TCCode.PadOut("ADC " + GlobalOperand(goffset2), 0);
                    TCCode.PadOut("STA ZP.TOPL", 0);
                    TCCode.PadOut("LDA " + GlobalOperand(goffset+1), 0);
                    TCCode.PadOut("ADC " + GlobalOperand(goffset2+1), 0);
                    TCCode.PadOut("STA ZP.TOPH", 0);
                    
                    TCCode.PadOut("LDA [ZP.TOP]", 0);
                    TCCode.PadOut("PHA", 0);
                
                }
                case "LGADDM":
                {
                    if (instruction.IsByte)
                    {
                        Die(0x0A);
                    }
                    TCCode.BPOffset(instruction.Offset, "X", false); 
                    
                    TCCode.PadOut("CLC", 0);
                    TCCode.PadOut("LDA 0x0101, X // LSB", 0);
                    int goffset = instruction.Offset2;
                    TCCode.PadOut("ADC " + GlobalOperand(goffset), 0);
                    TCCode.PadOut("STA ZP.TOPL", 0);
                    
                    TCCode.PadOut("LDA 0x0100, X // MSB", 0);
                    TCCode.PadOut("ADC " + GlobalOperand(goffset+1), 0);
                    TCCode.PadOut("STA ZP.TOPH", 0);
                    
                    TCCode.PadOut("LDA [ZP.TOP]", 0);
                    TCCode.PadOut("PHA", 0);
                
                }
                case "LGADDIM":
                {
                    if (instruction.IsByte)
                    {
                        Die(0x0A);
                    }
                    TCCode.BPOffset(instruction.Offset, "X", false); 
                    
                    TCCode.PadOut("CLC", 0);
                    TCCode.PadOut("LDA 0x0101, X // LSB", 0);
                    int goffset = instruction.Offset2;
                    TCCode.PadOut("ADC " + GlobalOperand(goffset), 0);
                    TCCode.PadOut("STA ZP.TOPL", 0);
                    
                    TCCode.PadOut("LDA 0x0100, X // MSB", 0);
                    TCCode.PadOut("ADC " + GlobalOperand(goffset+1), 0);
                    TCCode.PadOut("STA ZP.TOPH", 0);
                    TCCode.PadOut("LDA # 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2), 0);
                    TCCode.PadOut("STA [ZP.TOP]", 0);
                }
                case "LGADDI":
                {
                    if (instruction.IsByte)
                    {
                        Die(0x0A);
                    }
                    TCCode.BPOffset(instruction.Offset, "X", false); 
                    
                    TCCode.PadOut("CLC", 0);
                    TCCode.PadOut("LDA 0x0101, X // LSB", 0);
                    int goffset = instruction.Offset2;
                    TCCode.PadOut("ADC " + GlobalOperand(goffset), 0);
                    TCCode.PadOut("PHA", 0);
                    
                    TCCode.PadOut("LDA 0x0100, X // MSB", 0);
                    TCCode.PadOut("ADC 0x" + GlobalOperand(goffset+1), 0);
                    TCCode.PadOut("PHA", 0);
                    TCCode.PadOut("LDA # 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2), 0);
                    TCCode.PadOut("PHA", 0);
                }
                
                case "GGADDIM":
                {
                    if (instruction.IsByte)
                    {
                        Die(0x0A);
                    }
                    TCCode.PadOut("CLC", 0);
                    int goffset = instruction.Offset;
                    int goffset2 = instruction.Offset2;
                    TCCode.PadOut("LDA " + GlobalOperand(goffset), 0);
                    TCCode.PadOut("ADC " + GlobalOperand(goffset2), 0);
                    TCCode.PadOut("STA ZP.TOPL", 0);
                    TCCode.PadOut("LDA " + GlobalOperand(goffset+1), 0);
                    TCCode.PadOut("ADC " + GlobalOperand(goffset2+1), 0);
                    TCCode.PadOut("STA ZP.TOPH", 0);
                    TCCode.PadOut("LDA # 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2), 0);
                    TCCode.PadOut("STA [ZP.TOP]", 0);
                }
                case "GGADDI":
                {
                    if (instruction.IsByte)
                    {
                        Die(0x0A);
                    }
                    TCCode.PadOut("CLC", 0);
                    int goffset = instruction.Offset;
                    int goffset2 = instruction.Offset2;
                    TCCode.PadOut("LDA " + GlobalOperand(goffset), 0);
                    TCCode.PadOut("ADC " + GlobalOperand(goffset2), 0);
                    TCCode.PadOut("PHA", 0);
                    TCCode.PadOut("LDA " + GlobalOperand(goffset+1), 0);
                    TCCode.PadOut("ADC " + GlobalOperand(goffset2+1), 0);
                    TCCode.PadOut("PHA", 0);
                    TCCode.PadOut("LDA # 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2), 0);
                    TCCode.PadOut("PHA", 0);
                }
                
                case "LGADD":
                case "LGADDB":
                {
                    TCCode.BPOffset(instruction.Offset, "X", instruction.IsByte); 
                    TCCode.PadOut("CLC", 0);
                    if (instruction.IsByte)
                    {
                        TCCode.PadOut("LDA 0x0100, X", 0);
                        TCCode.PadOut("ADC " + GlobalOperand(instruction.Offset2), 0);
                        TCCode.PadOut("PHA", 0);
                    }
                    else
                    {
                        TCCode.PadOut("LDA 0x0101, X // LSB", 0);
                        TCCode.PadOut("ADC " + GlobalOperand(instruction.Offset2), 0);
                        TCCode.PadOut("PHA", 0);
                        TCCode.PadOut("LDA 0x0100, X // MSB", 0);
                        TCCode.PadOut("ADC " + GlobalOperand(instruction.Offset2+1), 0);
                        TCCode.PadOut("PHA", 0);
                    }
                }
                
                case "GGADDG":
                case "GGADDGB":
                {
                    int goffset  = instruction.Offset;
                    int goffset2 = instruction.Offset2;
                    int goffset3 = int(instruction.Operand);
                    TCCode.PadOut("CLC", 0);
                    TCCode.PadOut("LDA " + GlobalOperand(goffset), 0);
                    TCCode.PadOut("ADC " + GlobalOperand(goffset2), 0);
                    TCCode.PadOut("STA " + GlobalOperand(goffset3), 0);
                    if (!instruction.IsByte)
                    {
                        TCCode.PadOut("LDA " + GlobalOperand(goffset+1), 0);
                        TCCode.PadOut("ADC " + GlobalOperand(goffset2+1), 0);
                        TCCode.PadOut("STA " + GlobalOperand(goffset3+1), 0);
                    }
                }
                case "LLADDL":
                case "LLADDLB":
                {
                    TCCode.BPOffset(instruction.Offset,  "X", instruction.IsByte);
                    TCCode.BPOffset(instruction.Offset2, "Y", instruction.IsByte);
                    if (instruction.IsByte)
                    {
                        TCCode.PadOut("CLC", 0);
                        TCCode.PadOut("LDA 0x0100, X", 0);
                        TCCode.PadOut("ADC 0x0100, Y", 0);
                        TCCode.PadOut("STA 0x0100, X", 0);
                    }
                    else
                    {
                        TCCode.PadOut("CLC", 0);
                        TCCode.PadOut("LDA 0x0101, X // LSB", 0);
                        TCCode.PadOut("ADC 0x0101, Y // LSB", 0);
                        TCCode.PadOut("STA 0x0101, X // LSB", 0);
                        TCCode.PadOut("LDA 0x0100, X // MSB", 0);
                        TCCode.PadOut("ADC 0x0100, Y // MSB", 0);
                        TCCode.PadOut("STA 0x0100, X // MSB", 0);
                    }
                }
                
                
                
                case "LIANDB":
                {
                    if ((instruction.Operand).GetByte(0) == 0)
                    {
                        TCCode.PadOut("LDA # 0x00", 0);
                    }
                    else
                    {
                        TCCode.BPOffset(instruction.Offset, "X", instruction.IsByte); 
                        TCCode.PadOut("LDA 0x0100, X // LSB", 0);
                        TCCode.PadOut("AND # 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2), 0);
                    }
                    TCCode.PadOut("PHA", 0);
                }
                case "LIAND":
                {
                    if (instruction.Operand != 0)
                    {
                        TCCode.BPOffset(instruction.Offset, "X", instruction.IsByte); 
                    }
                    if ((instruction.Operand).GetByte(0) == 0)
                    {
                        TCCode.PadOut("LDA # 0x00", 0);
                    }
                    else
                    {
                        TCCode.PadOut("LDA 0x0101, X // LSB", 0);
                        TCCode.PadOut("AND # 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2), 0);
                    }
                    TCCode.PadOut("PHA", 0);
                    if ((instruction.Operand).GetByte(1) == 0)
                    {
                        TCCode.PadOut("LDA # 0x00", 0);
                    }
                    else
                    {
                        TCCode.PadOut("LDA 0x0100, X // MSB", 0);
                        TCCode.PadOut("AND # 0x" + ((instruction.Operand).GetByte(1)).ToHexString(2), 0);
                    }
                    TCCode.PadOut("PHA", 0);
                }
                
                case "LIANDFF":
                case "LIANDFFB":
                {
                    TCCode.BPOffset(instruction.Offset, "X", instruction.IsByte); 
                    if (instruction.IsByte)
                    {
                        TCCode.PadOut("LDA 0x0100, X // LSB", 0);
                        TCCode.PadOut("PHA", 0);
                    }
                    else
                    {    
                        TCCode.PadOut("LDA 0x0101, X // LSB", 0);
                        TCCode.PadOut("PHA", 0);
                        TCCode.PadOut("LDA # 0x00", 0);
                        TCCode.PadOut("PHA", 0);
                    }
                }
                case "LISHL8":
                case "LISHL8B":
                {
                    if (instruction.IsByte)
                    {
                        TCCode.PadOut("LDA # 0x00", 0);
                        TCCode.PadOut("PHA", 0);
                    }
                    else
                    {
                        TCCode.PadOut("LDA # 0x00", 0);
                        TCCode.PadOut("PHA", 0);
                        TCCode.BPOffset(instruction.Offset, "X", instruction.IsByte); 
                        TCCode.PadOut("LDA 0x0101, X // LSB", 0);
                        TCCode.PadOut("PHA", 0);
                    }
                }
                case "LISHR8":
                case "LISHR8B":
                {
                    if (instruction.IsByte)
                    {
                        TCCode.PadOut("LDA # 0x00", 0);
                        TCCode.PadOut("PHA", 0);
                    }
                    else
                    {
                        TCCode.BPOffset(instruction.Offset, "X", instruction.IsByte); 
                        TCCode.PadOut("LDA 0x0100, X // MSB", 0);
                        TCCode.PadOut("PHA", 0);
                        TCCode.PadOut("LDA # 0x00", 0);
                        TCCode.PadOut("PHA", 0);
                    }
                }
                case "IADD":
                case "IADDB":
                {
                    if (!instruction.IsByte)
                    {
                        TCCode.PadOut("PLX", 0);
                    }
                    TCCode.PadOut("CLC", 0);
                    TCCode.PadOut("PLA", 0);
                    TCCode.PadOut("ADC # 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2), 0);
                    TCCode.PadOut("PHA", 0);
                    if (!instruction.IsByte)
                    {
                        TCCode.PadOut("TXA", 0);
                        TCCode.PadOut("ADC # 0x" + ((instruction.Operand).GetByte(1)).ToHexString(2), 0);
                        TCCode.PadOut("PHA", 0);
                    }
                }
                case "IADDL":
                case "IADDLB":
                {
                    TCCode.BPOffset(instruction.Offset, "X", instruction.IsByte); 
                    if (instruction.IsByte)
                    {
                        TCCode.PadOut("CLC", 0);
                        TCCode.PadOut("PLA", 0);
                        TCCode.PadOut("ADC # 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2), 0);
                        TCCode.PadOut("STA 0x0100, X // LSB", 0);
                    }
                    else
                    {
                        TCCode.PadOut("PLY", 0);
                        TCCode.PadOut("CLC", 0);
                        TCCode.PadOut("PLA", 0);
                        TCCode.PadOut("ADC # 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2), 0);
                        TCCode.PadOut("STA 0x0101, X // LSB", 0);
                        TCCode.PadOut("TYA", 0);
                        TCCode.PadOut("ADC # 0x" + ((instruction.Operand).GetByte(1)).ToHexString(2), 0);
                        TCCode.PadOut("STA 0x0100, X // MSB", 0);
                    }
                }
                
                case "ILADD":
                case "ILADDB":
                case "LIADD":
                case "LIADDB":
                {
                    TCCode.BPOffset(instruction.Offset, "X", instruction.IsByte); 
                    TCCode.PadOut("CLC", 0);
                    if (instruction.IsByte)
                    {
                        TCCode.PadOut("LDA # 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2), 0);
                        TCCode.PadOut("ADC 0x0100, X // LSB", 0);
                        TCCode.PadOut("PHA", 0);
                    }
                    else
                    {
                        TCCode.PadOut("LDA # 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2), 0);
                        TCCode.PadOut("ADC 0x0101, X // LSB", 0);
                        TCCode.PadOut("PHA", 0);
                        TCCode.PadOut("LDA # 0x" + ((instruction.Operand).GetByte(1)).ToHexString(2), 0);
                        TCCode.PadOut("ADC 0x0100, X // MSB", 0);
                        TCCode.PadOut("PHA", 0);
                    }
                }
                case "GIADD":
                case "GIADDB":
                {
                    TCCode.PadOut("CLC", 0);
                    TCCode.PadOut("LDA # 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2), 0);
                    int goffset = instruction.Offset;
                    TCCode.PadOut("ADC " + GlobalOperand(goffset), 0);
                    TCCode.PadOut("PHA", 0);
                    if (!instruction.IsByte)
                    {
                        TCCode.PadOut("LDA # 0x" + ((instruction.Operand).GetByte(1)).ToHexString(2), 0);
                        TCCode.PadOut("ADC " + GlobalOperand(goffset+1), 0);
                        TCCode.PadOut("PHA", 0);
                    }
                }
                case "ILSUB":
                case "ILSUBB":
                {
                    TCCode.BPOffset(instruction.Offset, "X", instruction.IsByte); 
                    if (instruction.IsByte)
                    {
                        TCCode.PadOut("SEC", 0);
                        TCCode.PadOut("LDA # 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2), 0);
                        TCCode.PadOut("SBC 0x0100, X", 0);
                        TCCode.PadOut("PHA", 0);
                    }
                    else
                    {
                        TCCode.PadOut("SEC", 0);
                        TCCode.PadOut("LDA # 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2), 0);
                        TCCode.PadOut("SBC 0x0100, X // LSB", 0);
                        TCCode.PadOut("PHA", 0);
                        TCCode.PadOut("LDA # 0x" + ((instruction.Operand).GetByte(1)).ToHexString(2), 0);
                        TCCode.PadOut("SBC 0x0100, X // MSB", 0);
                        TCCode.PadOut("PHA", 0);
                    }
                }
                case "LISUB":
                case "LISUBB":
                {
                    TCCode.BPOffset(instruction.Offset, "X", instruction.IsByte); 
                    TCCode.PadOut("SEC", 0);
                    
                    if (instruction.IsByte)
                    {
                        TCCode.PadOut("LDA 0x0100, X", 0);
                        TCCode.PadOut("SBC # 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2), 0);
                        TCCode.PadOut("PHA", 0);
                    }
                    else
                    {
                        TCCode.PadOut("LDA 0x0101, X // LSB", 0);
                        TCCode.PadOut("SBC # 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2), 0);
                        TCCode.PadOut("PHA", 0);
                        TCCode.PadOut("LDA 0x0100, X // MSB", 0);
                        TCCode.PadOut("SBC # 0x" + ((instruction.Operand).GetByte(1)).ToHexString(2), 0);
                        TCCode.PadOut("PHA", 0);
                    }
                }
                case "LISHR":
                case "LISHRB":
                {
                    byte shift = (instruction.Operand).GetByte(0);
                    if (instruction.IsByte && (shift >= 8))
                    {
                        TCCode.PadOut("LDA # 0x00", 0);
                        TCCode.PadOut("PHA", 0);
                    }
                    else if (!instruction.IsByte && (shift >= 16))
                    {
                        TCCode.PadOut("LDA # 0x00", 0);
                        TCCode.PadOut("PHA", 0);
                        TCCode.PadOut("PHA", 0);
                    }
                    else if (shift == 0)
                    {
                        TCCode.BPOffset(instruction.Offset, "X", instruction.IsByte); 
                        if (instruction.IsByte)
                        {
                            TCCode.PadOut("LDA 0x0100, X", 0);
                            TCCode.PadOut("PHA", 0);
                        }
                        else
                        {
                            TCCode.PadOut("LDA 0x0101, X // LSB", 0);
                            TCCode.PadOut("PHA", 0);
                            TCCode.PadOut("LDA 0x0100, X // MSB", 0);
                            TCCode.PadOut("PHA", 0);
                        }   
                    }
                    else
                    {
                        TCCode.BPOffset(instruction.Offset, "Y", instruction.IsByte); 
                        if (instruction.IsByte)
                        {
                            TCCode.PadOut("LDA 0x0100, Y", 0);
                            TCCode.PadOut("STA ZP.NEXTL", 0);
                        }
                        else
                        {
                            TCCode.PadOut("LDA 0x0101, Y // LSB", 0);
                            TCCode.PadOut("STA ZP.NEXTL", 0);
                            TCCode.PadOut("LDA 0x0100, Y // MSB", 0);
                            TCCode.PadOut("STA ZP.NEXTH", 0);
                        }
                        TCCode.PadOut("LDX # 0x" + shift.ToHexString(2), 0);
                        TCCode.PadOut("loop", 0);
                        TCCode.PadOut("{", 0);
                        TCCode.PadOut("if (Z) { break; }", 1);
                        if (instruction.IsByte)
                        {
                            TCCode.PadOut("LSR ZP.NEXTL", 1);
                        }
                        else
                        {
                            TCCode.PadOut("LSR ZP.NEXTH", 1);
                            TCCode.PadOut("ROR ZP.NEXTL", 1);
                        }
                        TCCode.PadOut("DEX", 1);
                        TCCode.PadOut("}", 0);
                        TCCode.PadOut("LDA ZP.NEXTL", 0);
                        TCCode.PadOut("PHA", 0);
                        if (!instruction.IsByte)
                        {
                            TCCode.PadOut("LDA ZP.NEXTH", 0);
                            TCCode.PadOut("PHA", 0);
                        }
                    }
                }
                case "LISHL":
                case "LISHLB":
                {
                    byte shift = (instruction.Operand).GetByte(0);
                    if (instruction.IsByte && (shift >= 8))
                    {
                        TCCode.PadOut("LDA # 0x00", 0);
                        TCCode.PadOut("PHA", 0);
                    }
                    else if (!instruction.IsByte && (shift >= 16))
                    {
                        TCCode.PadOut("LDA # 0x00", 0);
                        TCCode.PadOut("PHA", 0);
                        TCCode.PadOut("PHA", 0);
                    }
                    else if (shift == 0)
                    {
                        TCCode.BPOffset(instruction.Offset, "X", instruction.IsByte); 
                        if (instruction.IsByte)
                        {
                            TCCode.PadOut("LDA 0x0100, X", 0);
                            TCCode.PadOut("PHA", 0);
                        }
                        else
                        {
                            TCCode.PadOut("LDA 0x0101, X, LSB", 0);
                            TCCode.PadOut("PHA", 0);
                            TCCode.PadOut("LDA 0x0100, X", 0);
                            TCCode.PadOut("PHA", 0);
                        }   
                    }
                    else
                    {
                        TCCode.BPOffset(instruction.Offset, "Y", instruction.IsByte); 
                        
                        if (instruction.IsByte)
                        {
                            TCCode.PadOut("LDA 0x0100, Y", 0);
                            TCCode.PadOut("STA ZP.NEXTL", 0);
                        }
                        else
                        {
                            TCCode.PadOut("LDA 0x0101, Y // LSB", 0);
                            TCCode.PadOut("STA ZP.NEXTL", 0);
                            TCCode.PadOut("LDA 0x0100, Y // MSB", 0);
                            TCCode.PadOut("STA ZP.NEXTH", 0);
                        }
                        TCCode.PadOut("LDX # 0x" + shift.ToHexString(2), 0);
                        TCCode.PadOut("loop", 0);
                        TCCode.PadOut("{", 0);
                        TCCode.PadOut("if (Z) { break; }", 1);
                        if (instruction.IsByte)
                        {
                            TCCode.PadOut("ASL ZP.NEXTL", 1);
                        }
                        else
                        {
                            TCCode.PadOut("ASL ZP.NEXTL", 1);
                            TCCode.PadOut("ROL ZP.NEXTH", 1);
                        }
                        TCCode.PadOut("DEX", 1);
                        TCCode.PadOut("}", 0);
                        TCCode.PadOut("LDA ZP.NEXTL", 0);
                        TCCode.PadOut("PHA", 0);
                        if (!instruction.IsByte)
                        {
                            TCCode.PadOut("LDA ZP.NEXTH", 0);
                            TCCode.PadOut("PHA", 0);
                        }
                    }
                }
                case "LIGTI":
                {
                    TCCode.BPOffset(instruction.Offset, "Y", false); 
                    TCCode.PadOut("SEC", 0);
                    TCCode.PadOut("LDA 0x0101, Y // LSB", 0);
                    TCCode.PadOut("SBC # 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2), 0);
                    TCCode.PadOut("STA ZP.TOPL", 0);
                    TCCode.PadOut("LDA 0x0100, Y // MSB", 0);
                    TCCode.PadOut("SBC # 0x" + ((instruction.Operand).GetByte(1)).ToHexString(2), 0);
                    TCCode.PadOut("STA ZP.TOPH", 0);
                    TCCode.PadOut("ASL // sign bit into carry", 0); 
                              
                    TCCode.PadOut("LDX #0  // TOP <= 0", 0);
                    TCCode.PadOut("loop", 0);
                    TCCode.PadOut("{", 0);
                    TCCode.PadOut("if (C) { break; }", 1);
                    TCCode.PadOut("//  0 or positive", 1);
                    TCCode.PadOut("LDA ZP.TOPL", 1);
                    TCCode.PadOut("if (Z)", 1);
                    TCCode.PadOut("{", 1);
                    TCCode.PadOut("LDA ZP.TOPH", 2);
                    TCCode.PadOut("if (Z) { break; }", 2);
                    TCCode.PadOut("}", 1);
                    TCCode.PadOut("LDX #1", 1);
                    TCCode.PadOut("break;", 1);
                    TCCode.PadOut("}", 0);

                    TCCode.PadOut("PHX", 0); // result in X
                }
                
                default:
                {
                    TCCode.PadOut("Generate() Not Implemented: " + name, 0);        
                }
            }
        }
        currentStream.Clear();
        InStreamMode = true;
    }
}
