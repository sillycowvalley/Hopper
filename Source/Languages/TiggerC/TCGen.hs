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
        int    Offset; // used by CALL, IF as bool
        int    Offset2;
        string Data;
    }
    
    delegate GenerateDelegate(Instruction instruction);
    
    <string, GenerateDelegate> generators;
    
    int  nestedStreamMode;
    bool capturingMode;
    bool enablePeephole;
    
    // Instructions:
    //
    //    PUSHL isByte, offset
    //    POPL  isByte, offset
    //    PUSHG isByte, offset
    //    POPG  isByte, offset
    //    PUSHM isByte ([top] could is a word address)
    //    POPM  isByte ([next] could is a word address)
    //    PUSHI isByte operand
    //    PUSHZ isByte 0
    //    ZEROG isByte offset
    //    PUSHC address (address needs to be added to Resources.StrConsts)
    //
    //    PADUNDER (cast [next] to word by inserting a zero MSB (assuming [top] is a word))
    //
    //    REM data  (comment)
    //    SP
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
    //
    //    LGADDPUSHM  : PUSHL offset0, PUSHG offset1, ADD PUSHMB
    //    LADDPUSHM   : PUSHL offset0, ADD PUSHMB
    //    ILADDPUSHM  : PUSHI operand, PUSHL offset0, ADD PUSHMB
    //    IGADDPUSHM  : PUSHI operand, PUSHG offset0, ADD PUSHMB
    //    GLADDPUSHM  : PUSHG offset0, PUSHL offset1, ADD PUSHMB
    //    GGADDPUSHM  : PUSHG offset0, PUSHG offset1, ADD PUSHMB
    //    LGADDIPOPM : PUSHL offset0, PUSHG offset1, ADD PUSHIB POPMB
    //    GGADDIPOPM : PUSHL offset0, PUSHG offset1, ADD PUSHIB POPMB
    //    IPOPM   : PUSHI operand, POPM
    //
    //    LGADDI  : PUSHL offset0, PUSHG offset1, ADD PUSHIB
    //    GGADDI  : PUSHL offset0, PUSHG offset1, ADD PUSHIB
    //    LLADDL  : PUSHL offset0, PUSHL offset1, ADD, POPL offset0 (offset0 may be the same as offset1)
    //    GGADDG  : PUSHG offset0, PUSHG offset1, ADD, POPG offset0 (offset0 may be the same as offset1)
    //    GGMULI  : PUSHG offset0, PUSHG offset1, MULI
    //    GGDIVI  : PUSHG offset0, PUSHG offset1, DIVI
    //    INCLI   : PUSHL offset0, PUSHI, ADD, POPL offset0
    //    INCGI   : PUSHG offset0, PUSHI, ADD, POPG offset0
    //    STLI    : PUSHI, POPL offset0
    //    STGI    : PUSHI, POPG offset0
    //    LISHR   : PUSHL offset, PUSHI, SHR
    //    --
    //    LISHR8  : PUSHL offset, PUSHI 0x0008, SHR
    //    --
    //    LISHL   : PUSHL offset, PUSHI, SHL
    //    --
    //    LIAND   : PUSHL offset, PUSHI, AND
    //    GIAND   : PUSHG offset, PUSHI, AND
    //    LIANDFF : PUSHL offset, PUSHI 0x00FF, AND
    //    GIANDFF : PUSHG offset, PUSHI 0x00FF, AND
    //    GIANDFFC: PUSHG offset, PUSHI 0x00FF, AND, DECSP 1
    //    ILADD   : PUSHI, PUSHL offset, ADD
    //    IGADD   : PUSHI, PUSHG offset, ADD
    //    ILSUB   : PUSHI, PUSHL offset, SUB
    //    --
    //    IADD    : PUSHI, ADD
    //    LADD    : PUSHL, ADD
    //    IADDC   : PUSHI, ADD, DECSP 1
    //    IADDL   : PUSHI, ADD, POPL offset0
    //    --
    //    LIADD   : PUSHL offset, PUSHI, ADD
    //    GIADD   : PUSHG offset, PUSHI, ADD
    //    LISUB   : PUSHL offset, PUSHI, SUB
    //    --
    //    LTX     : LT, LOOPEXIT
    //    LEX     : LE, LOOPEXIT
    //    LILE    : PUSHL offset, PUSHI, LE
    //    GILE    : PUSHG offset, PUSHI, LE
    //    LILT    : PUSHL offset, PUSHI, LT
    //    GILT    : PUSHG offset, PUSHI, LT
    //    LIGTI   : PUSHL offset, PUSHI, GTI
    //    --
    //    LILEX   : PUSHL offset, PUSHI, LE, LOOPEXIT
    //    GILEX   : PUSHG offset, PUSHI, LE, LOOPEXIT
    //    LILTX   : PUSHL offset, PUSHI, LT, LOOPEXIT
    //    GILTX   : PUSHG offset, PUSHI, LT, LOOPEXIT
    //
    //    IEQ     : PUSHI operand, EQ
    //    INE     : PUSHI operand, NE
    //    INEX    : PUSHI operand, NE, LOOPEXIT
    //    IEQZIF  : PUSHI 0x0000,  EQ, IF
    //    INEZIF  : PUSHI 0x0000,  NE, IF
    //
    //    PUSHM1  : PUSHI 0, PUSH 1, SUB
    
     
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
            if (instruction0.Name == "STGI")
            {
                if (instruction1.Name == "ZEROG") 
                {
                    if (instruction0.IsByte == instruction1.IsByte)
                    {
                        if (instruction0.Offset == int(instruction1.Operand))
                        {
                            if (instruction0.Operand == 0)
                            {
                                DeleteInstruction(currentStream.Count-1);
                                modified = true;
                                break;
                            }
                        }
                    }
                }
            }
            if ((instruction0.Name == "IF"))
            {
                if ((instruction1.Name == "IEQ") && (instruction1.Operand == 0))
                {
                    //Print(" IEQZIF");
                    instruction0.Name    = "IEQZIF";
                    instruction0.IsByte  = instruction1.IsByte;
                    currentStream[currentStream.Count-1] = instruction0;
                    DeleteInstruction(currentStream.Count-2);
                    modified = true;
                    break;
                }
                if ((instruction1.Name == "INE") && (instruction1.Operand == 0))
                {
                    //Print(" INEZIF");
                    instruction0.Name    = "INEZIF";
                    instruction0.IsByte  = instruction1.IsByte;
                    currentStream[currentStream.Count-1] = instruction0;
                    DeleteInstruction(currentStream.Count-2);
                    modified = true;
                    break;
                }
            }
            if ((instruction0.Name == "LOOPEXIT"))
            {
                if (instruction1.IsByte && (instruction1.Name == "PUSHI") && (instruction1.Operand != 0))
                {
                    // while true
                    DeleteInstruction(currentStream.Count-2);
                    DeleteInstruction(currentStream.Count-1);
                    modified = true;
                    break;
                }
                if (instruction1.Name == "INE") 
                {
                    //Print(" INEX");
                    instruction1.Name    = "INEX";
                    instruction1.Data = instruction0.Data;
                    currentStream[currentStream.Count-2] = instruction1;
                    DeleteInstruction(currentStream.Count-1);
                    modified = true;
                    break;
                }
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
                if (instruction1.Name == "GILE")
                {
                    //Print(" GILEX");
                    instruction1.Name    = "GILEX";
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
                    if (instruction1.Name == "PUSHL")
                    {
                        //Print(" LADD");
                        instruction1.Name    = "LADD";
                        currentStream[currentStream.Count-2] = instruction1;
                        DeleteInstruction(currentStream.Count-1);
                        modified = true;
                        break;
                    }    
                }
            }
            if ((instruction0.Name == "LADD"))
            {
                if (instruction1.IsByte == instruction0.IsByte)
                {
                    if (instruction1.Name == "PUSHI")
                    {
                        //Print(" ILADD");
                        instruction1.Name   = "ILADD";
                        instruction1.Offset = instruction0.Offset;
                        currentStream[currentStream.Count-2] = instruction1;
                        DeleteInstruction(currentStream.Count-1);
                        modified = true;
                        break;
                    }
                }
            }
            if (instruction0.Name == "EQ")
            {
                if (instruction1.IsByte == instruction0.IsByte)
                {
                    if (instruction1.Name == "PUSHI")
                    {
                        //Print(" IEQ");
                        instruction1.Name    = "IEQ";
                        currentStream[currentStream.Count-2] = instruction1;
                        DeleteInstruction(currentStream.Count-1);
                        modified = true;
                        break;
                    }   
                }
            }
            if (instruction0.Name == "NE")
            {
                if (instruction1.IsByte == instruction0.IsByte)
                {
                    if (instruction1.Name == "PUSHI")
                    {
                        //Print(" INE");
                        instruction1.Name    = "INE";
                        currentStream[currentStream.Count-2] = instruction1;
                        DeleteInstruction(currentStream.Count-1);
                        modified = true;
                        break;
                    }   
                }
            }
                                   
            if (instruction0.Name == "DECSP")
            {
                if (instruction0.Operand == 1)
                {
                    if (!instruction1.IsByte)
                    {
                        if (instruction1.Name == "IADD")
                        {
                            //Print(" IADDC");
                            instruction1.Name    = "IADDC";
                            currentStream[currentStream.Count-2] = instruction1;
                            DeleteInstruction(currentStream.Count-1);
                            modified = true;
                            break;
                        }
                        if (instruction1.Name == "GIANDFF")
                        {
                            //Print(" GIANDFFC");    
                            instruction1.Name    = "GIANDFFC";
                            currentStream[currentStream.Count-2] = instruction1;
                            DeleteInstruction(currentStream.Count-1);
                            modified = true;
                            break;
                        }
                    }
                    if (instruction1.IsByte)
                    {
                        if (instruction1.Name == "PUSHL") // PUSHLB DECSP 1
                        {
                            //Print(" [PUSHLB DECSP 1]");
                            DeleteInstruction(currentStream.Count-2);
                            DeleteInstruction(currentStream.Count-1);
                            modified = true;
                            break;
                        }
                        if (instruction1.Name == "PUSHG") // PUSHGB DECSP 1
                        {
                            DeleteInstruction(currentStream.Count-2);
                            DeleteInstruction(currentStream.Count-1);
                            modified = true;
                            break;
                        }
                        if (instruction1.Name == "PUSHM") // PUSHMB DECSP 1
                        {
                            //Print(" [PUSHMB DECSP 1]");
                            instruction0.Operand = 2; // the index argument of PUSHMB is always 2 bytes
                            currentStream[currentStream.Count-1] = instruction0;
                            DeleteInstruction(currentStream.Count-2);
                            modified = true;
                            break;
                        }
                    }
                }
                if (instruction0.Operand == 2)
                {
                    if (!instruction1.IsByte)
                    {
                        if (instruction1.Name == "PUSHL") // PUSHL DECSP 2
                        {
                            //Print(" [PUSHL DECSP 2]");
                            DeleteInstruction(currentStream.Count-2);
                            DeleteInstruction(currentStream.Count-1);
                            modified = true;
                            break;
                        }
                        if (instruction1.Name == "PUSHG") // PUSHG DECSP 2
                        {
                            DeleteInstruction(currentStream.Count-2);
                            DeleteInstruction(currentStream.Count-1);
                            modified = true;
                            break;
                        }
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
            if ((instruction0.Name == "PUSHZ"))
            {
                if (instruction0.IsByte && instruction1.IsByte && (instruction1.Name == "PUSHI")) // 0 -> CAST to word
                { 
                    //Print(" IZ->I");
                    instruction0.Name = "PUSHI";
                    instruction0.Operand = instruction1.Operand; // MSB is zero
                    instruction0.IsByte = false;
                    currentStream[currentStream.Count-2] = instruction0;
                    DeleteInstruction(currentStream.Count-1);
                    modified = true;
                    break;
                }
            }
            if ((instruction0.Name == "PUSHI"))
            {
                if (instruction0.IsByte)
                { 
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
                /*
                if (instruction0.IsByte == instruction1.IsByte)
                {
                    if (instruction1.Name == "PUSHG")
                    {
                        Print(" PUSHGI");
                        instruction1.Name    = "PUSHGI";
                        instruction1.Operand = instruction0.Operand;
                        currentStream[currentStream.Count-2] = instruction1;
                        DeleteInstruction(currentStream.Count-1);
                        modified = true;
                        break;
                    }
                }
                */
            }
            if ((instruction0.Name == "IPOPM"))
            {
                if (instruction0.IsByte)
                {
                    if (!instruction1.IsByte)
                    {
                        if (instruction1.Name == "GGADD")
                        {
                            //Print(" GGADDIPOPM");
                            instruction1.Name    = "GGADDIPOPM";
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
                        Print(" STLI");
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
                    if (instruction1.Name == "PUSHI") 
                    {
                        //Print(" STGI");
                        instruction0.Operand = instruction1.Operand;
                        instruction0.Name = "STGI";
                        currentStream[currentStream.Count-1] = instruction0;
                        DeleteInstruction(currentStream.Count-2);
                        modified = true;
                        break;
                    }
                    if ((instruction1.Name == "IGADD") && (instruction1.Offset == instruction0.Offset))
                    {   
                        Print(" INCGI");
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
                    if (instruction1.Name == "IADD")
                    {   
                        //Print(" IADDG");
                        instruction1.Name    = "IADDG";
                        instruction1.Offset = instruction0.Offset;
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
                        //Print(" LGADDPUSHM");
                        instruction1.Name    = "LGADDPUSHM";
                        currentStream[currentStream.Count-2] = instruction1;
                        DeleteInstruction(currentStream.Count-1);
                        modified = true;
                        break;
                    }
                    if (instruction1.Name == "LADD")
                    {
                        //Print(" LADDPUSHM");
                        instruction1.Name    = "LADDPUSHM";
                        currentStream[currentStream.Count-2] = instruction1;
                        DeleteInstruction(currentStream.Count-1);
                        modified = true;
                        break;
                    }
                }
            }
            if ((instruction0.Name == "LADDPUSHM"))
            {
                if (instruction0.IsByte == instruction1.IsByte)
                {
                    if (instruction1.Name == "PUSHG")
                    {
                        //Print(" GLADDPUSHM");
                        instruction1.Name    = "GLADDPUSHM";
                        instruction1.Offset2 = instruction0.Offset;
                        currentStream[currentStream.Count-2] = instruction1;
                        DeleteInstruction(currentStream.Count-1);
                        modified = true;
                        break;
                    }
                }
            }
            if (instruction0.Name == "LADDPUSHM")
            {
                if (instruction0.IsByte == instruction1.IsByte)
                {
                    if (instruction1.Name == "PUSHI")
                    {
                        //Print(" ILADDPUSHM");
                        instruction1.Name    = "ILADDPUSHM";
                        instruction1.Offset = instruction0.Offset;
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
                        //Print(" GGADDPUSHM");
                        instruction1.Name    = "GGADDPUSHM";
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
                    if (instruction1.Name == "IGADD")
                    {
                        //Print(" IGADDPUSHM");
                        instruction1.Name    = "IGADDPUSHM";
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
                    if (instruction1.Name == "ILADD")
                    {
                        //Print(" ILADDPUSHM");
                        instruction1.Name    = "ILADDPUSHM";
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
                        //Print(" LGADDIPOPM");
                        instruction1.Name    = "LGADDIPOPM";
                        currentStream[currentStream.Count-2] = instruction1;
                        DeleteInstruction(currentStream.Count-1);
                        modified = true;
                        break;
                    }
                    if (instruction1.Name == "GGADDI")
                    {
                        //Print(" GGADDIPOPM");
                        instruction1.Name    = "GGADDIPOPM";
                        currentStream[currentStream.Count-2] = instruction1;
                        DeleteInstruction(currentStream.Count-1);
                        modified = true;
                        break;
                    }
                }
                if (instruction1.IsByte == instruction0.IsByte)
                {
                    if (instruction1.Name == "PUSHI")
                    {
                        //Print(" IPOPM");
                        instruction1.Name                   = "IPOPM";
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
            if ((instruction2.Name == "PUSHI") && (instruction1.Name == "PUSHI") && (instruction0.Name == "SUB"))
            {
                if (!instruction2.IsByte && !instruction1.IsByte && !instruction0.IsByte)
                {
                    if ((instruction2.Operand == 0) && (instruction1.Operand == 1))
                    {
                        //Print(" PUSHM1");
                        instruction2.Operand = 0xFFFF;
                        currentStream[currentStream.Count-3] = instruction2;
                        DeleteInstruction(currentStream.Count-1);
                        DeleteInstruction(currentStream.Count-1);
                        modified = true;
                        break;
                    }
                }
            }
            if ((instruction2.Name == "DUP") && (instruction1.Name == "IPOPM") && (instruction0.Name == "DECSP"))
            { 
                if (!instruction2.IsByte && (instruction0.Operand == 2))
                {
                    DeleteInstruction(currentStream.Count-3);
                    DeleteInstruction(currentStream.Count-1);
                    modified = true;
                    break;
                }
            }
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
                if ((instruction2.IsByte == instruction1.IsByte) && (instruction1.IsByte == instruction0.IsByte))
                {
                    if (instruction0.Name == "ADD")
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
                    if (instruction0.Name == "MULI")
                    {
                        //Print(" GGMULI");
                        instruction2.Offset2 = instruction1.Offset;
                        instruction2.Name    = "GGMULI";
                        currentStream[currentStream.Count-3] = instruction2;
                        DeleteInstruction(currentStream.Count-1);
                        DeleteInstruction(currentStream.Count-1);
                        modified = true;
                        break;
                    }
                    if (instruction0.Name == "DIVI")
                    {
                        //Print(" GGDIVI");
                        instruction2.Offset2 = instruction1.Offset;
                        instruction2.Name    = "GGDIVI";
                        currentStream[currentStream.Count-3] = instruction2;
                        DeleteInstruction(currentStream.Count-1);
                        DeleteInstruction(currentStream.Count-1);
                        modified = true;
                        break;
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
                        
            if ((instruction2.Name == "PUSHI") && (instruction1.Name == "PUSHG"))
            {
                if (instruction2.IsByte == instruction1.IsByte)
                {
                    if (instruction1.IsByte == instruction0.IsByte)
                    {
                        if ((instruction0.Name == "ADD"))
                        {
                            //Print(" IGADD3");
                            instruction2.Offset = instruction1.Offset;
                            instruction2.Name = "IGADD";
                            currentStream[currentStream.Count-3] = instruction2;
                            DeleteInstruction(currentStream.Count-2);
                            DeleteInstruction(currentStream.Count-1);
                            modified = true;
                            break;
                        }
                        if ((instruction0.Name == "SUB"))
                        {
                            Print(" IGSUB3");
                            instruction2.Offset = instruction1.Offset;
                            instruction2.Name = "IGSUB";
                            currentStream[currentStream.Count-3] = instruction2;
                            DeleteInstruction(currentStream.Count-2);
                            DeleteInstruction(currentStream.Count-1);
                            modified = true;
                            break;
                        }
                    }           
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
                        if (instruction0.Name == "LE")
                        {
                            //Print(" GILE3");
                            instruction2.Operand = instruction1.Operand;
                            instruction2.Name = "GILE";
                            currentStream[currentStream.Count-3] = instruction2;
                            DeleteInstruction(currentStream.Count-2);
                            DeleteInstruction(currentStream.Count-1);
                            modified = true;
                            break;
                        }
                        if (instruction0.Name == "AND")
                        {
                            if (instruction1.Operand == 0x00FF)
                            {
                                //Print(" GIANDFF3");
                                instruction2.Name = "GIANDFF";
                                currentStream[currentStream.Count-3] = instruction2;
                                DeleteInstruction(currentStream.Count-2);
                                DeleteInstruction(currentStream.Count-1);
                            }
                            else
                            {
                                Print(" GIAND3");
                                instruction2.Operand = instruction1.Operand;
                                instruction2.Name = "GIAND";
                                currentStream[currentStream.Count-3] = instruction2;
                                DeleteInstruction(currentStream.Count-2);
                                DeleteInstruction(currentStream.Count-1);
                            }
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
            if ((instruction3.Name == "PUSHI") && (instruction2.Name == "PUSHI") && (instruction1.Name == "PUSHL") && (instruction0.Name == "ADD"))
            {
                if (!instruction3.IsByte && instruction2.IsByte && !instruction1.IsByte && !instruction0.IsByte)    
                {
                    // PUSHI 0x0200, PUSHIB 0x00, PUSHL [BP+0xFE], ADD
                    //Print(" PUSHI <-> PUSHIB");
                    uint operand3 = instruction3.Operand;
                    uint operand2 = instruction2.Operand;
                    
                    instruction3.Operand = (operand3 & 0xFF);
                    instruction3.IsByte  = true;
                    currentStream[currentStream.Count-4] = instruction3;
                    
                    instruction2.Operand = ((operand3 >> 8) & 0xFF) + (operand2 << 8);
                    instruction2.IsByte  = false;
                    currentStream[currentStream.Count-3] = instruction2;
                        
                    modified = true;
                    break;
                }
            }
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
        if (isByteIndex)
        {
            Die(0x0A);
        }
        Append("PUSHM", isByte);
    }
    PopMemory(bool isByteIndex, bool isByte)
    {
        if (isByteIndex)
        {
            Die(0x0A);
        }
        Append("POPM", isByte);
    }
    ZeroGlobal(bool isByte, uint operand)
    {
        Append("ZEROG", isByte, operand);
    }
    PushImmediate(bool isByte, uint literal)
    {
        Append("PUSHI", isByte, literal);
    }
    PushCastZero()
    {
        Append("PUSHZ", true, uint(0));
    }
    PadUnder()
    {
        Append("PADUNDER");
    }
    PushConst(uint literal)
    {
        Append("PUSHC", false, literal);
    }
    SP()
    {
        Append("SP");
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
    
    IF(bool hasBrace)
    {
        Instruction instruction;
        instruction.Name = "IF";
        instruction.Operand = (hasBrace ? 1 : 0);
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
            case "SP":
            {
                content = "SP";
            }
            case "LOOPEXIT":
            {
                content = instruction.Name;
            }
            case "PUSHZ":
            {
                content = instruction.Name+width;
            }
            case "PUSHI":
            {
                content = "PUSHI"+width+" 0x" + (instruction.Operand).ToHexString(instruction.IsByte ? 2 : 4);
            }
            case "ZEROG":
            {
                content = "ZEROG"+width+" [" + GlobalOperand(int(instruction.Operand)) + "]";
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
            case "IPOPM":
            {
                content = "IPOPM"+width;
            }
            case "DUP":
            {
                content = "DUP"+width;
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
            
            //case "PUSHGI":
            //{
            //    content = "PUSHGI"+width+" [" + GlobalOperand(instruction.Offset) + "] # 0x" + (instruction.Operand).ToHexString(instruction.IsByte ? 2 : 4);
            //}
            
            case "IEQ":
            case "INE":
            case "INEX":
            {
                content = instruction.Name+width+" 0x" + (instruction.Operand).ToHexString(instruction.IsByte ? 2 : 4);
            }
            case "IEQZIF":
            case "INEZIF":
            {
                content = instruction.Name+width+" 0x" + (0).ToHexString(instruction.IsByte ? 2 : 4);
            }
            
            
            
            
            case "LGADD":
            case "LGADDPUSHM":
            {
                content = instruction.Name + width + " [BP+" + OffsetToHex(instruction.Offset) + "] [" + GlobalOperand(instruction.Offset2) + "]";
            }
            case "GLADD":
            case "GLADDPUSHM":
            {
                content = instruction.Name + width + " [" + GlobalOperand(instruction.Offset) + "] [BP+" + OffsetToHex(instruction.Offset2) + "]";
            }
            
            case "IGADDPUSHM":
            {
                content = instruction.Name + width + " # 0x" + (instruction.Operand).ToHexString(instruction.IsByte ? 2 : 4) + " [" + GlobalOperand(instruction.Offset) + "]";
            }
            
            case "ILADDPUSHM":
            {
                content = instruction.Name + width + " # 0x" + (instruction.Operand).ToHexString(instruction.IsByte ? 2 : 4) + " [BP+" + OffsetToHex(instruction.Offset) + "]";
            }
            case "LADDPUSHM":
            {
                content = instruction.Name + width + " [BP+" + OffsetToHex(instruction.Offset) + "]";
            }
            case "LGADDI":
            case "LGADDIPOPM":
            {
                content = instruction.Name + width + " [BP+" + OffsetToHex(instruction.Offset) + "] [" + GlobalOperand(instruction.Offset2) + "] # 0x" + (instruction.Operand).ToHexString(instruction.IsByte ? 2 : 4);
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
            case "GGMULI":
            case "GGDIVI":
            case "GGADDPUSHM":
            {
                content = instruction.Name + width + " [" + GlobalOperand(instruction.Offset) + "] [" +GlobalOperand(instruction.Offset2) + "]";
            }
            case "GGADDG":
            {
                content = instruction.Name + width + " [" + GlobalOperand(instruction.Offset) + "] [" +GlobalOperand(instruction.Offset2) + "] [" +GlobalOperand(int(instruction.Operand)) + "]";
            }
            
            case "GGADDI":
            case "GGADDIPOPM":
            {
                content = instruction.Name + width + " [" + GlobalOperand(instruction.Offset) + "] [" +GlobalOperand(instruction.Offset2) + "] # 0x" + (instruction.Operand).ToHexString(instruction.IsByte ? 2 : 4);
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
            case "STGI":
            case "GILT":
            case "GILE":
            case "INCGI":
            case "GIADD":
            case "GILTX":
            case "GILEX":
            case "GIAND":
            {
                content = instruction.Name + width + " [" + GlobalOperand(instruction.Offset) + "] # 0x" + (instruction.Operand).ToHexString(instruction.IsByte ? 2 : 4);
            }
            case "IADD":
            case "IADDC":
            {
                content = instruction.Name + width + " # 0x" + (instruction.Operand).ToHexString(instruction.IsByte ? 2 : 4);
            }
            case "IADDL":
            {
                content = instruction.Name + width + " # 0x" + (instruction.Operand).ToHexString(instruction.IsByte ? 2 : 4) + " [BP+" + OffsetToHex(instruction.Offset) + "]";
            }
            case "IADDG":
            {
                content = instruction.Name + width + " # 0x" + (instruction.Operand).ToHexString(instruction.IsByte ? 2 : 4) + " [" + GlobalOperand(instruction.Offset) + "]";
            }
            case "LTX":
            case "LEX":
            {
                content = instruction.Name + width + " " +instruction.Data;
            }
            case "LADD":
            {
                content = instruction.Name + width + " [BP+" + OffsetToHex(instruction.Offset) + "]";
            }    
            case "LIANDFF":
            {
                content = instruction.Name + width + " [BP+" + OffsetToHex(instruction.Offset) + "] # 0x00FF";
            }
            case "GIANDFF":
            case "GIANDFFC":
            {
                content = instruction.Name + width + " [" + GlobalOperand(instruction.Offset) + "] # 0x00FF";
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
            case "IGADD":
            case "IGSUB":
            {
                content = instruction.Name + width + " # 0x" + (instruction.Operand).ToHexString(instruction.IsByte ? 2 : 4) + " [" + GlobalOperand(instruction.Offset) + "]";
            }
            
            default:
            {
                Print(" ToString() Not Implemented: '" + instruction.Name + "'");
                Die(0x0A);
            }
        }
        return content;
    }
    
    Initialize()
    {
        //<string, GenerateDelegate> generators;
        generators.Clear();
        GenerateDelegate generateDelegate;
        
        generateDelegate = generateREM;
        generators["REM"] = generateDelegate;

        generateDelegate = generatePUSHI;
        generators["PUSHI"] = generateDelegate;
        generateDelegate = generatePUSHIB;
        generators["PUSHIB"] = generateDelegate;
        
        generateDelegate = generatePUSHZ;
        generators["PUSHZ"] = generateDelegate;
        generateDelegate = generatePUSHZB;
        generators["PUSHZB"] = generateDelegate;
        
        
        generateDelegate = generateINE;
        generators["INE"] = generateDelegate;
        generateDelegate = generateINEB;
        generators["INEB"] = generateDelegate;
        generateDelegate = generateIEQ;
        generators["IEQ"] = generateDelegate;
        generateDelegate = generateIEQB;
        generators["IEQB"] = generateDelegate;
        generateDelegate = generateINEX;
        generators["INEX"] = generateDelegate;
        generateDelegate = generateINEXB;
        generators["INEXB"] = generateDelegate;
        
        generateDelegate = generateIEQZIF;
        generators["IEQZIF"] = generateDelegate;
        generateDelegate = generateIEQZIFB;
        generators["IEQZIFB"] = generateDelegate;
        
        generateDelegate = generateINEZIF;
        generators["INEZIF"] = generateDelegate;
        generateDelegate = generateINEZIFB;
        generators["INEZIFB"] = generateDelegate;
        
        generateDelegate = generateSP;
        generators["SP"] = generateDelegate;

        generateDelegate = generateZEROG;
        generators["ZEROG"] = generateDelegate;
        generators["ZEROGB"] = generateDelegate;

        generateDelegate = generatePOPL;
        generators["POPL"] = generateDelegate;
        generators["POPLB"] = generateDelegate;

        generateDelegate = generatePOPG;
        generators["POPG"] = generateDelegate;
        generators["POPGB"] = generateDelegate;

        generateDelegate = generatePUSHL;
        generators["PUSHL"] = generateDelegate;
        generators["PUSHLB"] = generateDelegate;

        generateDelegate = generatePUSHG;
        generators["PUSHG"] = generateDelegate;
        generators["PUSHGB"] = generateDelegate;

        //generateDelegate = generatePUSHGI;
        //generators["PUSHGI"] = generateDelegate;
        //generators["PUSHGIB"] = generateDelegate;
        
        generateDelegate = generatePUSHM;
        generators["PUSHM"] = generateDelegate;
        generators["PUSHMB"] = generateDelegate;

        generateDelegate = generatePOPM;
        generators["POPM"] = generateDelegate;
        generators["POPMB"] = generateDelegate;
        
        generateDelegate = generateIPOPM;
        generators["IPOPM"] = generateDelegate;
        generators["IPOPMB"] = generateDelegate;

        generateDelegate = generatePUSHC;
        generators["PUSHC"] = generateDelegate;

        generateDelegate = generateADD;
        generators["ADD"] = generateDelegate;
        generators["ADDB"] = generateDelegate;

        generateDelegate = generateSUB;
        generators["SUB"] = generateDelegate;
        generators["SUBB"] = generateDelegate;

        generateDelegate = generateMUL;
        generators["MUL"] = generateDelegate;
        generators["MULB"] = generateDelegate;

        generateDelegate = generateDIV;
        generators["DIV"] = generateDelegate;
        generators["DIVB"] = generateDelegate;

        generateDelegate = generateMOD;
        generators["MOD"] = generateDelegate;
        generators["MODB"] = generateDelegate;

        generateDelegate = generateMULI;
        generators["MULI"] = generateDelegate;

        generateDelegate = generateDIVI;
        generators["DIVI"] = generateDelegate;

        generateDelegate = generateMODI;
        generators["MODI"] = generateDelegate;

        generateDelegate = generateEQ;
        generators["EQ"] = generateDelegate;
        generators["EQB"] = generateDelegate;

        generateDelegate = generateNE;
        generators["NE"] = generateDelegate;
        generators["NEB"] = generateDelegate;

        generateDelegate = generateGT;
        generators["GT"] = generateDelegate;
        generators["GTB"] = generateDelegate;

        generateDelegate = generateGE;
        generators["GE"] = generateDelegate;
        generators["GEB"] = generateDelegate;

        generateDelegate = generateLTI;
        generators["LTI"] = generateDelegate;
        generators["LTIB"] = generateDelegate;

        generateDelegate = generateLEI;
        generators["LEI"] = generateDelegate;
        generators["LEIB"] = generateDelegate;

        generateDelegate = generateGTI;
        generators["GTI"] = generateDelegate;
        generators["GTIB"] = generateDelegate;

        generateDelegate = generateGEI;
        generators["GEI"] = generateDelegate;
        generators["GEIB"] = generateDelegate;

        generateDelegate = generateSHL;
        generators["SHL"] = generateDelegate;
        generators["SHLB"] = generateDelegate;

        generateDelegate = generateSHR;
        generators["SHR"] = generateDelegate;
        generators["SHRB"] = generateDelegate;

        generateDelegate = generateAND;
        generators["AND"] = generateDelegate;
        generators["ANDB"] = generateDelegate;

        generateDelegate = generateOR;
        generators["OR"] = generateDelegate;
        generators["ORB"] = generateDelegate;

        generateDelegate = generateXOR;
        generators["XOR"] = generateDelegate;
        generators["XORB"] = generateDelegate;

        generateDelegate = generateNOT;
        generators["NOT"] = generateDelegate;
        generators["NOTB"] = generateDelegate;

        generateDelegate = generateBOOLNOTB;
        generators["BOOLNOTB"] = generateDelegate;

        generateDelegate = generateLOOPEXIT;
        generators["LOOPEXIT"] = generateDelegate;

        generateDelegate = generatePADUNDER;
        generators["PADUNDER"] = generateDelegate;

        generateDelegate = generateDECSP;
        generators["DECSP"] = generateDelegate;
        
        generateDelegate = generateDUP;
        generators["DUPB"] = generateDelegate;
        generators["DUP"] = generateDelegate;

        generateDelegate = generateIF;
        generators["IF"] = generateDelegate;

        generateDelegate = generateELSE;
        generators["ELSE"] = generateDelegate;

        generateDelegate = generateENDIF;
        generators["ENDIF"] = generateDelegate;

        generateDelegate = generateLT;
        generators["LT"] = generateDelegate;
        generators["LTB"] = generateDelegate;

        generateDelegate = generateLE;
        generators["LE"] = generateDelegate;
        generators["LEB"] = generateDelegate;

        generateDelegate = generateCALL;
        generators["CALL"] = generateDelegate;

        generateDelegate = generateINCLI;
        generators["INCLI"] = generateDelegate;
        generators["INCLIB"] = generateDelegate;

        generateDelegate = generateINCGI;
        generators["INCGI"] = generateDelegate;
        generators["INCGIB"] = generateDelegate;

        generateDelegate = generateLEX;
        generators["LEX"] = generateDelegate;
        generators["LEXB"] = generateDelegate;

        generateDelegate = generateLTX;
        generators["LTX"] = generateDelegate;
        generators["LTXB"] = generateDelegate;

        generateDelegate = generateLILE;
        generators["LILE"] = generateDelegate;
        generators["LILEB"] = generateDelegate;

        generateDelegate = generateLILT;
        generators["LILT"] = generateDelegate;
        generators["LILTB"] = generateDelegate;

        generateDelegate = generateGILE;
        generators["GILE"] = generateDelegate;
        generators["GILEB"] = generateDelegate;

        generateDelegate = generateGILT;
        generators["GILT"] = generateDelegate;
        generators["GILTB"] = generateDelegate;

        generateDelegate = generateGILTX;
        generators["GILTX"] = generateDelegate;
        generators["GILTXB"] = generateDelegate;

        generateDelegate = generateGILEX;
        generators["GILEX"] = generateDelegate;

        generateDelegate = generateGILEX;
        generators["GILEX"] = generateDelegate;
        generators["GILEXB"] = generateDelegate;

        generateDelegate = generateLILEX;
        generators["LILEX"] = generateDelegate;
        generators["LILEXB"] = generateDelegate;

        generateDelegate = generateLILTX;
        generators["LILTX"] = generateDelegate;
        generators["LILTXB"] = generateDelegate;

        generateDelegate = generateSTLI;
        generators["STLI"] = generateDelegate;
        generators["STLIB"] = generateDelegate;

        generateDelegate = generate2L;
        generators["2L"] = generateDelegate;
        generators["2LB"] = generateDelegate;

        generateDelegate = generate2G;
        generators["2G"] = generateDelegate;
        generators["2GB"] = generateDelegate;

        generateDelegate = generateSTGI;
        generators["STGI"] = generateDelegate;
        generators["STGIB"] = generateDelegate;

        generateDelegate = generateGGADD;
        generators["GGADD"] = generateDelegate;
        generators["GGADDB"] = generateDelegate;

        generateDelegate = generateGGMULI;
        generators["GGMULI"] = generateDelegate;
        
        generateDelegate = generateGGDIVI;
        generators["GGDIVI"] = generateDelegate;
        
        generateDelegate = generateGGADDG;
        generators["GGADDG"] = generateDelegate;
        generators["GGADDGB"] = generateDelegate;

        generateDelegate = generateLLADD;
        generators["LLADD"] = generateDelegate;
        generators["LLADDB"] = generateDelegate;

        generateDelegate = generateGGADDPUSHM;
        generators["GGADDPUSHM"] = generateDelegate;

        generateDelegate = generateLGADDPUSHM;
        generators["LGADDPUSHM"] = generateDelegate;
        
        generateDelegate = generateGLADDPUSHM;
        generators["GLADDPUSHM"] = generateDelegate;
        
        generateDelegate = generateILADDPUSHM;
        generators["ILADDPUSHM"] = generateDelegate;
        
        generateDelegate = generateIGADDPUSHM;
        generators["IGADDPUSHM"] = generateDelegate;
        
        
        generateDelegate = generateLADDPUSHM;
        generators["LADDPUSHM"] = generateDelegate;

        generateDelegate = generateLGADDIPOPM;
        generators["LGADDIPOPM"] = generateDelegate;

        generateDelegate = generateLGADDI;
        generators["LGADDI"] = generateDelegate;

        generateDelegate = generateGGADDIPOPM;
        generators["GGADDIPOPM"] = generateDelegate;

        generateDelegate = generateGGADDI;
        generators["GGADDI"] = generateDelegate;

        generateDelegate = generateLGADD;
        generators["LGADD"] = generateDelegate;
        generators["LGADDB"] = generateDelegate;

        generateDelegate = generateLLADDL;
        generators["LLADDL"] = generateDelegate;
        generators["LLADDLB"] = generateDelegate;

        generateDelegate = generateGIANDB;
        generators["GIANDB"] = generateDelegate;

        generateDelegate = generateGIAND;
        generators["GIAND"] = generateDelegate;

        generateDelegate = generateGIANDFF;
        generators["GIANDFF"] = generateDelegate;

        generateDelegate = generateGIANDFFB;
        generators["GIANDFFB"] = generateDelegate;
        generators["GIANDFFC"] = generateDelegate;

        generateDelegate = generateLIANDB;
        generators["LIANDB"] = generateDelegate;

        generateDelegate = generateLIAND;
        generators["LIAND"] = generateDelegate;

        generateDelegate = generateLIANDFF;
        generators["LIANDFF"] = generateDelegate;
        generators["LIANDFFB"] = generateDelegate;

        generateDelegate = generateLISHL8;
        generators["LISHL8"] = generateDelegate;
        generators["LISHL8B"] = generateDelegate;

        generateDelegate = generateLISHR8;
        generators["LISHR8"] = generateDelegate;
        generators["LISHR8B"] = generateDelegate;

        generateDelegate = generateIADD;
        generators["IADD"] = generateDelegate;
        generators["IADDB"] = generateDelegate;
        
        generateDelegate = generateLADD;
        generators["LADD"] = generateDelegate;
        generators["LADDB"] = generateDelegate;

        generateDelegate = generateIADDC;
        generators["IADDC"] = generateDelegate;

        generateDelegate = generateIADDL;
        generators["IADDL"] = generateDelegate;
        generators["IADDLB"] = generateDelegate;
        
        generateDelegate = generateIADDG;
        generators["IADDG"] = generateDelegate;
        generators["IADDGB"] = generateDelegate;

        generateDelegate = generateILADD;
        generators["ILADD"] = generateDelegate;
        generators["ILADDB"] = generateDelegate;
        generators["LIADD"] = generateDelegate;
        generators["LIADDB"] = generateDelegate;

        generateDelegate = generateIGADD;
        generators["IGADD"] = generateDelegate;
        generators["IGADDB"] = generateDelegate;
        generators["GIADD"] = generateDelegate;
        generators["GIADDB"] = generateDelegate;

        generateDelegate = generateIGSUB;
        generators["IGSUB"] = generateDelegate;
        generators["IGSUBB"] = generateDelegate;

        generateDelegate = generateILSUB;
        generators["ILSUB"] = generateDelegate;
        generators["ILSUBB"] = generateDelegate;

        generateDelegate = generateLISUB;
        generators["LISUB"] = generateDelegate;
        generators["LISUBB"] = generateDelegate;

        generateDelegate = generateLISHR;
        generators["LISHR"] = generateDelegate;
        generators["LISHRB"] = generateDelegate;

        generateDelegate = generateLISHL;
        generators["LISHL"] = generateDelegate;
        generators["LISHLB"] = generateDelegate;

        generateDelegate = generateLIGTI;
        generators["LIGTI"] = generateDelegate;

        
    }
    
    <string,uint> pairs;
    
    EmitPairs()
    {
        uint max = 0;
        foreach (var pr in pairs)
        {
            uint count = pr.value;
            if (count > max)
            {
                max = count;
            }
        }
        uint rows = 0;
        loop
        {
            if (max <= 2)   { break; }
            if (rows >= 20) { break; }
            bool first = true;
            foreach (var pr in pairs)
            {
                uint count = pr.value;
                if (count == max)
                {
                    if (first)
                    {
                        PrintLn();
                        Print(count.ToString() + ": ");
                        first = false;
                        rows++;
                    }
                    else
                    {
                        Print(", ");
                    }
                    Print(pr.key);
                }
            }
            max--;
        }
    }
    
    bool wasRem;
    Generate()
    {   
        if (FirstPass || (currentStream.Count == 0) || !Compiling) { return; }
        
        InStreamMode = false;
        wasRem = false;
        
        if (IsExperimental && (currentStream.Count != 1))
        {
            string content = ToString(currentStream);
            PadOut("", 0);
            PadOut("// ### " + content, 0);
            Instruction previous;
            foreach (var vi in currentStream)
            {
                Instruction instruction = vi;
                if ((previous.Name).Length != 0)
                {
                    string pname = previous.Name + (previous.IsByte ? "B" : "");
                    string cname = instruction.Name + (instruction.IsByte ? "B" : "");
                    string pr = pname + "+" + cname;
                    if (pr.Contains("CALL+") || pr.Contains("+CALL")  || pr.Contains("+ELSE")  || pr.Contains("+ENDIF"))
                    {
                        // ignore
                    }
                    else if (pr.StartsWith("PUSH") && pr.Contains("+PUSH"))
                    {
                        // ignore PUSH+PUSH (of any kind)
                    }
                    else if (!pairs.Contains(pr))
                    {
                        pairs[pr] = 1;
                    }
                    else
                    {
                        pairs[pr] = pairs[pr] + 1;
                    }
                }
                previous = instruction;
            }
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
            if (generators.Contains(name))
            {
                GenerateDelegate generateDelegate = generators[name];
                generateDelegate(instruction);
            }
            else
            {
                TCCode.PadOut(name + ": Not Implemented in TCGen", 0);
            }
        }
        currentStream.Clear();
        InStreamMode = true;
    }
    generateGIANDB(Instruction instruction)
    {
        if ((instruction.Operand).GetByte(0) == 0)
        {
            TCCode.PadOut("LDA # 0x00", 0);
        }
        else
        {
            int goffset  = instruction.Offset;
            TCCode.PadOut("LDA " + GlobalOperand(goffset), 0);
            TCCode.PadOut("AND # 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2), 0);
        }
        TCCode.PadOut("PHA", 0);
    }
    generateGIAND(Instruction instruction)
    {
        int goffset  = instruction.Offset;
        if ((instruction.Operand).GetByte(0) == 0)
        {
            TCCode.PadOut("LDA # 0x00", 0);
        }
        else
        {
            TCCode.PadOut("LDA " + GlobalOperand(goffset), 0);
            TCCode.PadOut("AND # 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2), 0);
        }
        TCCode.PadOut("PHA", 0);
        if ((instruction.Operand).GetByte(1) == 0)
        {
            TCCode.PadOut("LDA # 0x00", 0);
        }
        else
        {
            TCCode.PadOut("LDA " + GlobalOperand(goffset+1), 0);
            TCCode.PadOut("AND # 0x" + ((instruction.Operand).GetByte(1)).ToHexString(2), 0);
        }
        TCCode.PadOut("PHA", 0);
    }
    generateGIANDFF(Instruction instruction)
    {
        int goffset  = instruction.Offset;
        TCCode.PadOut("LDA " + GlobalOperand(goffset), 0);
        TCCode.PadOut("PHA", 0);
        TCCode.PadOut("LDA # 0x00", 0);
        TCCode.PadOut("PHA", 0);
    }
    generateGIANDFFB(Instruction instruction)
    {
        int goffset  = instruction.Offset;
        TCCode.PadOut("LDA " + GlobalOperand(goffset), 0);
        TCCode.PadOut("PHA", 0);
    }
    generateLIANDB(Instruction instruction)
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
    generateLIAND(Instruction instruction)
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
    generateLIANDFF(Instruction instruction)
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
    generateLISHL8(Instruction instruction)
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
    generateLISHR8(Instruction instruction)
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
    generateIADD(Instruction instruction)
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
    generateIADDC(Instruction instruction)
    {
        if (instruction.IsByte)
        {
            Die(0x0B);
        }
        TCCode.PadOut("PLA", 0); // toss MSB
        TCCode.PadOut("CLC", 0);
        TCCode.PadOut("PLA", 0);
        TCCode.PadOut("ADC # 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2), 0);
        TCCode.PadOut("PHA", 0);
    }
    generateIADDL(Instruction instruction)
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
    generateILADD(Instruction instruction)
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
    generateLADD(Instruction instruction)
    {
        TCCode.BPOffset(instruction.Offset, "X", instruction.IsByte); 
        TCCode.PadOut("CLC", 0);
        if (instruction.IsByte)
        {
            TCCode.PadOut("PLA", 0); // NEXTL
            TCCode.PadOut("ADC 0x0100, X // LSB", 0); // TOPL
            TCCode.PadOut("PHA", 0);
        }
        else
        {
            TCCode.PadOut("PLY", 0); // NEXTH
            TCCode.PadOut("PLA", 0); // NEXTL
            TCCode.PadOut("ADC 0x0101, X // LSB", 0); // TOPL
            TCCode.PadOut("PHA", 0);
            TCCode.PadOut("TYA", 0); // NEXTH
            TCCode.PadOut("ADC 0x0100, X // MSB", 0); // TOPH
            TCCode.PadOut("PHA", 0);
        }
    }
    generateLADDPUSHM(Instruction instruction)
    {
        if (instruction.IsByte)
        {
            Die(0x0A);
        }
        TCCode.BPOffset(instruction.Offset, "X", instruction.IsByte); 
        TCCode.PadOut("CLC", 0);
        TCCode.PadOut("PLY", 0); // NEXTH
        TCCode.PadOut("PLA", 0); // NEXTL
        TCCode.PadOut("ADC 0x0101, X // LSB", 0); // TOPL
        TCCode.PadOut("STA ZP.TOPL", 0);
        TCCode.PadOut("TYA", 0); // NEXTH
        TCCode.PadOut("ADC 0x0100, X // MSB", 0); // TOPH
        TCCode.PadOut("STA ZP.TOPH", 0);
        TCCode.PadOut("LDA [ZP.TOP]", 0);
        TCCode.PadOut("PHA", 0);
    }
    generateIADDG(Instruction instruction)
    {
        int goffset = instruction.Offset;
        if (instruction.IsByte)
        {
            TCCode.PadOut("CLC", 0);
            TCCode.PadOut("PLA", 0);
            TCCode.PadOut("ADC # 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2), 0);
            TCCode.PadOut("STA " + GlobalOperand(goffset), 0);
        }
        else
        {
            TCCode.PadOut("PLY", 0);
            TCCode.PadOut("CLC", 0);
            TCCode.PadOut("PLA", 0);
            TCCode.PadOut("ADC # 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2), 0);
            TCCode.PadOut("STA " + GlobalOperand(goffset), 0);
            TCCode.PadOut("TYA", 0);
            TCCode.PadOut("ADC # 0x" + ((instruction.Operand).GetByte(1)).ToHexString(2), 0);
            TCCode.PadOut("STA " + GlobalOperand(goffset+1), 0);
        }
    }
    generateIGADD(Instruction instruction)
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
    generateIGSUB(Instruction instruction)
    {
        int goffset = instruction.Offset;
        if (instruction.IsByte)
        {
            TCCode.PadOut("SEC", 0);
            TCCode.PadOut("LDA # 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2), 0);
            TCCode.PadOut("SBC " + GlobalOperand(goffset), 0);
            TCCode.PadOut("PHA", 0);
        }
        else
        {
            TCCode.PadOut("SEC", 0);
            TCCode.PadOut("LDA # 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2), 0);
            TCCode.PadOut("SBC " + GlobalOperand(goffset), 0);
            TCCode.PadOut("PHA", 0);
            TCCode.PadOut("LDA # 0x" + ((instruction.Operand).GetByte(1)).ToHexString(2), 0);
            TCCode.PadOut("SBC " + GlobalOperand(goffset+1), 0);
            TCCode.PadOut("PHA", 0);
        }
    }
    generateILSUB(Instruction instruction)
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
            TCCode.PadOut("SBC 0x0101, X // LSB", 0);
            TCCode.PadOut("PHA", 0);
            TCCode.PadOut("LDA # 0x" + ((instruction.Operand).GetByte(1)).ToHexString(2), 0);
            TCCode.PadOut("SBC 0x0100, X // MSB", 0);
            TCCode.PadOut("PHA", 0);
        }
    }
    generateLISUB(Instruction instruction)
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
    
    generateLISHR(Instruction instruction)
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
    generateLISHL(Instruction instruction)
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
    generateLIGTI(Instruction instruction)
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
    
    generateINCLI(Instruction instruction)
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
    generateSTGI(Instruction instruction)
    {
        int goffset = instruction.Offset;
        byte lsb = (instruction.Operand).GetByte(0);
        byte msb = (instruction.Operand).GetByte(1);
        
        if (lsb == 0)
        {
            TCCode.PadOut("STZ " + GlobalOperand(goffset), 0);
        }
        else
        {
            TCCode.PadOut("LDA # 0x" + lsb.ToHexString(2), 0);
            TCCode.PadOut("STA " + GlobalOperand(goffset), 0);
        }
        if (!instruction.IsByte)
        {
            if (msb == 0)
            {
                TCCode.PadOut("STZ " + GlobalOperand(goffset+1), 0);
            }
            else
            {
                if (msb != lsb)
                {
                    TCCode.PadOut("LDA # 0x" + msb.ToHexString(2), 0);
                }
                TCCode.PadOut("STA " + GlobalOperand(goffset+1), 0);
            }
        }
    }
    generateSTLI(Instruction instruction)
    {
        TCCode.BPOffset(instruction.Offset, "X", instruction.IsByte);
        byte lsb = (instruction.Operand).GetByte(0);
        byte msb = (instruction.Operand).GetByte(1);
            
        if (lsb == 0)
        {
            if (instruction.IsByte)
            {
                TCCode.PadOut("STZ 0x0100, X // LSB", 0);
            }
            else
            {
                TCCode.PadOut("STZ 0x0101, X // LSB", 0);
            }
        } 
        else
        {
            TCCode.PadOut("LDA # 0x" + lsb.ToHexString(2), 0);
            if (instruction.IsByte)
            {
                TCCode.PadOut("STA 0x0100, X // LSB", 0);
            }
            else
            {
                TCCode.PadOut("STA 0x0101, X // LSB", 0);
            }
        }
        if (!instruction.IsByte)
        {
            if (msb == 0)
            {
                TCCode.PadOut("STZ 0x0100, X // MSB", 0);
            }
            else
            {
                if (msb != lsb)
                {
                    TCCode.PadOut("LDA # 0x" + msb.ToHexString(2), 0);
                }
                TCCode.PadOut("STA 0x0100, X // MSB", 0);
            }
        }
    }
    generateLILE(Instruction instruction)
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
    generateLILT(Instruction instruction)
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
    generateGILE(Instruction instruction)
    {
        string top = (instruction.IsByte ? ("# 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2)) :  ("# 0x" + (instruction.Operand).ToHexString(4)));
        int goffset = instruction.Offset;
        string next = "[" + GlobalOperand(goffset) + "]";
        TCCode.PadOut("LDX #1 // " + next + " <= " + top, 0);
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
    generateGILT(Instruction instruction)
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
    generateGILTX(Instruction instruction)
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
    generateGILEX(Instruction instruction)
    {
        string top = (instruction.IsByte ? ("# 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2)) :  ("# 0x" + (instruction.Operand).ToHexString(4)));
        int goffset = instruction.Offset;
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
        TCCode.PadOut("if (NZ) // " + next + " == " + top + " (not >)?", 0);
        TCCode.PadOut("{", 0);
        TCCode.PadOut("if (C) // " + next + " <  " + top + " (not >)?", 1);
        TCCode.PadOut("{", 1);
        TCCode.PadOut("break; // " + instruction.Data, 2);
        TCCode.PadOut("}", 1);
        TCCode.PadOut("}", 0);
    }
    
    generateLILEX(Instruction instruction)
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
    generateLILTX(Instruction instruction)
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
    generate2L(Instruction instruction)
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
    generate2G(Instruction instruction)
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
    generateLLADD(Instruction instruction)
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
    generateGGADDPUSHM(Instruction instruction)
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
    generateLGADDPUSHM(Instruction instruction)
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
    generateGLADDPUSHM(Instruction instruction)
    {
        if (instruction.IsByte)
        {
            Die(0x0A);
        }
        TCCode.BPOffset(instruction.Offset2, "X", false); 
        
        TCCode.PadOut("CLC", 0);
        int goffset = instruction.Offset;
        TCCode.PadOut("LDA " + GlobalOperand(goffset), 0);
        TCCode.PadOut("ADC 0x0101, X // LSB", 0);
        TCCode.PadOut("STA ZP.TOPL", 0);
        
        TCCode.PadOut("LDA " + GlobalOperand(goffset+1), 0);
        TCCode.PadOut("ADC 0x0100, X // MSB", 0);
        TCCode.PadOut("STA ZP.TOPH", 0);
        
        TCCode.PadOut("LDA [ZP.TOP]", 0);
        TCCode.PadOut("PHA", 0);
    }
    generateILADDPUSHM(Instruction instruction)
    {
        if (instruction.IsByte)
        {
            Die(0x0A);
        }
        TCCode.BPOffset(instruction.Offset, "X", false); 
        if (instruction.Operand == 0)
        {
            TCCode.PadOut("LDA 0x0101, X // LSB", 0);
            TCCode.PadOut("STA ZP.TOPL", 0);
            TCCode.PadOut("LDA 0x0100, X // MSB", 0);
            TCCode.PadOut("STA ZP.TOPH", 0);
        }
        else
        {
            TCCode.PadOut("CLC", 0);
            TCCode.PadOut("LDA # 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2), 0);
            TCCode.PadOut("ADC 0x0101, X // LSB", 0);
            TCCode.PadOut("STA ZP.TOPL", 0);
            
            TCCode.PadOut("LDA # 0x" + ((instruction.Operand).GetByte(1)).ToHexString(2), 0);
            TCCode.PadOut("ADC 0x0100, X // MSB", 0);
            TCCode.PadOut("STA ZP.TOPH", 0);
        }
        
        TCCode.PadOut("LDA [ZP.TOP]", 0);
        TCCode.PadOut("PHA", 0);
    }
    generateIGADDPUSHM(Instruction instruction)
    {
        if (instruction.IsByte)
        {
            Die(0x0A);
        }
        int goffset = instruction.Offset;
        if (instruction.Operand == 0)
        {
            TCCode.PadOut("LDA " + GlobalOperand(goffset), 0);
            TCCode.PadOut("STA ZP.TOPL", 0);
            TCCode.PadOut("LDA " + GlobalOperand(goffset+1), 0);
            TCCode.PadOut("STA ZP.TOPH", 0);
        }
        else
        {
            TCCode.PadOut("CLC", 0);
            TCCode.PadOut("LDA # 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2), 0);
            TCCode.PadOut("ADC " + GlobalOperand(goffset), 0);
            TCCode.PadOut("STA ZP.TOPL", 0);
            
            TCCode.PadOut("LDA # 0x" + ((instruction.Operand).GetByte(1)).ToHexString(2), 0);
            TCCode.PadOut("ADC " + GlobalOperand(goffset+1), 0);
            TCCode.PadOut("STA ZP.TOPH", 0);
        }
        TCCode.PadOut("LDA [ZP.TOP]", 0);
        TCCode.PadOut("PHA", 0);
    }
    
    generateLGADDIPOPM(Instruction instruction)
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
    generateLGADDI(Instruction instruction)
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
        TCCode.PadOut("ADC " + GlobalOperand(goffset+1), 0);
        TCCode.PadOut("PHA", 0);
        TCCode.PadOut("LDA # 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2), 0);
        TCCode.PadOut("PHA", 0);
    }
    generateGGADDIPOPM(Instruction instruction)
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
    generateGGADDI(Instruction instruction)
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
    generateLGADD(Instruction instruction)
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
    generateGGADD(Instruction instruction)
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

    generateGGADDG(Instruction instruction)
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
    generateLLADDL(Instruction instruction)
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
    generateLTX(Instruction instruction)
    {
        // TODO : optimize
        TCOps.CompareLT(instruction.IsByte);
        TCCode.PadOut("PLA", 0);
        TCCode.PadOut("if (Z) // " + instruction.Data, 0);
        TCCode.PadOut("{", 0);
        TCCode.PadOut("break;", 1);
        TCCode.PadOut("}", 0);
    }
    generateLEX(Instruction instruction)
    {
        // TODO : optimize
        TCOps.CompareLE(instruction.IsByte);
        TCCode.PadOut("PLA", 0);
        TCCode.PadOut("if (Z) // " + instruction.Data, 0);
        TCCode.PadOut("{", 0);
        TCCode.PadOut("break;", 1);
        TCCode.PadOut("}", 0);
    }
    generateINCGI(Instruction instruction)
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
                    TCCode.PadOut("ADC # 0x" + ((instruction.Operand).GetByte(1)).ToHexString(2), 0);
                    TCCode.PadOut("STA " + GlobalOperand(goffset+1), 0);
                }
            }
        }               
    }
    generateCALL(Instruction instruction)
    {
        TCCode.Call(instruction.Data);
        if (instruction.Operand != 0)
        {
            TCCode.PopBytes((instruction.Operand).GetByte(0), "");
        }
        if (instruction.Offset != 0)
        {
            TCOps.PushTop(instruction.IsByte);
        }
    }
    generateLE(Instruction instruction) { TCOps.CompareLE(instruction.IsByte); }
    generateLT(Instruction instruction) { TCOps.CompareLT(instruction.IsByte); }
    
    generateNOT(Instruction instruction) { TCOps.BitNot(instruction.IsByte); }
    generateXOR(Instruction instruction) { TCOps.Xor(instruction.IsByte); }
    
    generatePUSHG(Instruction instruction)
    {
        TCCode.PushVariable(instruction.Data, instruction.Offset, instruction.IsByte, true);
    }
    
    generatePUSHC(Instruction instruction)
    {
        TCCode.PushConst(instruction.Operand);
    }
    generatePUSHL(Instruction instruction)
    {
        TCCode.PushVariable(instruction.Data, instruction.Offset, instruction.IsByte, false);
    }
    generatePOPG(Instruction instruction)
    {
        TCCode.PopVariable(instruction.Data, instruction.Offset, instruction.IsByte, true);
    }
    generatePOPL(Instruction instruction)
    {
        TCCode.PopVariable(instruction.Data, instruction.Offset, instruction.IsByte, false);
    }
    generateZEROG(Instruction instruction)
    {
        int offset = Int.FromBytes((instruction.Operand).GetByte(0), (instruction.Operand).GetByte(1));
        TCCode.PadOut("STZ " + GlobalOperand(offset), 0);
        if (!instruction.IsByte)
        {
            TCCode.PadOut("STZ " + GlobalOperand(offset+1), 0);
        }
    }
    generateSP(Instruction instruction)
    {
        TCCode.PadOut("TSX", 0);
        TCCode.PadOut("PHX", 0);
    }
    generatePUSHIB(Instruction instruction)
    { 
        TCCode.PushByte((instruction.Operand).GetByte(0));
    }
    generatePUSHI(Instruction instruction)
    { 
        TCCode.PushWord(instruction.Operand);
    }
    generatePUSHZB(Instruction instruction)
    { 
        TCCode.PushByte(0);
    }
    generatePUSHZ(Instruction instruction)
    { 
        TCCode.PushWord(0);
    }
    /*
    generatePUSHGI(Instruction instruction)
    {
        TCCode.PushVariable(instruction.Data, instruction.Offset, instruction.IsByte, true);
        if (instruction.IsByte)
        {
            TCCode.PushByte((instruction.Operand).GetByte(0));
        }
        else
        {
            TCCode.PushWord(instruction.Operand);
        }
    }
    */
    generateREM(Instruction instruction) { wasRem = true; }
    
    generateADD(Instruction instruction)
    {
        TCOps.Add(instruction.IsByte);
    }
    generateSUB(Instruction instruction)
    {
        TCOps.Sub(instruction.IsByte);
    }
    generateMUL(Instruction instruction)
    {
        TCOps.Mul(instruction.IsByte);
    }
    generateDIV(Instruction instruction)
    {
        TCOps.Div(instruction.IsByte);
    }
    generateMOD(Instruction instruction)  { TCOps.Mod(instruction.IsByte); }
    generateMULI(Instruction instruction) { TCOps.MulI(); }
    generateDIVI(Instruction instruction) { TCOps.DivI(); }
    generateMODI(Instruction instruction) { TCOps.ModI(); }
    
    generateLOOPEXIT(Instruction instruction)
    {
        TCCode.PadOut("PLA", 0);
        TCCode.PadOut("if (Z) // " + instruction.Data, 0);
        TCCode.PadOut("{", 0);
        TCCode.PadOut("break;", 1);
        TCCode.PadOut("}", 0);
    }
    generateENDIF(Instruction instruction) { TCCode.PadOut("}", 0); }
    generateELSE(Instruction instruction)
    {
        TCCode.PadOut("}", 0);
        TCCode.Else();
        TCCode.PadOut("{", 0);
    }
    generateIF(Instruction instruction)
    {
        TCCode.PadOut("", 0);
        TCCode.PadOut("PLA // bool", 0); // bool so one byte
        TCCode.PadOut("if (NZ)", 0);
        if (instruction.Operand != 0)
        {
            TCCode.PadOut("{ // if", 0);
        }
    }
    generateDECSP(Instruction instruction)    { TCCode.PopBytes((instruction.Operand).GetByte(0), ""); }
    generatePADUNDER(Instruction instruction) { TCCode.CastPad(true); }
    generateBOOLNOTB(Instruction instruction) { TCOps.BoolNot(); }
    
    generateSHR(Instruction instruction)      { TCOps.Shr(instruction.IsByte); }
    generateSHL(Instruction instruction)      { TCOps.Shl(instruction.IsByte); }
    generateGEI(Instruction instruction)      { TCOps.CompareGEI(); }
    generateGTI(Instruction instruction)      { TCOps.CompareGTI(); }
    generateLEI(Instruction instruction)      { TCOps.CompareLEI(); }
    generateLTI(Instruction instruction)      { TCOps.CompareLTI(); }
    generateGE(Instruction instruction)       { TCOps.CompareGE(instruction.IsByte); }
    generateGT(Instruction instruction)       { TCOps.CompareGT(instruction.IsByte); }
    generateNE(Instruction instruction)       { TCOps.CompareNE(instruction.IsByte); }
    generateEQ(Instruction instruction)       { TCOps.CompareEQ(instruction.IsByte); }
    
    
    
    
    generateGGMULI(Instruction instruction)
    {
        if (instruction.IsByte)
        {
            Die(0x0B);
        }
        int next  = instruction.Offset;
        int top   = instruction.Offset2;
        TCCode.PadOut("LDA " + GlobalOperand(next), 0);
        TCCode.PadOut("STA ZP.NEXTL", 0);
        TCCode.PadOut("LDA " + GlobalOperand(next+1), 0);
        TCCode.PadOut("STA ZP.NEXTH", 0);
        TCCode.PadOut("LDA " + GlobalOperand(top), 0);
        TCCode.PadOut("STA ZP.TOPL", 0);
        TCCode.PadOut("LDA " + GlobalOperand(top+1), 0);
        TCCode.PadOut("STA ZP.TOPH", 0);
        TCCode.PadOut("TCOps.MulI();", 0);
        TCCode.PadOut("LDA ZP.TOPL", 0);
        TCCode.PadOut("PHA", 0);
        TCCode.PadOut("LDA ZP.TOPH", 0);
        TCCode.PadOut("PHA", 0);
    }
    generateGGDIVI(Instruction instruction)
    {
        if (instruction.IsByte)
        {
            Die(0x0B);
        }
        int next  = instruction.Offset;
        int top   = instruction.Offset2;
        TCCode.PadOut("LDA " + GlobalOperand(next), 0);
        TCCode.PadOut("STA ZP.NEXTL", 0);
        TCCode.PadOut("LDA " + GlobalOperand(next+1), 0);
        TCCode.PadOut("STA ZP.NEXTH", 0);
        TCCode.PadOut("LDA " + GlobalOperand(top), 0);
        TCCode.PadOut("STA ZP.TOPL", 0);
        TCCode.PadOut("LDA " + GlobalOperand(top+1), 0);
        TCCode.PadOut("STA ZP.TOPH", 0);
        TCCode.PadOut("TCOps.DivI();", 0);
        TCCode.PadOut("LDA ZP.TOPL", 0);
        TCCode.PadOut("PHA", 0);
        TCCode.PadOut("LDA ZP.TOPH", 0);
        TCCode.PadOut("PHA", 0);
    }
    generateDUP(Instruction instruction)
    {
        TCCode.Dup(instruction.IsByte);
    }
    generatePUSHM(Instruction instruction)
    {
        PadOut("// read memory" +  Bitness(instruction.IsByte), 0);
        
        PadOut("PLA", 0);        
        PadOut("STA ZP.TOPH", 0);        
        PadOut("PLA", 0);        
        PadOut("STA ZP.TOPL", 0);
        PadOut("LDA [ZP.TOP]", 0);
        PadOut("PHA", 0);
        if (!instruction.IsByte)
        {
            PadOut("LDY # 1", 0);
            PadOut("LDA [ZP.TOP], Y", 0);
            PadOut("PHA", 0);
        }
    }
    
    generatePOPM(Instruction instruction)
    {
        PadOut("", 0);
        PadOut("// write memory" +  Bitness(instruction.IsByte), 0);
    
        if (!instruction.IsByte)
        {
            TCCode.PadOut("PLA", 0); 
            TCCode.PadOut("STA ZP.ACCH", 0);     
        }   
        TCCode.PadOut("PLA", 0); 
        TCCode.PadOut("STA ZP.ACCL", 0);     
        TCCode.PadOut("PLA", 0);        
        TCCode.PadOut("STA ZP.TOPH", 0);        
        TCCode.PadOut("PLA", 0);        
        TCCode.PadOut("STA ZP.TOPL", 0);
        TCCode.PadOut("LDA ZP.ACCL", 0);        
        TCCode.PadOut("STA [ZP.TOP]", 0);
        if (!instruction.IsByte)
        {
            TCCode.PadOut("LDA ZP.ACCH", 0);     
            TCCode.PadOut("LDY # 1", 0);
            TCCode.PadOut("STA [ZP.TOP], Y", 0);
        }
    }
    generateIPOPM(Instruction instruction)
    {
        PadOut("", 0);
        PadOut("// write memory" +  Bitness(instruction.IsByte), 0);
    
        TCCode.PadOut("PLA", 0);        
        TCCode.PadOut("STA ZP.TOPH", 0);        
        TCCode.PadOut("PLA", 0);        
        TCCode.PadOut("STA ZP.TOPL", 0);
        TCCode.PadOut("LDA # 0x" + (instruction.Operand).ToHexString(2), 0);        
        TCCode.PadOut("STA [ZP.TOP]", 0);
        if (!instruction.IsByte)
        {
            TCCode.PadOut("LDA # 0x" + (instruction.Operand >> 8).ToHexString(2), 0);     
            TCCode.PadOut("LDY # 1", 0);
            TCCode.PadOut("STA [ZP.TOP], Y", 0);
        }
    }
    generateINEB(Instruction instruction)       
    { 
        byte lsb = (instruction.Operand).GetByte(0);
        TCCode.PadOut("// != (8 bit)", 0);
        TCCode.PadOut("// arguments in NEXT and TOP", 0);
        TCCode.PadOut("LDX # 1 // NEXT != TOP", 0);
        TCCode.PadOut("PLA", 0); // NEXTL
        if (lsb != 0)
        {
            TCCode.PadOut("CMP # 0x" + lsb.ToHexString(2), 0); // TOPL
        }
        TCCode.PadOut("if (Z)", 0);
        TCCode.PadOut("{", 0);
        TCCode.PadOut("LDX # 0 // NEXT == TOP", 1);
        TCCode.PadOut("}", 0);
        TCCode.PadOut("// result in X", 0);
        TCCode.PadOut("PHX", 0); 
    }
    generateINE(Instruction instruction)       
    { 
        byte lsb = (instruction.Operand).GetByte(0);
        byte msb = (instruction.Operand).GetByte(1);
        TCCode.PadOut("// != (16 bit)", 0);
        TCCode.PadOut("PLY", 0); // NEXTH
        TCCode.PadOut("// arguments in NEXT and TOP", 0);
        TCCode.PadOut("LDX # 1 // NEXT != TOP", 0);
        TCCode.PadOut("PLA", 0); // NEXTL
        if (lsb != 0)
        {
            TCCode.PadOut("CMP # 0x" + lsb.ToHexString(2), 0); // TOPL
        }
        TCCode.PadOut("if (Z)", 0);
        TCCode.PadOut("{", 0);
        TCCode.PadOut("TYA", 1); // NEXTH
        if (msb != 0)
        {
            TCCode.PadOut("CMP # 0x" + msb.ToHexString(2), 1); // TOPH
        }
        TCCode.PadOut("if (Z)", 1);
        TCCode.PadOut("{", 1);
        TCCode.PadOut("LDX # 0 // NEXT == TOP", 2);
        TCCode.PadOut("}", 1);
        TCCode.PadOut("}", 0);
        TCCode.PadOut("// result in X", 0);
        TCCode.PadOut("PHX", 0);
    }
    generateINEXB(Instruction instruction)       
    { 
        byte lsb = (instruction.Operand).GetByte(0);
        TCCode.PadOut("// != (8 bit)", 0);
        TCCode.PadOut("// arguments in NEXT and TOP", 0);
        TCCode.PadOut("PLA", 0); // NEXTL
        if (lsb != 0)
        {
            TCCode.PadOut("CMP # 0x" + lsb.ToHexString(2), 0); // TOPL
        }
        TCCode.PadOut("if (Z)", 0);
        TCCode.PadOut("{", 0);
        TCCode.PadOut("break; // NEXT == TOP", 1);
        TCCode.PadOut("}", 0);
    }
    generateINEX(Instruction instruction)       
    { 
        byte lsb = (instruction.Operand).GetByte(0);
        byte msb = (instruction.Operand).GetByte(1);
        TCCode.PadOut("// != (16 bit)", 0);
        TCCode.PadOut("PLY", 0); // NEXTH
        TCCode.PadOut("// arguments in NEXT and TOP", 0);
        TCCode.PadOut("PLA", 0); // NEXTL
        if (lsb != 0)
        {
            TCCode.PadOut("CMP # 0x" + lsb.ToHexString(2), 0); // TOPL
        }
        TCCode.PadOut("if (Z)", 0);
        TCCode.PadOut("{", 0);
        TCCode.PadOut("TYA", 1); // NEXTH
        if (msb != 0)
        {
            TCCode.PadOut("CMP # 0x" + msb.ToHexString(2), 1); // TOPH
        }
        TCCode.PadOut("if (Z)", 1);
        TCCode.PadOut("{", 1);
        TCCode.PadOut("break; // NEXT == TOP", 2);
        TCCode.PadOut("}", 1);
        TCCode.PadOut("}", 0);
    }
    generateIEQB(Instruction instruction)
    { 
        byte lsb = (instruction.Operand).GetByte(0);
        TCCode.PadOut("// == (8 bit)", 0);
        TCCode.PadOut("// arguments in NEXT and TOP", 0);
        TCCode.PadOut("LDX # 0 // NEXT != TOP", 0);
        TCCode.PadOut("PLA", 0); // NEXTL
        if (lsb != 0)
        {
            TCCode.PadOut("CMP # 0x" + lsb.ToHexString(2), 0); // TOPL
        }
        TCCode.PadOut("if (Z)", 0);
        TCCode.PadOut("{", 0);
        TCCode.PadOut("LDX # 1 // NEXT == TOP", 1);
        TCCode.PadOut("}", 0);
        TCCode.PadOut("// result in X", 0);
        TCCode.PadOut("PHX", 0);
    }
    generateIEQ(Instruction instruction)
    { 
        byte lsb = (instruction.Operand).GetByte(0);
        byte msb = (instruction.Operand).GetByte(1);
        TCCode.PadOut("// == (16 bit)", 0);
        TCCode.PadOut("PLY", 0); // NEXTH
        TCCode.PadOut("// arguments in NEXT and TOP", 0);
        TCCode.PadOut("LDX # 0 // NEXT != TOP", 0);
        TCCode.PadOut("PLA", 0); // NEXTL
        if (lsb != 0)
        {
            TCCode.PadOut("CMP # 0x" + lsb.ToHexString(2), 0); // TOPL
        }
        TCCode.PadOut("if (Z)", 0);
        TCCode.PadOut("{", 0);
        TCCode.PadOut("TYA", 1); // NEXTH
        if (msb != 0)
        {
            TCCode.PadOut("CMP # 0x" + msb.ToHexString(2), 1); // TOPH
        }
        TCCode.PadOut("if (Z)", 1);
        TCCode.PadOut("{", 1);
        TCCode.PadOut("LDX # 1 // NEXT == TOP", 2);
        TCCode.PadOut("}", 1);
        TCCode.PadOut("}", 0);
        TCCode.PadOut("// result in X", 0);
        TCCode.PadOut("PHX", 0); 
    }
    generateOR(Instruction instruction)  
    {
        if (instruction.IsByte)
        {
            TCCode.PadOut("PLA", 0);
            TCCode.PadOut("STA ZP.NEXTL", 0);
            TCCode.PadOut("PLA", 0);
            TCCode.PadOut("ORA ZP.NEXTL", 0);
            TCCode.PadOut("PHA", 0);
        }
        else
        {
            TCOps.Or(instruction.IsByte);
        }
    } 
    generateAND(Instruction instruction)
    {
        if (instruction.IsByte)
        {
            TCCode.PadOut("PLA", 0);
            TCCode.PadOut("STA ZP.NEXTL", 0);
            TCCode.PadOut("PLA", 0);
            TCCode.PadOut("AND ZP.NEXTL", 0);
            TCCode.PadOut("PHA", 0);
        }
        else
        {
            TCOps.And(instruction.IsByte); 
        }
    }
    generateIEQZIFB(Instruction instruction)
    { 
        TCCode.PadOut("// == 0 (8 bit) ?", 0);
        TCCode.PadOut("PLA", 0);
        TCCode.PadOut("if (Z)", 0);
        if (instruction.Operand != 0)
        {
            TCCode.PadOut("{ // if", 0);
        }
    }
    generateINEZIFB(Instruction instruction)       
    { 
        TCCode.PadOut("// != 0 (8 bit) ?", 0);
        TCCode.PadOut("PLA", 0);
        TCCode.PadOut("if (NZ)", 0);
        if (instruction.Operand != 0)
        {
            TCCode.PadOut("{ // if", 0);
        }
    }
    generateIEQZIF(Instruction instruction)
    { 
        TCCode.PadOut("// == 0 (16 bit) ?", 0);
        TCCode.PadOut("PLY", 0);
        TCCode.PadOut("PLA", 0);
        TCCode.PadOut("if (Z)", 0);
        TCCode.PadOut("{", 0);
        TCCode.PadOut("TYA", 1);
        TCCode.PadOut("}", 0);
        TCCode.PadOut("if (Z)", 0);
        if (instruction.Operand != 0)
        {
            TCCode.PadOut("{ // if", 0);
        }
    }
    generateINEZIF(Instruction instruction)       
    { 
        TCCode.PadOut("// != 0 (16 bit) ?", 0);
        TCCode.PadOut("PLY", 0);
        TCCode.PadOut("PLA", 0);
        TCCode.PadOut("if (Z)", 0);
        TCCode.PadOut("{", 0);
        TCCode.PadOut("TYA", 1);
        TCCode.PadOut("}", 0);
        TCCode.PadOut("if (NZ)", 0);
        if (instruction.Operand != 0)
        {
            TCCode.PadOut("{ // if", 0);
        }
    }
    
}
