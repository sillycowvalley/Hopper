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
    //    ADDLL   : PUSHL offset0, PUSHL offset?, ADD
    //    ADDLG   : PUSHL offset0, PUSHG offset1, ADD
    //    ADDLGM  : ADDLG PUSHMB
    //    ADDLGI  : ADDLG PUSHIB
    //    ADDLGIM : ADDLGI POPMB
    //    INCLI   : PUSHL offset0, PUSHI, ADD, POPL offset0
    //    ADDLLL  : PUSHL offset0, PUSHL offset?, ADD, POPL offset0
    //    STLI    : PUSHI, POPL offset0
    //    LISHR   : PUSHL offset, PUSHI, SHR
    //    LISHR8  : PUSHL offset, PUSHI 0x0008, SHR
    //    LISHL   : PUSHL offset, PUSHI, SHL
    //    LIAND   : PUSHL offset, PUSHI, AND
    //    LIANDFF : PUSHL offset, PUSHI 0x00FF, AND
    //    ILADD   : PUSHI, PUSHL offset, ADD
    //    ILSUB   : PUSHI, PUSHL offset, SUB
    //    LIADD   : PUSHL offset, PUSHI, ADD
    //    LISUB   : PUSHL offset, PUSHI, SUB
    //    LTX     : LT, LOOPEXIT
    //    LEX     : LE, LOOPEXIT
    //    LILE    : PUSHL offset, PUSHI, LE
    //    LILT    : PUSHL offset, PUSHI, LT
    //    LIGTI   : PUSHL offset, PUSHI, GTI
    //    LILEX   : PUSHL offset, PUSHI, LE, LOOPEXIT
    //    LILTX   : PUSHL offset, PUSHI, LT, LOOPEXIT
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
            if (instruction0.Name == "LOOPEXIT")
            {
                if (instruction1.Name == "LE") 
                {
                    Print(" LEX");    
                }
                if (instruction1.Name == "LT") 
                {
                    Print(" LTX");    
                }
                if (instruction1.Name == "LILE") 
                {
                    Print(" LILEX");
                    instruction1.Name    = "LILEX";
                    instruction1.Data = instruction0.Data;
                    currentStream[currentStream.Count-2] = instruction1;
                    DeleteInstruction(currentStream.Count-1);
                    modified = true;
                    break;
                }    
                if (instruction1.Name == "LILT")
                {
                    Print(" LILTX");
                    instruction1.Name    = "LILTX";
                    instruction1.Data = instruction0.Data;
                    currentStream[currentStream.Count-2] = instruction1;
                    DeleteInstruction(currentStream.Count-1);
                    modified = true;
                    break;
                }
            }
            if (instruction0.Name == "PUSHI")
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
                        if (instruction1.Name == "ADDLG")
                        {
                            instruction1.Name    = "ADDLGI";
                            instruction1.Operand = instruction0.Operand;
                            currentStream[currentStream.Count-2] = instruction1;
                            DeleteInstruction(currentStream.Count-1);
                            modified = true;
                            break;
                        }
                    }
                }
            }
            if (instruction0.Name == "POPL")
            {
                if (instruction0.IsByte == instruction1.IsByte)
                {
                    if (instruction1.Name == "PUSHI") 
                    {
                        instruction0.Operand = instruction1.Operand;
                        instruction0.Name = "STLI";
                        currentStream[currentStream.Count-1] = instruction0;
                        DeleteInstruction(currentStream.Count-2);
                        modified = true;
                        break;
                    }
                    if ((instruction1.Name == "ADDLL") && (instruction1.Offset == instruction0.Offset))
                    {   
                        instruction1.Name    = "ADDLLL";
                        currentStream[currentStream.Count-2] = instruction1;
                        DeleteInstruction(currentStream.Count-1);
                        modified = true;
                        break;
                    }
                    if ((instruction1.Name == "ILADD") && (instruction1.Offset == instruction0.Offset))
                    {   
                        instruction1.Name    = "INCLI";
                        currentStream[currentStream.Count-2] = instruction1;
                        DeleteInstruction(currentStream.Count-1);
                        modified = true;
                        break;
                    }
                    if ((instruction1.Name == "LIADD") && (instruction1.Offset == instruction0.Offset))
                    {   
                        Print(" INCLI(2)");
                        instruction1.Name    = "INCLI";
                        currentStream[currentStream.Count-2] = instruction1;
                        DeleteInstruction(currentStream.Count-1);
                        modified = true;
                        break;
                    }
                    
                }
            }
            if (instruction0.Name == "PUSHM")
            {
                if (instruction0.IsByte && !instruction1.IsByte)
                {
                    if (instruction1.Name == "ADDLG")
                    {
                        instruction1.Name    = "ADDLGM";
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
                    if (instruction1.Name == "ADDLGI")
                    {
                        instruction1.Name    = "ADDLGIM";
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
            if ((instruction2.Name == "PUSHL") && (instruction1.Name == "PUSHL"))
            {
                if (instruction0.Name == "ADD")
                {
                    if ((instruction2.IsByte == instruction1.IsByte) && (instruction1.IsByte == instruction0.IsByte))
                    {
                        instruction2.Offset2 = instruction1.Offset;
                        instruction2.Name    = "ADDLL";
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
                    instruction2.Offset2 = instruction1.Offset;
                    instruction2.Name    = "ADDLG";
                    currentStream[currentStream.Count-3] = instruction2;
                    DeleteInstruction(currentStream.Count-1);
                    DeleteInstruction(currentStream.Count-1);
                    modified = true;
                    break;
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
                            instruction2.Name = "LIANDFF";
                            currentStream[currentStream.Count-3] = instruction2;
                            DeleteInstruction(currentStream.Count-2);
                            DeleteInstruction(currentStream.Count-1);
                        }
                        else
                        {
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
                            instruction2.Name = "LISHR8";
                            currentStream[currentStream.Count-3] = instruction2;
                            DeleteInstruction(currentStream.Count-2);
                            DeleteInstruction(currentStream.Count-1);
                        }
                        else
                        {
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
                            instruction2.Name = "LISHL8";
                            currentStream[currentStream.Count-3] = instruction2;
                            DeleteInstruction(currentStream.Count-2);
                            DeleteInstruction(currentStream.Count-1);
                        }
                        else
                        {
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
                        Print(" INCLI(OLD1)");
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
                    if (/*false &&*/ (instruction3.Name == "PUSHL") && (instruction2.Name == "PUSHL") && (instruction1.Name == "ADD") && (instruction0.Name == "POPL")) // UNUSED OPT 6                   
                    {   
                        instruction3.Offset2 = instruction2.Offset;
                        instruction3.IsByte  = true;
                        instruction3.Name    = "ADDLLL";
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
                        Print(" INCLI(OLD2)");
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
                    if ((instruction3.Name == "PUSHL") && (instruction2.Name == "PUSHL") && (instruction1.Name == "ADD") && (instruction0.Name == "POPL"))
                    {
                        instruction3.Offset2 = instruction2.Offset;
                        instruction3.IsByte  = false;
                        instruction3.Name    = "ADDLLL";
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
        currentStream.Append(instruction);
        if (IsOptimized)
        {
            if (enablePeephole)
            {
                // peephole optimizer
                loop
                {
                    bool modified;
                    if (currentStream.Count >= 2)
                    {
                        modified = OptimizeTwo();
                    }
                    if (currentStream.Count >= 3)
                    {
                        modified = OptimizeThree();
                    }
                    if (currentStream.Count >= 4)
                    {
                        modified = OptimizeFour();
                    }
                    if (!modified) { break;}
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
                content = "PUSHG"+width+" [0x0100 + " + OffsetToHex(instruction.Offset) + "]";
            }
            case "POPG":
            {
                content = "POPG"+width+" [0x0100 + " + OffsetToHex(instruction.Offset) + "]";
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
            
            
            case "ADDLG":
            case "ADDLGM":
            {
                content = instruction.Name + width + " [BP+" + OffsetToHex(instruction.Offset) + "] [0x" + (instruction.Offset2).ToHexString(2) + "]";
            }
            case "ADDLGI":
            case "ADDLGIM":
            {
                content = instruction.Name + width + " [BP+" + OffsetToHex(instruction.Offset) + "] [0x" + (instruction.Offset2).ToHexString(2) + "] # 0x" + (instruction.Operand).ToHexString(2);
            }
            case "ADDLL":
            case "ADDLLL":
            {
                content = instruction.Name + width + " [BP+" + OffsetToHex(instruction.Offset) + "] [BP+0x" + (instruction.Offset2).ToHexString(2) + "]";
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
            {
                content = instruction.Name + width + " [BP+" + OffsetToHex(instruction.Offset) + "] # 0x" + (instruction.Operand).ToHexString(instruction.IsByte ? 2 : 4);
            }
            case "LTX":
            case "LEX":
            {
                content = instruction.Name + width + " " +instruction.Data;
            }
            case "LILTX":
            case "LILEX":
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
                Print(" Not Implemented: " + instruction.Name);
                Die(0x0A);
            }
        }
        return content;
    }
    Generate()
    {   
        if (currentStream.Count == 0) { return; }
        
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
                        TCCode.OffsetTo(instruction.Offset, "X"); 
                        if (instruction.Operand == 1)
                        {
                            TCCode.PadOut("INC 0x0100, X", 0);
                            if (!instruction.IsByte)
                            {
                                TCCode.PadOut("if (Z)", 0);
                                TCCode.PadOut("{", 0);
                                TCCode.PadOut("DEX", 1);
                                TCCode.PadOut("INC 0x0100, X", 1);
                                TCCode.PadOut("}", 0);
                            }
                        }
                        else
                        {
                            TCCode.PadOut("CLC", 0);
                            TCCode.PadOut("LDA 0x0100, X", 0);
                            TCCode.PadOut("ADC # 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2), 0);
                            TCCode.PadOut("STA 0x0100, X", 0);
                            if (!instruction.IsByte)
                            {
                                TCCode.PadOut("DEX", 0);
                                TCCode.PadOut("LDA 0x0100, X", 0);
                                TCCode.PadOut("ADC # 0x" + ((instruction.Operand).GetByte(1)).ToHexString(2), 0);
                                TCCode.PadOut("STA 0x0100, X", 0);
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
                case "LTX":
                case "LTXB":
                {
                    TCOps.CompareLT(instruction.IsByte);
                    TCCode.PadOut("PLA", 0);
                    TCCode.PadOut("if (Z) // " + instruction.Data, 0);
                    TCCode.PadOut("{", 0);
                    TCCode.PadOut("break;", 1);
                    TCCode.PadOut("}", 0);
                }
                case "LEX":
                case "LEXB":
                {
                    TCOps.CompareLE(instruction.IsByte);
                    TCCode.PadOut("PLA", 0);
                    TCCode.PadOut("if (Z) // " + instruction.Data, 0);
                    TCCode.PadOut("{", 0);
                    TCCode.PadOut("break;", 1);
                    TCCode.PadOut("}", 0);
                }
                
                case "LILE":
                case "LILEB":
                {
                    TCCode.OffsetTo(instruction.Offset, "Y");
                    // arguments in NEXT and TOP
                    string top = (instruction.IsByte ? ("# 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2)) :  ("# 0x" + (instruction.Operand).ToHexString(4)));
                    string next = "[BP+" + OffsetToHex(instruction.Offset) + "]";
                    TCCode.PadOut("LDX #1 // " + next + " <= " + top, 0);
                    if (instruction.IsByte)
                    {
                        TCCode.PadOut("LDA 0x0100, Y", 0); // NEXTL
                        TCCode.PadOut("CMP # 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2), 0); // TOPL
                    }
                    else
                    {
                        TCCode.PadOut("DEY", 0);
                        TCCode.PadOut("LDA 0x0100, Y", 0); // NEXTH
                        TCCode.PadOut("CMP # 0x" + ((instruction.Operand).GetByte(1)).ToHexString(2), 0); // TOPH
                        TCCode.PadOut("if (Z)", 0);
                        TCCode.PadOut("{", 0);
                        TCCode.PadOut("INY", 1);
                        TCCode.PadOut("LDA 0x0100, Y", 1); // NEXTL
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
                case "LILEX":
                case "LILEXB":
                {
                    TCCode.OffsetTo(instruction.Offset, "Y");
                    // arguments in NEXT and TOP
                    string top = (instruction.IsByte ? ("# 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2)) :  ("# 0x" + (instruction.Operand).ToHexString(4)));
                    string next = "[BP+" + OffsetToHex(instruction.Offset) + "]";
                    if (instruction.IsByte)
                    {
                        TCCode.PadOut("LDA 0x0100, Y", 0); // NEXTL
                        TCCode.PadOut("CMP # 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2), 0); // TOPL
                    }
                    else
                    {
                        TCCode.PadOut("DEY", 0);
                        TCCode.PadOut("LDA 0x0100, Y", 0); // NEXTH
                        TCCode.PadOut("CMP # 0x" + ((instruction.Operand).GetByte(1)).ToHexString(2), 0); // TOPH
                        TCCode.PadOut("if (Z)", 0);
                        TCCode.PadOut("{", 0);
                        TCCode.PadOut("INY", 1);
                        TCCode.PadOut("LDA 0x0100, Y", 1); // NEXTL
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
                case "LILT":
                case "LILTB":
                {
                    TCCode.OffsetTo(instruction.Offset, "Y");
                    // arguments in NEXT and TOP
                    string top = (instruction.IsByte ? ("# 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2)) :  ("# 0x" + (instruction.Operand).ToHexString(4)));
                    string next = "[BP+" + OffsetToHex(instruction.Offset) + "]";
                    TCCode.PadOut("LDX #1 // " + next + " < " + top, 0);
                    if (instruction.IsByte)
                    {
                        TCCode.PadOut("LDA 0x0100, Y", 0); // NEXTL
                        TCCode.PadOut("CMP # 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2), 0); // TOPL
                    }
                    else
                    {
                        TCCode.PadOut("DEY", 0);
                        TCCode.PadOut("LDA 0x0100, Y", 0); // NEXTH
                        TCCode.PadOut("CMP # 0x" + ((instruction.Operand).GetByte(1)).ToHexString(2), 0); // TOPH
                        TCCode.PadOut("if (Z)", 0);
                        TCCode.PadOut("{", 0);
                        TCCode.PadOut("INY", 1);
                        TCCode.PadOut("LDA 0x0100, Y", 1); // NEXTL
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
                case "LILTX":
                case "LILTXB":
                {
                    TCCode.OffsetTo(instruction.Offset, "Y");
                    // arguments in NEXT and TOP
                    string top = (instruction.IsByte ? ("# 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2)) :  ("# 0x" + (instruction.Operand).ToHexString(4)));
                    string next = "[BP+" + OffsetToHex(instruction.Offset) + "]";
                    if (instruction.IsByte)
                    {
                        TCCode.PadOut("LDA 0x0100, Y", 0); // NEXTL
                        TCCode.PadOut("CMP # 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2), 0); // TOPL
                    }
                    else
                    {
                        TCCode.PadOut("DEY", 0);
                        TCCode.PadOut("LDA 0x0100, Y", 0); // NEXTH
                        TCCode.PadOut("CMP # 0x" + ((instruction.Operand).GetByte(1)).ToHexString(2), 0); // TOPH
                        TCCode.PadOut("if (Z)", 0);
                        TCCode.PadOut("{", 0);
                        TCCode.PadOut("INY", 1);
                        TCCode.PadOut("LDA 0x0100, Y", 1); // NEXTL
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
                    TCCode.OffsetTo(instruction.Offset, "X");
                    if (instruction.Operand == 0)
                    {
                        TCCode.PadOut("STZ 0x0100, X", 0);
                        if (!instruction.IsByte)
                        {
                            TCCode.PadOut("DEX", 0);
                            TCCode.PadOut("STZ 0x0100, X", 0);
                        }
                    }
                    else
                    {
                        TCCode.PadOut("LDA # 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2), 0);
                        TCCode.PadOut("STA 0x0100, X", 0);
                        if (!instruction.IsByte)
                        {
                            TCCode.PadOut("DEX", 0);
                            if ((instruction.Operand).GetByte(1) != (instruction.Operand).GetByte(0))
                            {
                                TCCode.PadOut("LDA # 0x" + ((instruction.Operand).GetByte(1)).ToHexString(2), 0);
                            }
                            TCCode.PadOut("STA 0x0100, X", 0);
                        }
                    }
                }
                case "ADDLL":
                case "ADDLLB":
                {
                    TCCode.OffsetTo(instruction.Offset, "X");
                    bool sameL = (instruction.Offset2 ==  instruction.Offset);
                    string y = "X";
                    if (!sameL)
                    {
                        TCCode.OffsetTo(instruction.Offset2, "Y"); 
                        y = "Y";
                    }
                    TCCode.PadOut("CLC", 0);
                    TCCode.PadOut("LDA 0x0100, X", 0);
                    TCCode.PadOut("ADC 0x0100, " + y, 0);
                    TCCode.PadOut("PHA", 0);
                    if (!instruction.IsByte)
                    {
                        TCCode.PadOut("DEX", 0);
                        if (!sameL)
                        {
                            TCCode.PadOut("DEY", 0);
                        }
                        TCCode.PadOut("LDA 0x0100, X", 0);
                        TCCode.PadOut("ADC 0x0100, " + y, 0);
                        TCCode.PadOut("PHA", 0);
                    }
                }
                case "ADDLGM":
                {
                    TCCode.OffsetTo(instruction.Offset, "X"); 
                    int goffset = 255 - instruction.Offset2;
                    TCCode.PadOut("CLC", 0);
                    TCCode.PadOut("LDA 0x0100, X", 0);
                    TCCode.PadOut("ADC 0x01" + (goffset.GetByte(0)).ToHexString(2), 0);
                    TCCode.PadOut("STA ZP.TOPL", 0);
                    
                    TCCode.PadOut("DEX", 0);
                    goffset--;
                    TCCode.PadOut("LDA 0x0100, X", 0);
                    TCCode.PadOut("ADC 0x01" + (goffset.GetByte(0)).ToHexString(2), 0);
                    TCCode.PadOut("STA ZP.TOPH", 0);
                    
                    TCCode.PadOut("LDA [ZP.TOP]", 0);
                    TCCode.PadOut("PHA", 0);
                
                }
                case "ADDLGIM":
                {
                    TCCode.OffsetTo(instruction.Offset, "X"); 
                    int goffset = 255 - instruction.Offset2; 
                    TCCode.PadOut("CLC", 0);
                    TCCode.PadOut("LDA 0x0100, X", 0);
                    TCCode.PadOut("ADC 0x01" + (goffset.GetByte(0)).ToHexString(2), 0);
                    TCCode.PadOut("STA ZP.TOPL", 0);
                    
                    TCCode.PadOut("DEX", 0);
                    goffset--;
                    TCCode.PadOut("LDA 0x0100, X", 0);
                    TCCode.PadOut("ADC 0x01" + (goffset.GetByte(0)).ToHexString(2), 0);
                    TCCode.PadOut("STA ZP.TOPH", 0);
                    TCCode.PadOut("LDA # 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2), 0);
                    TCCode.PadOut("STA [ZP.TOP]", 0);
                }
                case "ADDLGI":
                {
                    TCCode.OffsetTo(instruction.Offset, "X"); 
                    int goffset = 255 - instruction.Offset2;
                    TCCode.PadOut("CLC", 0);
                    TCCode.PadOut("LDA 0x0100, X", 0);
                    TCCode.PadOut("ADC 0x01" + (goffset.GetByte(0)).ToHexString(2), 0);
                    TCCode.PadOut("PHA", 0);
                    
                    TCCode.PadOut("DEX", 0);
                    goffset--;
                    TCCode.PadOut("LDA 0x0100, X", 0);
                    TCCode.PadOut("ADC 0x01" + (goffset.GetByte(0)).ToHexString(2), 0);
                    TCCode.PadOut("PHA", 0);
                    TCCode.PadOut("LDA # 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2), 0);
                    TCCode.PadOut("PHA", 0);
                }
                
                case "ADDLG":
                case "ADDLGB":
                {
                    TCCode.OffsetTo(instruction.Offset, "X"); 
                    int goffset = 255 - instruction.Offset2;
                    TCCode.PadOut("CLC", 0);
                    TCCode.PadOut("LDA 0x0100, X", 0);
                    TCCode.PadOut("ADC 0x01" + (goffset.GetByte(0)).ToHexString(2), 0);
                    TCCode.PadOut("PHA", 0);
                    if (!instruction.IsByte)
                    {
                        TCCode.PadOut("DEX", 0);
                        goffset--;
                        TCCode.PadOut("LDA 0x0100, X", 0);
                        TCCode.PadOut("ADC 0x01" + (goffset.GetByte(0)).ToHexString(2), 0);
                        TCCode.PadOut("PHA", 0);
                    }
                }
                case "ADDLLL":
                case "ADDLLLB":
                {
                    if ((instruction.Offset + 2 == instruction.Offset2) && (instruction.Offset >= 0))
                    {
                        TCCode.OffsetTo(instruction.Offset,  "X");    
                        if (instruction.Offset == 0)
                        {
                            TCCode.PadOut("PHX", 0);
                            TCCode.PadOut("PLY", 0);
                        }
                        else
                        {
                            TCCode.PadOut("TAY", 0);
                        }
                        TCCode.PadOut("DEY", 0);
                        TCCode.PadOut("DEY", 0);
                    }
                    else if ((instruction.Offset2 + 2 == instruction.Offset) && (instruction.Offset2 >= 0))
                    {
                        TCCode.OffsetTo(instruction.Offset2, "Y");
                        if (instruction.Offset == 0)
                        {
                            TCCode.PadOut("PHY", 0);
                            TCCode.PadOut("PLX", 0);
                        }
                        else
                        {
                            TCCode.PadOut("TAX", 0);
                        }
                        TCCode.PadOut("DEX", 0);
                        TCCode.PadOut("DEX", 0);
                    }
                    else
                    {
                        TCCode.OffsetTo(instruction.Offset,  "X");
                        TCCode.OffsetTo(instruction.Offset2, "Y");
                    }
                    TCCode.PadOut("CLC", 0);
                    TCCode.PadOut("LDA 0x0100, X", 0);
                    TCCode.PadOut("ADC 0x0100, Y", 0);
                    TCCode.PadOut("STA 0x0100, X", 0);
                    if (!instruction.IsByte)
                    {
                        TCCode.PadOut("DEX", 0);
                        TCCode.PadOut("DEY", 0);
                        TCCode.PadOut("LDA 0x0100, X", 0);
                        TCCode.PadOut("ADC 0x0100, Y", 0);
                        TCCode.PadOut("STA 0x0100, X", 0);
                    }
                }
                case "LIAND":
                case "LIANDB":
                {
                    TCCode.OffsetTo(instruction.Offset, "X"); 
                    if ((instruction.Operand).GetByte(0) == 0)
                    {
                        TCCode.PadOut("LDA # 0x00", 0);
                    }
                    else
                    {
                        TCCode.PadOut("LDA 0x0100, X", 0);
                        TCCode.PadOut("AND # 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2), 0);
                    }
                    TCCode.PadOut("PHA", 0);
                    if (!instruction.IsByte)
                    {
                        if ((instruction.Operand).GetByte(1) == 0)
                        {
                            TCCode.PadOut("LDA # 0x00", 0);
                        }
                        else
                        {
                            TCCode.PadOut("DEX", 0);
                            TCCode.PadOut("LDA 0x0100, X", 0);
                            TCCode.PadOut("AND # 0x" + ((instruction.Operand).GetByte(1)).ToHexString(2), 0);
                        }
                        TCCode.PadOut("PHA", 0);
                    }
                }
                case "LIANDFF":
                case "LIANDFFB":
                {
                    TCCode.OffsetTo(instruction.Offset, "X"); 
                    TCCode.PadOut("LDA 0x0100, X", 0);
                    TCCode.PadOut("PHA", 0);
                    if (!instruction.IsByte)
                    {
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
                        TCCode.OffsetTo(instruction.Offset, "X"); 
                        TCCode.PadOut("LDA 0x0100, X", 0);
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
                        TCCode.OffsetTo(instruction.Offset, "X"); 
                        TCCode.PadOut("DEX", 0);
                        TCCode.PadOut("LDA 0x0100, X", 0);
                        TCCode.PadOut("PHA", 0);
                        TCCode.PadOut("LDA # 0x00", 0);
                        TCCode.PadOut("PHA", 0);
                    }
                }
                case "ILADD":
                case "ILADDB":
                case "LIADD":
                case "LIADDB":
                {
                    TCCode.OffsetTo(instruction.Offset, "X"); 
                    TCCode.PadOut("CLC", 0);
                    TCCode.PadOut("LDA # 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2), 0);
                    TCCode.PadOut("ADC 0x0100, X", 0);
                    TCCode.PadOut("PHA", 0);
                    if (!instruction.IsByte)
                    {
                        TCCode.PadOut("DEX", 0);
                        TCCode.PadOut("LDA # 0x" + ((instruction.Operand).GetByte(1)).ToHexString(2), 0);
                        TCCode.PadOut("ADC 0x0100, X", 0);
                        TCCode.PadOut("PHA", 0);
                    }
                }
                case "ILSUB":
                case "ILSUBB":
                {
                    TCCode.OffsetTo(instruction.Offset, "X"); 
                    TCCode.PadOut("SEC", 0);
                    TCCode.PadOut("LDA # 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2), 0);
                    TCCode.PadOut("SBC 0x0100, X", 0);
                    TCCode.PadOut("PHA", 0);
                    if (!instruction.IsByte)
                    {
                        TCCode.PadOut("DEX", 0);
                        TCCode.PadOut("LDA # 0x" + ((instruction.Operand).GetByte(1)).ToHexString(2), 0);
                        TCCode.PadOut("SBC 0x0100, X", 0);
                        TCCode.PadOut("PHA", 0);
                    }
                }
                case "LISUB":
                case "LISUBB":
                {
                    TCCode.OffsetTo(instruction.Offset, "X"); 
                    TCCode.PadOut("SEC", 0);
                    TCCode.PadOut("LDA 0x0100, X", 0);
                    TCCode.PadOut("SBC # 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2), 0);
                    TCCode.PadOut("PHA", 0);
                    if (!instruction.IsByte)
                    {
                        TCCode.PadOut("DEX", 0);
                        TCCode.PadOut("LDA 0x0100, X", 0);
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
                        TCCode.OffsetTo(instruction.Offset, "X"); 
                        TCCode.PadOut("LDA 0x0100, X", 0);
                        TCCode.PadOut("PHA", 0);
                        if (!instruction.IsByte)
                        {
                            TCCode.PadOut("DEX", 0);
                            TCCode.PadOut("LDA 0x0100, X", 0);
                            TCCode.PadOut("PHA", 0);
                        }   
                    }
                    else
                    {
                        TCCode.OffsetTo(instruction.Offset, "Y"); 
                        TCCode.PadOut("LDA 0x0100, Y", 0);
                        TCCode.PadOut("STA ZP.NEXTL", 0);
                        if (!instruction.IsByte)
                        {
                            TCCode.PadOut("DEY", 0);
                            TCCode.PadOut("LDA 0x0100, Y", 0);
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
                        TCCode.OffsetTo(instruction.Offset, "X"); 
                        TCCode.PadOut("LDA 0x0100, X", 0);
                        TCCode.PadOut("PHA", 0);
                        if (!instruction.IsByte)
                        {
                            TCCode.PadOut("DEX", 0);
                            TCCode.PadOut("LDA 0x0100, X", 0);
                            TCCode.PadOut("PHA", 0);
                        }   
                    }
                    else
                    {
                        TCCode.OffsetTo(instruction.Offset, "Y"); 
                        TCCode.PadOut("LDA 0x0100, Y", 0);
                        TCCode.PadOut("STA ZP.NEXTL", 0);
                        if (!instruction.IsByte)
                        {
                            TCCode.PadOut("DEY", 0);
                            TCCode.PadOut("LDA 0x0100, Y", 0);
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
                    TCCode.OffsetTo(instruction.Offset, "Y"); 
                    TCCode.PadOut("SEC", 0);
                    TCCode.PadOut("LDA 0x0100, Y", 0);
                    TCCode.PadOut("SBC # 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2), 0);
                    TCCode.PadOut("STA ZP.TOPL", 0);
                    TCCode.PadOut("DEY", 0);
                    TCCode.PadOut("LDA 0x0100, Y", 0);
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
                    TCCode.PadOut("Not Implemented: " + name, 0);        
                }
            }
        }
        currentStream.Clear();
        InStreamMode = true;
    }
}
