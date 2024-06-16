unit TinyGen
{
    uses "TinyCode"
    uses "TinyType"
    uses "TinyExpression"
    
    record Instruction
    {
        string Name;
        bool   IsByte;
        uint   Operand;
        int    Offset; // used by CALL, PUSHM, POPM as bool
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
    // Optimization opcodes:
    //
    //    INCLI
    //    ADDLL
    //    STLI
    
     
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
                }
            }
            if (instruction0.Name == "POPL")
            {
                if (instruction1.Name == "PUSHI") 
                {
                    if (instruction0.IsByte && (instruction0.IsByte == instruction1.IsByte))
                    {
                        instruction0.Operand = instruction1.Operand;
                        instruction0.Name = "STLI";
                        currentStream[currentStream.Count-1] = instruction0;
                        DeleteInstruction(currentStream.Count-2);
                        modified = true;
                        break;
                    }
                    if (!instruction0.IsByte && (instruction0.IsByte == instruction1.IsByte))
                    {
                        instruction0.Operand = instruction1.Operand;
                        instruction0.Name = "STLI";
                        currentStream[currentStream.Count-1] = instruction0;
                        DeleteInstruction(currentStream.Count-2);
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
                    if (false && (instruction3.Name == "PUSHL") && (instruction2.Name == "PUSHL") && (instruction1.Name == "ADD") && (instruction0.Name == "POPL")) // UNUSED OPT 6                   
                    {   
                        Print(" A1");
                        instruction3.Operand = UInt.FromBytes((instruction2.Offset).GetByte(0), (instruction2.Offset).GetByte(1));
                        instruction3.IsByte  = true;
                        instruction3.Name    = "ADDLL";
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
                        instruction3.Operand = UInt.FromBytes((instruction2.Offset).GetByte(0), (instruction2.Offset).GetByte(1));
                        instruction3.IsByte  = false;
                        instruction3.Name    = "ADDLL";
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
            
            case "INCLI":
            {
                content = instruction.Name + width + " [BP+" + OffsetToHex(instruction.Offset) + "] 0x" + (instruction.Operand).ToHexString(instruction.IsByte ? 2 : 4);
            }
            case "ADDLL":
            {
                content = instruction.Name + width + " [BP+" + OffsetToHex(instruction.Offset) + "] [BP+0x" + (instruction.Operand).ToHexString(2) + "]";
            }
            case "STLI":
            {
                content = instruction.Name + width + " [BP+" + OffsetToHex(instruction.Offset) + "] 0x" + (instruction.Operand).ToHexString(instruction.IsByte ? 2 : 4);
            }
            
            default:
            {
                Print(" TODO : " + instruction.Name);
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
        
        if (IsExperimental)
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
            TinyCode.PadOut("// " + ToString(instruction), 0);
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
                    TinyCode.PushWord(operand);
                }
                case "PUSHIB":  
                { 
                    TinyCode.PushByte(operand.GetByte(0));
                }
                case "POPL":
                case "POPLB":
                {
                    TinyCode.PopVariable(instruction.Data, instruction.Offset, instruction.IsByte, false);
                }
                case "POPG":
                case "POPGB":
                {
                    TinyCode.PopVariable(instruction.Data, instruction.Offset, instruction.IsByte, true);
                }
                case "PUSHL":
                case "PUSHLB":
                {
                    TinyCode.PushVariable(instruction.Data, instruction.Offset, instruction.IsByte, false);
                }
                case "PUSHG":
                case "PUSHGB":
                {
                    TinyCode.PushVariable(instruction.Data, instruction.Offset, instruction.IsByte, true);
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
                    TinyCode.PushConst(instruction.Operand);
                }
                
                case "ADD":
                case "ADDB":
                {
                    TinyOps.Add(instruction.IsByte);
                }
                case "SUB":
                case "SUBB":
                {
                    TinyOps.Sub(instruction.IsByte);
                }
                case "MUL":
                case "MULB":
                {
                    TinyOps.Mul(instruction.IsByte);
                }
                case "DIV":
                case "DIVB":
                {
                    TinyOps.Div(instruction.IsByte);
                }
                case "MOD":
                case "MODB":
                {
                    TinyOps.Mod(instruction.IsByte);
                }
                
                case "MULI":
                {
                    TinyOps.MulI();
                }
                case "DIVI":
                {
                    TinyOps.DivI();
                }
                case "MODI":
                {
                    TinyOps.ModI();
                }
                
                case "EQ":
                case "EQB":
                {
                    TinyOps.CompareEQ(instruction.IsByte);
                }
                case "NE":
                case "NEB":
                {
                    TinyOps.CompareNE(instruction.IsByte);
                }
                case "LT":
                case "LTB":
                {
                    TinyOps.CompareLT(instruction.IsByte);
                }
                case "LE":
                case "LEB":
                {
                    TinyOps.CompareLE(instruction.IsByte);
                }
                case "GT":
                case "GTB":
                {
                    TinyOps.CompareGT(instruction.IsByte);
                }
                case "GE":
                case "GEB":
                {
                    TinyOps.CompareGE(instruction.IsByte);
                }
                
                case "LTI":
                case "LTIB":
                {
                    TinyOps.CompareLTI();
                }
                case "LEI":
                case "LEIB":
                {
                    TinyOps.CompareLEI();
                }
                case "GTI":
                case "GTIB":
                {
                    TinyOps.CompareGTI();
                }
                case "GEI":
                case "GEIB":
                {
                    TinyOps.CompareGEI();
                }
                
                case "SHL":
                case "SHLB":
                {
                    TinyOps.Shl(instruction.IsByte);
                }
                case "SHR":
                case "SHRB":
                {
                    TinyOps.Shr(instruction.IsByte);
                }
                case "AND":
                case "ANDB":
                {
                    TinyOps.And(instruction.IsByte);
                }
                case "OR":
                case "ORB":
                {
                    TinyOps.Or(instruction.IsByte);
                }
                case "XOR":
                case "XORB":
                {
                    TinyOps.Xor(instruction.IsByte);
                }
                
                
                case "PADUNDER":
                {
                    TinyCode.CastPad(true);
                }
                case "DECSP":
                {
                    TinyCode.PopBytes(operand.GetByte(0), "");
                }
                case "CALL":
                {
                    TinyCode.Call(instruction.Data);
                    if (instruction.Operand != 0)
                    {
                        TinyCode.PopBytes(operand.GetByte(0), "");
                    }
                    if (instruction.Offset != 0)
                    {
                        TinyOps.PushTop(instruction.IsByte);
                    }
                }
                case "IF":
                {
                    TinyCode.If("if");
                    TinyCode.PadOut("{", 0);
                }
                case "ELSE":
                {
                    TinyCode.PadOut("}", 0);
                    TinyCode.Else();
                    TinyCode.PadOut("{", 0);
                }
                case "ENDIF":
                {
                    TinyCode.PadOut("}", 0);
                }
                
                case "INCLI":
                case "INCLIB":
                {
                    if (instruction.Operand != 0)
                    {
                        TinyCode.OffsetTo(instruction.Offset, false, "X"); 
                        if (instruction.Operand == 1)
                        {
                            TinyCode.PadOut("INC 0x0100, X", 0);
                            if (!instruction.IsByte)
                            {
                                TinyCode.PadOut("if (Z)", 0);
                                TinyCode.PadOut("{", 0);
                                TinyCode.PadOut("DEX", 1);
                                TinyCode.PadOut("INC 0x0100, X", 1);
                                TinyCode.PadOut("}", 0);
                            }
                        }
                        else
                        {
                            TinyCode.PadOut("CLC", 0);
                            TinyCode.PadOut("LDA 0x0100, X", 0);
                            TinyCode.PadOut("ADC 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2), 0);
                            TinyCode.PadOut("STA 0x0100, X", 0);
                            if (!instruction.IsByte)
                            {
                                TinyCode.PadOut("DEX", 0);
                                TinyCode.PadOut("LDA 0x0100, X", 0);
                                TinyCode.PadOut("ADC 0x" + ((instruction.Operand).GetByte(1)).ToHexString(2), 0);
                                TinyCode.PadOut("STA 0x0100, X", 0);
                            }
                        }
                    }               
                }
                case "STLI":
                case "STLIB":
                {
                    TinyCode.OffsetTo(instruction.Offset, false, "X");
                    TinyCode.PadOut("LDA # 0x" + ((instruction.Operand).GetByte(0)).ToHexString(2), 0);
                    TinyCode.PadOut("STA 0x0100, X", 0);
                    if (!instruction.IsByte)
                    {
                        TinyCode.PadOut("DEX", 0);
                        if ((instruction.Operand).GetByte(1) != (instruction.Operand).GetByte(0))
                        {
                            TinyCode.PadOut("LDA # 0x" + ((instruction.Operand).GetByte(1)).ToHexString(2), 0);
                        }
                        TinyCode.PadOut("STA 0x0100, X", 0);
                    }
                }
                case "ADDLL":
                {
                    int offset = Int.FromBytes((instruction.Operand).GetByte(0), (instruction.Operand).GetByte(1));
                    TinyCode.OffsetTo(instruction.Offset, false, "X"); 
                    TinyCode.OffsetTo(offset, false, "Y"); 
                    TinyCode.PadOut("CLC", 0);
                    TinyCode.PadOut("LDA 0x0100, X", 0);
                    TinyCode.PadOut("ADC 0x0100, Y", 0);
                    TinyCode.PadOut("STA 0x0100, X", 0);
                    if (!instruction.IsByte)
                    {
                        TinyCode.PadOut("DEX", 0);
                        TinyCode.PadOut("DEY", 0);
                        TinyCode.PadOut("LDA 0x0100, X", 0);
                        TinyCode.PadOut("ADC 0x0100, Y", 0);
                        TinyCode.PadOut("STA 0x0100, X", 0);
                    }
                }
                
                default:
                {
                    TinyCode.PadOut("TODO " + name, 0);        
                }
            }
        }
        currentStream.Clear();
        InStreamMode = true;
    }
}
