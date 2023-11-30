unit HopperCode
{
    // Knobs and dials:
    const int  codeLimit   = 0x2000;   // 8K, tvplot.bas sample is currently 0x1B06 bytes (in CHECKED build)
    byte[codeLimit] programCode;       // tokenized version of the program
    
    uint            gCodeSize;         // number of code bytes currently used in programCode
    Instruction     gLastInstruction0; // last Hopper VM opcode appended to code
    Instruction     gLastInstruction1; // prior to last Hopper VM opcode appended to code
    Instruction     gLastInstruction2; // prior to last Hopper VM opcode appended to code
    bool            gRuntimeFixups;    // runtime expressions were used as the targets for GOTO / GOSUB
    bool            gBreakCheck;       // are we checking for <ctrl><X>? set by BRON / BROFF
    
    // list of locations in programCode that contain actual line numbers to be replaced 
    // addresses for the starts of each of those lines:
    <uint>          lineNumberFixups;  
    // address in programCode of the first OpCode for each line
    <uint,uint>     lineNumberAddresses;
    
    uses "/Source/Compiler/CodeGen/Instructions"
    
    uses "/Source/System/Runtime"
        
    uses "/Source/Basic/Platform" // helper methods called from the VM byte code
    
    // SYSCALL0 indices used by BASIC
    const byte iArrayGetItem        = 0x0D; // V GetItem(V[] this, uint index) system;
    const byte iArraySetItem        = 0x0E;// SetItem(V[] this, uint index, V value) system;
    const byte iListLengthGet       = 0x10; // uint Length { get system; }
    const byte iListAppend          = 0x11; // Append(<V> this, V value) system;
    const byte iListRemove          = 0x17; // Remove(<V> this, uint index) system;
    const byte iListGetItem         = 0x13; // V GetItem(<V> this, uint index) system;
    const byte iMemoryReadByte      = 0xA9; // byte ReadByte(uint address) system;
    const byte iMemoryWriteByte     = 0xAA; // WriteByte(uint address, byte value) system;
    const byte iMemoryReadBit       = 0xB6; // byte ReadBit(uint address, uint index) system;
    const byte iMemoryWriteBit      = 0xB7; // WriteBit(uint address, uint index, byte value) system;
    const byte iStringPushImmediate = 0xD3; // string PushImmediate() system;
    const byte iMemoryReadWord      = 0xD7; // uint ReadWord(uint address) system;
    const byte iMemoryWriteWord     = 0xD8; // Writeword(uint address, uint value) system;
    
    // LIBCALL indices used by BASIC
    const byte iMemoryIncWord       = 0x18; // IncWord(uint address) library;
    
    
    Instruction LastInstruction { get { return gLastInstruction0;     } } // last Hopper VM opcode appended to code
    uint        CodeSizeLimit   { get { return codeLimit;             } }
    uint        CurrentCodeSize { get { return gCodeSize;             } } // number of code bytes currently used in programCode.

    New() //called once to initialize
    {
        Clear();    
    }
     
    Clear() // called to clear previous program and prepare for new code generation
    {
        gCodeSize = 0;
        gRuntimeFixups = false;
        
        lineNumberFixups.Clear();
        lineNumberAddresses.Clear();
        
        // DIE is unused in our code so good "undefined" value
        gLastInstruction0 = Instruction.DIE;
        gLastInstruction1 = Instruction.DIE;
        gLastInstruction2 = Instruction.DIE;
    }
    
    Run(uint startLine)
    {
        uint startIndex = 0;
        if (startLine != 0)
        {
            startIndex = lineNumberAddresses[startLine];
        }
        uint exitCode = Runtime.Inline(programCode, startIndex);
    }
    
    uint CastIntToUInt(int value)
    {
        if (value < 0)
        {
            <byte> bytes = value.ToBytes();
            return uint(bytes[0]) + uint(bytes[1]) << 8;
        }
        else
        {
            return uint(value);
        }
    }
    
    appendCode(Instruction instruction)
    {
        if (gCodeSize >= codeLimit)
        {
            Error(5);  // Out of memory for code
        }
        else
        {
            programCode[gCodeSize] = byte(instruction);
            gCodeSize++;
            peepHoleOptimize(instruction);
        }
    }
    appendCode(Instruction instruction, uint operand)
    {
        if ((gCodeSize+2) >= codeLimit)
        {
            Error(5);  // Out of memory for code
        }
        else
        {
            bool isStackOffset; bool isAddressOffset; bool isRET;
            byte operands = Instructions.GetKitchenSinkWidth(instruction, ref isStackOffset, ref isAddressOffset, ref isRET);
            if (operands == 0)
            {
                Error(21, 'a'); // Internal error
            }
            byte lsb = byte(operand & 0xFF);
            byte msb = byte(operand >> 8);
            
            programCode[gCodeSize] = byte(instruction);
            gCodeSize++;
            programCode[gCodeSize] = lsb;
            gCodeSize++;
            if (operands == 2)
            {
                programCode[gCodeSize] = msb;
                gCodeSize++;
            }
            peepHoleOptimize(instruction);
        }
    }
    appendCode(Instruction instruction, int ioperand)
    {
        if ((gCodeSize+2) >= codeLimit)
        {
            Error(5);  // Out of memory for code
        }
        else
        {
            uint operand = CastIntToUInt(ioperand);
            programCode[gCodeSize] = byte(instruction);
            gCodeSize++;
            programCode[gCodeSize] = byte(operand & 0xFF);
            gCodeSize++;
            programCode[gCodeSize] = byte(operand >> 8);
            gCodeSize++;
            peepHoleOptimize(instruction);
        }
    }
    
    setCodeWord(uint address, uint value)
    {
        programCode[address]   = byte(value & 0xFF);
        programCode[address+1] = byte(value >> 8);
    }
    uint getCodeWord(uint address)
    {
        return programCode[address] + (programCode[address+1] << 8);
    }
    
    uint fixup(uint ln, bool immediate)
    {
        if ((ln < 1) || (ln > lineLimit))
        {
            if (immediate && (ln >= 10000) && (ln <= 10002))
            {
                // > lineLimit
            }
            else
            {
                Error(2);
                return 0;
            }
        }
        if (!Source.LineExists(ln))
        {
            ln = Source.GetNextLine(ln);
            if (!Source.LineExists(ln))
            {
                return gCodeSize-1; // Goto beyond end of program same as Goto <end>
            }   
        }
        return lineNumberAddresses[ln];
    }
    uint GetCurrentLineNumber() // only used for error reporting (speed doen't matter)
    {
        uint pc = Runtime.PC - Runtime.UserCode;
        
        uint bestDelta = 10000;
        uint bestLine;
        // <uint,uint>     lineNumberAddresses;
        foreach (var kv in lineNumberAddresses)
        {
            uint ln = kv.key;
            uint address = kv.value;
            if (pc >= address)
            {
                uint delta = pc - address;
                if (delta < bestDelta)
                {
                    bestLine = ln;
                    bestDelta = delta;
                }
            }
        }
        return bestLine;
    }
    
    // called from VM code to resolve runtime line number expressions
    uint lineToAddress(uint ln) 
    {
        uint dynamicAddress = fixup(ln, false);
        return UserCode+dynamicAddress;
    }
    
    Operation(Instruction instruction)
    {
#ifdef CHECKED
        bool isStackOffset; bool isAddressOffset; bool isRET;
        byte operandWidth = Instructions.GetKitchenSinkWidth(instruction, ref isStackOffset, ref isAddressOffset, ref isRET);
        if (   (operandWidth != 0)
            || isRET
            || isStackOffset
            || isAddressOffset
           )
        {
            Error(21, 'b'); // Internal system error: not an 'operation'
        }
#endif
        appendCode(instruction);
    }
#ifdef CHECKED
    DivZeroCheck()
    {
        bool needToCheck = true;
        uint operandWidth;
        uint divisor;
        if (isLastPushImmediate(ref operandWidth, ref divisor))
        {
            if (divisor != 0)
            {
                needToCheck = false;
            }
        }
        if (needToCheck)
        {
            appendCode(Instruction.DUP, 0);     //  // DUP 0 implies duplicating [top]
            appendCode(Instruction.JNZB, 8);
            appendCode(Instruction.PUSHIB, 16); // Division by zero
            appendCode(Instruction.CALLW, uint(Platform.ErrorPtr));   
            appendCode(Instruction.EXIT);   
        }
    }
#endif
    Negate()
    {
        // top = 0 - top;
        appendCode(Instruction.PUSHI0);
        appendCode(Instruction.SWAP);
        appendCode(Instruction.SUBI);
    }
    
    End()
    {
        appendCode(Instruction.EXIT); 
    }
    Rem()
    {
        appendCode(Instruction.NOP); 
    }
    bool IsPushImmediate(Instruction instruction)
    {
        if (   (instruction == Instruction.PUSHIW) || (instruction == Instruction.PUSHIB)
            || (instruction == Instruction.PUSHI0) || (instruction == Instruction.PUSHI1) || (instruction == Instruction.PUSHIM1)
           )
        {
            return true;
        }
        return false;
    }
    bool isLastPushImmediate(ref uint operandWidth, ref uint operandValue)
    {
        bool result;
        if (LastInstruction == Instruction.PUSHIW)
        {
            operandWidth = 2;
            operandValue = getCodeWord(gCodeSize - 2);
            result = true;
        }
        else if (LastInstruction == Instruction.PUSHIB)
        {
            operandWidth = 1;
            operandValue = programCode[gCodeSize-1];
            result = true;
        }
        else if (LastInstruction == Instruction.PUSHI0)
        {
            operandWidth = 0;
            operandValue = 0;
            result = true;
        }
        else if (LastInstruction == Instruction.PUSHI1)
        {
            operandWidth = 0;
            operandValue = 1;
            result = true;
        }
        else if (LastInstruction == Instruction.PUSHIM1)
        {
            operandWidth = 0;
            operandValue = 0xFFFF;
            result = true;
        }
        return result;
    }
    GotoAddress(Basic basicInstruction)
    {
        Instruction jumpOpCode = Instruction.JW;
        if (basicInstruction == Basic.Gosub)
        {
            Error(22, 'm'); // implement GOSUB/RETURN
        }
        uint operandWidth;
        uint address;
        if (isLastPushImmediate(ref operandWidth, ref address))
        {
            // can be resolved at compile time
            gCodeSize = gCodeSize - (operandWidth+1);
            int  offset  = int(address) - int(gCodeSize);
            uint uoffset = CastIntToUInt(offset);
            appendCode(jumpOpCode, uoffset); // put the address right back but as a jump opCode with an offset
        }
        else
        {
            Error(21, 'c'); // internal error : should only ever come to GotoAddress with absolute address in PUSHIW
        }
    }
    GotoLine(Basic basicInstruction, bool jumpIsJZ)
    {
        GotoLine(basicInstruction, 0, false, jumpIsJZ);
    }
    GotoLine(Basic basicInstruction, uint lineNumber, bool jumpIsJZ)
    {
        GotoLine(basicInstruction, lineNumber, true, jumpIsJZ);
    }
    GotoLine(Basic basicInstruction, uint lineNumber, bool useLineNumber, bool jumpIsJZ)
    {
        Instruction jumpOpCode = Instruction.JW;
        if (jumpIsJZ)
        {
            jumpOpCode = Instruction.JZW;
        }
        
        uint operandWidth;
        if (useLineNumber)
        {
            // can be resolved at compile time
            appendCode(jumpOpCode, lineNumber);
            lineNumberFixups.Append(gCodeSize-2);
        }
        else
        {
            // basicInstruction == Basic.Gosub:  always called with useLineNumber == false and nextJumpIsJZ = false
            if (isLastPushImmediate(ref operandWidth, ref lineNumber))
            {
                // can be resolved at compile time
                gCodeSize = gCodeSize - (operandWidth+1);
                
                if (basicInstruction == Basic.Gosub)
                {
                    // Push return address: gCodeSize+3 (gCodeSize after JW <line> below)
                    uint address = gCodeSize+3;
                    address = address + 9; // plus myself
                    if (address < 256)
                    {
                        address--;
                        appendCode(Instruction.PUSHIB, address);                        // 2 bytes
                    }
                    else
                    {
                        appendCode(Instruction.PUSHIW, address);                        // 3 bytes
                    }
                    appendCode(Instruction.CALLW, uint(Platform.PushReturnPtr));   // 3 bytes
                    appendCode(Instruction.JNZB, 3); // return stack overflow?      // 2 bytes
                    appendCode(Instruction.EXIT);                                   // 1 byte
                }
                
                appendCode(jumpOpCode, lineNumber); // put the line number right back but as a jump opCode
                lineNumberFixups.Append(gCodeSize-2);
            }
            else
            {
                uint address;
                if (basicInstruction == Basic.Gosub)
                {
                    // Push return address: gCodeSize+6 (gCodeSize after CALLW <addr>, JW <line> below)
                    address = gCodeSize+6;
                    address = address + 9; // plus myself
                    if (address < 256)
                    {
                        address--;
                        appendCode(Instruction.PUSHIB, address);                        // 2 bytes
                    }
                    else
                    {
                        appendCode(Instruction.PUSHIW, address);                        // 3 bytes
                    }
                    appendCode(Instruction.CALLW, uint(Platform.PushReturnPtr));       // 3 bytes
                    appendCode(Instruction.JNZB, 3); // return stack overflow?          // 2 bytes
                    appendCode(Instruction.EXIT);                                       // 1 bytes
                }
                
                // Runtime address calculation based on lineNumber if line came from variable or expression.
                // This address will be calculated and put on the stack at runtime:
                appendCode(Instruction.CALLW, uint(Platform.HopperLineToAddressPtr));
                appendCode(Instruction.JREL);
                gRuntimeFixups = true;
            }
        }
    }
    bool IsBreakCheck 
    { 
        get 
        { 
            return gBreakCheck; 
        } 
    }
    
    BreakCheck(bool breakCheck) // used by BRON and BROFF to turn break checking on and off
    {
        if (gBreakCheck != breakCheck)
        {
            gBreakCheck = breakCheck;
            HopperCode.Clear(); // requires a re-compile
        }
    }
    InsertBreakCheck()
    {
        if (gBreakCheck)
        {
            appendCode(Instruction.CALLW, uint(Platform.IsBreakPtr));
            appendCode(Instruction.JZB, 3);
            appendCode(Instruction.EXIT);
        }
    }
    Rnd()
    {
        appendCode(Instruction.CALLW, uint(Platform.RndPtr));
    }
    Seed()
    {
        appendCode(Instruction.CALLW, uint(Platform.SeedPtr));
    }
    Return()
    {
        appendCode(Instruction.CALLW, uint(Platform.PopReturnPtr));
        appendCode(Instruction.DUP,   0);
        appendCode(Instruction.JNZB,  5); // return stack empty?
        appendCode(Instruction.DECSP, 0);
        appendCode(Instruction.EXIT);
        appendCode(Instruction.JREL);  // J [top]
    }
    Get()
    {
        appendCode(Instruction.CALLW, uint(Platform.GetChPtr));
#ifdef CHECKED        
        appendCode(Instruction.DUP,  0); // DUP 0 implies duplicating [top], char(0) if <ctrl><X> was pressed
        appendCode(Instruction.JNZB, 3);
        appendCode(Instruction.EXIT);
#endif        
    }
    Input(byte iVariable, bool isString)
    {
        appendCode(Instruction.PUSHIB, iVariable);
        appendCode(Instruction.PUSHIB, byte(isString));
        appendCode(Instruction.CALLW, uint(Platform.InputPtr));
        appendCode(Instruction.JNZB, 3); // Input(..) returns false if <ctrl><X>
        appendCode(Instruction.EXIT);
    }
    PrintRef()
    {
        appendCode(Instruction.CALLW, uint(Platform.PrintRefPtr));
    }
    PrintChar()
    {
        // LSB = ch, MSB = null assumed to be on stack
        appendCode(Instruction.SYSCALL0, iStringPushImmediate);
        appendCode(Instruction.CALLW, uint(Platform.PrintPtr));
    }
    PrintInt()
    {
        appendCode(Instruction.CALLW, uint(Platform.IntToStringPtr));
        appendCode(Instruction.CALLW, uint(Platform.PrintPtr));
    }
    PrintHex()
    {
        appendCode(Instruction.PUSHIB, 4);
        appendCode(Instruction.CALLW, uint(Platform.UIntToStringHexPtr));
        appendCode(Instruction.CALLW, uint(Platform.PrintPtr));
    }
    PrintString(string text)
    {
#ifdef DEBUG
        if (text.Length == 0)
        {
            Error(21, 'd'); // internal error : assume we never get here with an empty string
            return;
        }
#endif   
        byte ch = 0; // MSB and LSB = 0 (null)
        uint index = text.Length;
        if ((index & 1) == 1) 
        {
            // extra odd character at the end (or a single character string)
            ch = byte(text[index-1]); // LSB
        }
        appendCode(Instruction.PUSHIB, byte(ch)); // last ch and/or null
        index = index >> 1;
        loop
        {
            if (index == 0)
            {
                break;
            }
            index--;
            appendCode(Instruction.PUSHIW, byte(text[index*2]) + (byte(text[index*2+1]) << 8));
        }
        appendCode(Instruction.SYSCALL0, iStringPushImmediate);
        appendCode(Instruction.CALLW, uint(Platform.PrintPtr));
    }
    PushTOP()
    {
        appendCode(Instruction.PUSHIW, Memory.Top);
    }
     
    PushInt(int integer)
    {
        appendCode(Instruction.PUSHIW, integer);
    }
    PeekW()
    {
        appendCode(Instruction.CALLW, uint(Platform.ReadWordPtr));
    }
    PokeW()
    {
        appendCode(Instruction.CALLW, uint(Platform.WriteWordPtr));
    }
    PeekB()
    {
        appendCode(Instruction.SYSCALL0, iMemoryReadByte);
    }
    PokeB()
    {
        appendCode(Instruction.SYSCALL0, iMemoryWriteByte);
    }
    PeekBit()
    {
        appendCode(Instruction.SYSCALL0, iMemoryReadBit);
    }
    PokeBit()
    {
        appendCode(Instruction.SYSCALL0, iMemoryWriteBit);
    }
    PokeString(byte variableIndex, string str)
    {
        HopperCode.GetVariable(variableIndex);
        uint offset = 0;
        foreach (var ch in str)
        {
            appendCode(Instruction.DUP, 0); // DUP 0 implies duplicating [top], the current value in the variable
            if (0 != offset)
            {
                appendCode(Instruction.PUSHIW, offset);
                appendCode(Instruction.ADDI);
            }
            appendCode(Instruction.PUSHIB, byte(ch));
            HopperCode.PokeB();
            offset++;
        }
        // no DUP on the last one, consume the original
        if (0 != offset)
        {
            appendCode(Instruction.PUSHIW, offset);
            appendCode(Instruction.ADDI);
        }
        appendCode(Instruction.PUSHIB, 0);
        HopperCode.PokeB();
    }
    
    StartNewLine(uint lineNumber)
    {
        lineNumberAddresses[lineNumber] = gCodeSize;
        HopperCode.ResetPeepholeBoundary();
    }
    Finish(bool isImmediate)
    {
        uint lowerBound = 0;
        if (isImmediate)
        {
            lowerBound = lineNumberAddresses[10000];
        }
        if (gLastInstruction0 != Instruction.EXIT)
        {
            appendCode(Instruction.EXIT);
        }
        // switch from line numbers to addresses:
        foreach (var fix in lineNumberFixups)
        {
            if (isImmediate)
            {
                if (fix < lowerBound)
                {
                    continue;
                }
            }
            uint lineNumber = getCodeWord(fix);
            uint address = fixup(lineNumber, isImmediate);
            int  offset  = int(address) - int(fix-1);
            uint uoffset = CastIntToUInt(offset);
            setCodeWord(fix, uoffset);
            //uint realAddress = UserCode + fix;
            //WriteLn("0x" + realAddress.ToHexString(4) + ", line:" + lineNumber.ToString() + " " + offset.ToString());
            if (Condition != Conditions.None)
            {
                break;
            }
        }
        if (!gRuntimeFixups)
        {
            lineNumberFixups.Clear();
        }
    }
    SetVariableRef(byte variableIndex)
    {
        // $<v> = <expr>
        // push variable global from the stack as address
        appendCode(Instruction.PUSHGLOBALB, Platform.Variables + variableIndex * 2);
        
        // WriteWord(uint address, uint w)
        appendCode(Instruction.SYSCALL0, iMemoryWriteWord);
    }
    SetVariable(byte variableIndex)
    {
        // pop variable global from the stack
        appendCode(Instruction.POPGLOBALB, Platform.Variables + variableIndex * 2);
        
    }
    GetVariable(byte variableIndex)
    {
        // push variable global to the stack
        appendCode(Instruction.PUSHGLOBALB, Platform.Variables + variableIndex * 2);
    }
    IncVariable(byte variableIndex)
    {
        // increment variable global to the stack
        appendCode(Instruction.INCGLOBALB, Platform.Variables + variableIndex * 2);
    }        
    UntilJump(int address)      
    {
        int offset = address - int(gCodeSize);
        uint w = CastIntToUInt(offset);
        appendCode(Instruction.JZW, offset);
    }
    
    
#ifdef DEBUG

    Disassemble(uint lineNumber)
    {
        if (!lineNumberAddresses.Contains(lineNumber)) // code exists for this line?
        {
            return;
        }
        uint pc = lineNumberAddresses[lineNumber];
        uint endPC = gCodeSize;
        if (lineNumber < Source.LineLimit+3)
        {
            uint nextLine = GetNextLine(lineNumber);
            if (lineNumberAddresses.Contains(nextLine))
            {
                endPC = lineNumberAddresses[nextLine];
            }
        }
        
        string content;
        loop
        {
            if (pc >= endPC)
            {
                break;
            }
            uint actualAddress = pc;
            byte cd = programCode[pc];
            Instruction opCode = Instruction(cd);
            string     content = Instructions.ToString(opCode);
            bool isStackOffset; bool isJumpOffset; bool isRET;
            byte operandWidth = Instructions.GetKitchenSinkWidth(opCode, ref isStackOffset, ref isJumpOffset, ref isRET);
        
            string addressContent = "0x" + actualAddress.ToHexString(4) + "  ";
            addressContent = addressContent + "0x" + cd.ToHexString(2) + " ";
            uint methodKey;
            switch (operandWidth)
            {
                case 0:
                {
                    addressContent = addressContent + "           ";
                }
                case 1:
                {
                    long jumpTarget = pc;
                    pc++;
                    byte op = programCode[pc];
                    
                    addressContent = addressContent + "0x" + op.ToHexString(2) + "       ";
                    
                    //iSysCall = op;
                    string offsetString;
                    if (isStackOffset || isJumpOffset)
                    {
                        int offset = op;
                        if (offset > 127)
                        {
                            offset = offset - 256; // 255 -> -1
                        }
                        string sign;
                        if (offset >= 0)
                        {
                            offsetString = "+";
                        }
                        offsetString = offsetString + offset.ToString();
                        jumpTarget = jumpTarget + offset;
                    }
                    if (isJumpOffset)
                    {
                        content = content + " 0x" + jumpTarget.ToHexString(4);
                        content = content + " (" + offsetString + ")";
                    }
                    else if (isStackOffset)
                    {
                        content = content + " 0x" + op.ToHexString(2);
                        content = content + " (BP" + offsetString + ")";
                    }
                    else
                    {
                        methodKey = op;
                        content = content + " 0x" + op.ToHexString(2);
                    }
                }
                case 2:
                {
                    long jumpTarget = pc;
                    pc++;
                    byte lsb = programCode[pc];
                    addressContent = addressContent + "0x" + lsb.ToHexString(2) + " ";
                    pc++;
                    byte msb = programCode[pc];
                    addressContent = addressContent + "0x" + msb.ToHexString(2) + "  ";
                    uint op = lsb + (msb << 8);
                    string offsetString;
                    if (isStackOffset || isJumpOffset)
                    {
                        long offset = op;
                        if (offset > 32767)
                        {
                            offset = offset - 65536; // 0x10000 -> -1
                        }
                        string sign;
                        if (offset >= 0)
                        {
                            offsetString = "+";
                        }
                        offsetString = offsetString + offset.ToString();
                        jumpTarget = jumpTarget + offset;
                    }
                    if (isJumpOffset)
                    {
                        content = content + " 0x" + jumpTarget.ToHexString(4);
                        content = content + " (" + offsetString + ")";
                    }
                    else if (isStackOffset)
                    {
                        content = content + " 0x" + op.ToHexString(4);
                        content = content + " (BP" + offsetString  + ")";
                    }
                    else
                    {
                        methodKey = op;
                        content = content + " " + "0x" + op.ToHexString(4);
                    }
                }
            } // switch
            string methodName;
#ifndef TERSE
            if ((Instruction.CALLIW == opCode) || (Instruction.CALLW == opCode))
            {
                if (methodKey == uint(Platform.IntToStringPtr))
                {
                    methodName = "Int.ToString(..)";
                }
                else if (methodKey == uint(Platform.UIntToStringHexPtr))
                {
                    methodName = "UInt.ToStringHex(..)";
                }
                else if (methodKey == uint(Platform.IsBreakPtr))
                {
                    methodName = "Platform.IsBreak(..)";
                }
                else if (methodKey == uint(Platform.PrintPtr))
                {
                    methodName = "Platform.Print(..)";
                }
                else if (methodKey == uint(Platform.GetChPtr))
                {
                    methodName = "Platform.GetCh(..)";
                }
                else if (methodKey == uint(Platform.RndPtr))
                {
                    methodName = "Platform.Rnd(..)";
                }
                else if (methodKey == uint(Platform.SeedPtr))
                {
                    methodName = "Platform.Seed(..)";
                }
                else if (methodKey == uint(Platform.PrintRefPtr))
                {
                    methodName = "Platform.PrintRef(..)";
                }
                else if (methodKey == uint(Platform.InputPtr))
                {
                    methodName = "Platform.Input(..)";
                }
                else if (methodKey == uint(Platform.ErrorPtr))
                {
                    methodName = "Platform.Error(..)";
                }
                else if (methodKey == uint(Platform.PushReturnPtr))
                {
                    methodName = "Platform.PushReturn(..)";
                }
                else if (methodKey == uint(Platform.PopReturnPtr))
                {
                    methodName = "Platform.PopReturn(..)";
                }
                else if (methodKey == uint(Platform.HopperLineToAddressPtr))
                {
                    methodName = "HopperCode.LineToAddress(..)";
                }
                else if (methodKey == uint(Platform.ReadWordPtr))
                {
                    methodName = "Platform.ReadWord(..)";
                }
                else if (methodKey == uint(Platform.WriteWordPtr))
                {
                    methodName = "Platform.WriteWord(..)";
                }
                else
                {
                    methodName = "Unknown Platform Method";
                }
                methodName = "    // " + methodName;
                if (Instruction.CALLW == opCode)
                {
                    methodName = " " + methodName;
                }
            }
            else if (Instruction.LIBCALL == opCode)
            {
                if (methodKey == iMemoryIncWord)
                {
                    methodName = "Memory.IncWord(..)"; // IncWord(uint address) library;
                }
                else
                {
                    methodName = "Unknown LibCall";
                }
                methodName = "     // " + methodName;
            }
            else if (Instruction.SYSCALL0 == opCode)
            {
                if (methodKey == iMemoryReadByte)
                {
                    methodName = "Memory.ReadByte(..)"; // byte ReadByte(uint address) system;
                }
                else if (methodKey == iMemoryWriteByte)
                {
                    methodName = "Memory.WriteByte(..)"; // WriteByte(uint address, byte value) system;
                }
                else if (methodKey == iMemoryReadWord)
                {
                    methodName = "Memory.ReadWord(..)"; // uint ReadWord(uint address) system;
                }
                else if (methodKey == iMemoryWriteWord)
                {
                    methodName = "Memory.WriteWord(..)"; // WriteWord(uint address, uint value) system;
                }
                else if (methodKey == iMemoryReadBit)
                {
                    methodName = "Memory.ReadBit(..)"; // byte ReadBit(uint address) system;
                }
                else if (methodKey == iMemoryWriteBit)
                {
                    methodName = "Memory.WriteBit(..)"; // WriteBit(uint address, byte value) system;
                }
                else if (methodKey == iStringPushImmediate)
                {
                    methodName = "String.PushImmediate(..)"; // PushImmediate(..) system;
                }
                else if (methodKey == iListAppend)
                {
                    methodName = "List.Append(..)";
                }
                else if (methodKey == iListLengthGet)
                {
                    methodName = "List.Length";
                }
                else if (methodKey == iListGetItem)
                {
                    methodName = "List.GetItem(..)";
                }
                else if (methodKey == iArrayGetItem)
                {
                    methodName = "Array.GetItem(..)";
                }
                else if (methodKey == iArraySetItem)
                {
                    methodName = "Array.SetItem(..)";
                }
                else if (methodKey == iListRemove)
                {
                    methodName = "List.Remove(..)";
                }
                else
                {
                    methodName = "Unknown SysCall";
                }
                methodName = "    // " + methodName;
            }
#endif // TERSE            
            pc++;
            content = "    " + addressContent + content + methodName;
            WriteLn(content);
#ifndef MCU            
            Diagnostics.OutputDebug(content);
#endif

        } // loop
    }
#endif

    ResetPeepholeBoundary()
    { 
        // DIE is unused in our code so good "undefined" value
        gLastInstruction0 = Instruction.DIE;
        gLastInstruction1 = Instruction.DIE;
        gLastInstruction2 = Instruction.DIE;
    } 
    peepHoleOptimize(Instruction instruction)
    {
        bool replaced;
#ifdef OPTIMIZER        
        if (instruction == Instruction.PUSHGLOBALB)
        {
            if (gLastInstruction0 == Instruction.PUSHGLOBALB)
            {
                instruction = Instruction.PUSHGLOBALBB;
                byte operand1 = programCode[gCodeSize-1];
                programCode[gCodeSize-4] = byte(instruction);
                programCode[gCodeSize-2] = operand1;
                gCodeSize--;
                gLastInstruction0 = instruction; replaced = true;
            }
        }
        else if (instruction == Instruction.POPGLOBALB)
        {
            if (   (gLastInstruction0 == Instruction.ADDI) 
                && (gLastInstruction1 == Instruction.PUSHGLOBALBB)
               )
            {
                instruction = Instruction.INCGLOBALBB;
                byte operand0 = programCode[gCodeSize-5];
                byte operand1 = programCode[gCodeSize-4];
                byte result   = programCode[gCodeSize-1];
                if (result == operand0)
                {
                    programCode[gCodeSize-6] = byte(instruction);
                    gCodeSize = gCodeSize - 3;
                    gLastInstruction0 = instruction; replaced = true;
                    gLastInstruction1 = gLastInstruction2;
                    gLastInstruction2 = Instruction.DIE; // undefined
                }
                else if (result == operand1)
                {
                    programCode[gCodeSize-6] = byte(instruction);
                    programCode[gCodeSize-5] = operand1;
                    programCode[gCodeSize-4] = operand0;
                    gCodeSize = gCodeSize - 3;
                    gLastInstruction0 = instruction; replaced = true;
                    gLastInstruction1 = gLastInstruction2;
                    gLastInstruction2 = Instruction.DIE; // undefined
                }
            }
            else if (   (gLastInstruction0 == Instruction.ADDI) 
                     && (gLastInstruction1 == Instruction.PUSHI1)
                    )
            {
                if (gLastInstruction2 == Instruction.PUSHGLOBALB)
                {
                    instruction = Instruction.INCGLOBALB;
                    byte operand0 = programCode[gCodeSize-5];
                    byte result   = programCode[gCodeSize-1];
                    if (result == operand0) 
                    {
                        programCode[gCodeSize-6] = byte(instruction);
                        gCodeSize = gCodeSize - 4;              
                        gLastInstruction0 = instruction; replaced = true;
                        gLastInstruction1 = Instruction.DIE; // undefined
                        gLastInstruction2 = Instruction.DIE; // undefined
                    }
                }
            }
            else if (   (gLastInstruction0 == Instruction.ADDI) 
                     && (gLastInstruction1 == Instruction.PUSHI1)
                    )
            {
                if (gLastInstruction2 == Instruction.PUSHGLOBALB)
                {
                    instruction = Instruction.INCGLOBALB;
                    byte operand0 = programCode[gCodeSize-5];
                    byte result   = programCode[gCodeSize-1];
                    if (result == operand0) 
                    {
                        programCode[gCodeSize-6] = byte(instruction);
                        gCodeSize = gCodeSize - 4;              
                        gLastInstruction0 = instruction; replaced = true;
                        gLastInstruction1 = Instruction.DIE; // undefined
                        gLastInstruction2 = Instruction.DIE; // undefined
                    }
                }
            }
            else if (   (gLastInstruction0 == Instruction.ADDI) 
                     && (gLastInstruction1 == Instruction.PUSHGLOBALB)
                    )
            {
                if (gLastInstruction2 == Instruction.PUSHI1)
                {
                    instruction = Instruction.INCGLOBALB;
                    byte operand0 = programCode[gCodeSize-4];
                    byte result   = programCode[gCodeSize-1];
                    if (result == operand0) 
                    {
                        programCode[gCodeSize-6] = byte(instruction);
                        programCode[gCodeSize-5] = result;
                        gCodeSize = gCodeSize - 4;              
                        gLastInstruction0 = instruction; replaced = true;
                        gLastInstruction1 = Instruction.DIE; // undefined
                        gLastInstruction2 = Instruction.DIE; // undefined
                    }
                }
            }
        }
        else if (instruction == Instruction.LEI)
        {
            if (gLastInstruction0 == Instruction.PUSHIB)
            {
                instruction = Instruction.PUSHIWLEI;
                programCode[gCodeSize-3] = byte(instruction);
                programCode[gCodeSize-1] = 0;
                gLastInstruction0 = instruction; replaced = true;
            }
            else if (gLastInstruction0 == Instruction.PUSHIW)
            {
                instruction = Instruction.PUSHIWLEI;
                programCode[gCodeSize-4] = byte(instruction);
                gCodeSize--;
                gLastInstruction0 = instruction; replaced = true;
            }
        }
        else if (instruction == Instruction.PUSHIW)
        {
            if (programCode[gCodeSize-1] == 0) // MSB
            {
                uint lsb = programCode[gCodeSize-2];
                if (lsb == 0)
                {
                    instruction = Instruction.PUSHI0;
                    programCode[gCodeSize-3] = byte(instruction);
                    gCodeSize = gCodeSize - 2;
                }
                else if (lsb == 1)
                {
                    instruction = Instruction.PUSHI1;
                    programCode[gCodeSize-3] = byte(instruction);
                    gCodeSize = gCodeSize - 2;
                    
                }
                else // lsb must be <= 255
                {
                    instruction = Instruction.PUSHIB;
                    programCode[gCodeSize-3] = byte(instruction);
                    // programCode[gCodeSize-2] is already the LSB
                    gCodeSize = gCodeSize - 1;
                }
            }
        }
        else if ((instruction == Instruction.ADDI) || (instruction == Instruction.ADD))
        {
            if (gLastInstruction0 == Instruction.PUSHIB)
            {
                gCodeSize = gCodeSize - 1; // remove the ADD / ADDI
                programCode[gCodeSize-2] = byte(Instruction.ADDB);
                gLastInstruction2 = Instruction.DIE; // undefined
                gLastInstruction1 = gLastInstruction2;
                gLastInstruction0 = gLastInstruction1;
                instruction = Instruction.ADDB;
                replaced = true;
            }
            else if (gLastInstruction0 == Instruction.PUSHI1)
            {
                programCode[gCodeSize-2] = byte(Instruction.ADDB);
                programCode[gCodeSize-1] = 1;
                gLastInstruction2 = Instruction.DIE; // undefined
                gLastInstruction1 = gLastInstruction2;
                gLastInstruction0 = gLastInstruction1;
                instruction = Instruction.ADDB;
                replaced = true;
            }
        }
        else if (instruction == Instruction.SWAP)
        {
            if (   Instructions.IsPushConstant(gLastInstruction0)
                && Instructions.IsPushConstant(gLastInstruction1)
               )
            {
                gCodeSize = gCodeSize - 1; // remove the SWAP
                byte width1 = Instructions.GetSimpleOperandWidth(gLastInstruction1);
                byte width0 = Instructions.GetSimpleOperandWidth(gLastInstruction0);
                uint operand0;
                uint operand1;
                uint offset = 2;
                uint index = 2;
                if (width0 == 1)
                {
                    offset = 3;
                    index = index + 1;
                    operand0 = programCode[gCodeSize-1];
                }
                else if (width0 == 2)
                {
                    offset = 4;
                    index = index + 2;
                    operand0 = programCode[gCodeSize-2] + (programCode[gCodeSize-1] << 8);
                }
                if (width1 == 1)
                {
                    index = index + 1;
                    operand1 = programCode[gCodeSize-offset];
                }
                else if (width1 == 2)
                {
                    index = index + 2;
                    operand1 = programCode[gCodeSize-(offset+1)] + (programCode[gCodeSize-offset] << 8);
                }
                programCode[gCodeSize-index] = byte(gLastInstruction0);
                index--;
                if (width0 == 1)
                {
                    programCode[gCodeSize-index] = byte(operand0);
                    index--;
                }
                else if (width0 == 2)
                {
                    programCode[gCodeSize-index] = byte(operand0 & 0xFF);
                    index--;
                    programCode[gCodeSize-index] = byte(operand0 >> 8);
                    index--;
                }
                programCode[gCodeSize-index] = byte(gLastInstruction1);
                index--;
                if (width1 == 1)
                {
                    programCode[gCodeSize-index] = byte(operand1);
                    index--;
                }
                else if (width1 == 2)
                {
                    programCode[gCodeSize-index] = byte(operand1 & 0xFF);
                    index--;
                    programCode[gCodeSize-index] = byte(operand1 >> 8);
                    index--;
                }
                
                //WriteLn(" X" + width0.ToString() + ":" + width1.ToString() + "=" + index.ToString() + " ");
                replaced = true;
                gLastInstruction0 = Instruction.DIE; // undefined
                gLastInstruction1 = Instruction.DIE; // undefined
                gLastInstruction2 = Instruction.DIE; // undefined
            }
        }
#endif        
        if (!replaced)
        {
            gLastInstruction2 = gLastInstruction1;
            gLastInstruction1 = gLastInstruction0;
            gLastInstruction0 = instruction;
        }
    }
    
}
