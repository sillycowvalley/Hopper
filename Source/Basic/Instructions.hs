unit Instructions
{
    uses "/Source/System/Time"
    
    uses "/Source/System/Diagnostics"
    uses "/Source/Basic/Errors"
    uses "/Source/Basic/Commands"
    uses "/Source/Basic/BasicArrays"
    
    
    
    <string,bool> reservedWords;
    
    // Tigger BASIC configurable limits:
    const uint byteCodeLimit    = 512;    // maximum number of byte code instructions for BASIC to JIT to
    const uint returnStackLimit =  32;    // maximum depth of GOSUB calls
    const uint valueStackLimit  =  32;    // maximum stack depth for expressions
    const uint variableLimit    =  32;    // maximum number of variable names
    
    // 62 bytes of globals:
    
    int[byteCodeLimit] byteCode; 
    
    uint[returnStackLimit] returnStack;
    int [valueStackLimit]  valueStack;
    byte[valueStackLimit]  typeStack;
    
    byte[variableLimit]      variableTypes;
    int[variableLimit]       variableValues; // integer values, boolean values, array addresses
    <byte,string>            variableStringValues;
    
    // only used by compilation and array identification
    <string,bool> variableExists; 
    <string,byte> variableIndices;
     
    uint byteCodeNext;
    byte variableNext;
    
    <uint, uint> byteCodeStart;
    <uint, uint> byteCodeLength;
    
    string allStrings;
    uint csp;
    uint sp;
    
    long startTime;
    
    bool gWasError;
    
    string sharedString; // used in BuildString
    
    byte gOpCode;
    byte gVariable;
    uint gCurrent;
    uint gEnd;
    uint gJumpLine;
    uint gCurrentLineNumber;
    string gOriginalLine;
    string gCurrentContent;
    
    bool goodEND;
    bool gWasPrint;
    
    delegate OpCodeDelegate();
    <byte, OpCodeDelegate> opCodeDelegates;
    
#ifdef CHECKED    
    <byte,string>  variableNames;  // used only by Tron / DASM
    bool gTronState;
    uint gTronCount;
    <byte,uint> gTronCounts;
#endif    

    const uint LineLimit = 9999;
    
    <uint, string> gSourceCode;
    uint gLastLine;
    uint LastLine { get { return gLastLine; } set { gLastLine = value; } }

    bool TryParseLineNumber(string content, ref uint lineNumber)
    {
        // variation on UInt.TryParse(..) that does a lot less (faster)
        uint length;
        byte b;
        loop
        {
            lineNumber = 0;
            length = content.Length;
            if (length == 0)
            {
                break;
            }
            if (length < 5) // 0..9999
            {
                for (uint i=0; i < length; i++)
                {
                    b = byte(content[i]);
                    lineNumber = lineNumber * 10;
                    if (b < 48)
                    {
                        lineNumber = 0;
                        break;
                    }
                    b = b - 48; // 48 is ASCII for '0'
                    if (b > 9)
                    {
                        lineNumber = 0;
                        break;
                    }
                    lineNumber = lineNumber + b; 
                }
            }
            break;
        } // loop
        if ((lineNumber < 1) || (lineNumber > LineLimit))
        {
            //Error(3, content); // Illegal line number
            return false;
        }
        return true;
    }
    SetSource(uint lineNumber, string content)
    {
        gSourceCode[lineNumber] = content;
        if (lineNumber > LastLine)
        {
            LastLine = lineNumber;
        }
    }
    string GetSource(uint lineNumber)
    {
        return gSourceCode[lineNumber];
    }
    ClearSource()
    {
        LastLine = 0;
        gSourceCode.Clear();
    }
    bool SourceLineExists(uint lineNumber)
    {
        return gSourceCode.Contains(lineNumber);
    }
    
    enum OpCode
    {
        JMP,
        GOSUB,
        RETURN,
        LET, // implied works too
        
        PRINT,
        IF,   
        JNZ,  // pop stack and GOTO xxxx if true
        JZ,   // pop stack and GOTO xxxx if false
        JLE,  // pop top and next, GOTO xxxx if next <= top
        JLT,  // pop top and next, GOTO xxxx if next <  top
        FOR,   
        NEXT,  
        WHILE, 
        WEND,  
        END,   
        REM,   
        DIM,   
        CLS,
    
        PushImmediateInteger = 32,
        PushImmediateString,  
        PushImmediateBoolean,
        PushVariable,             
        SetElement,
        GetElement,
        IncrementVariable,
        IncrementPushVariable,
        Nop,
        Add,
        Subtract,
        Multiply,
        Divide,
        Modulus,
        Negate,
        
        EQ,
        NE,
        LT,
        LE,
        GT,
        GE,
        
        GetMillis,
        GetSeconds,
        SetLED,
        DELAY,
        MID,
    }
    enum BasicType
    {
        Undefined,
        String,
        Integer,
        Boolean,
        BasicArray,
        // for Parser
        Runtime, // unknown until runtime
        // for DASM
        LineNumber,
        Identifier
    }
    
    string BasicTypeToString(BasicType bt)
    {
        switch (bt)
        {
            case BasicType.Undefined:
            {
                return "Undefined";
            }
            case BasicType.String:
            {
                return "String";
            }
            case BasicType.Integer:
            {
                return "Integer";
            }
            case BasicType.Boolean:
            {
                return "Boolean";
            }
            case BasicType.BasicArray:
            {
                return "BasicArray";
            }
            case BasicType.LineNumber:
            {
                return "LineNumber";
            }
            case BasicType.Identifier:
            {
                return "Identifier";
            }
        }
        return "BasicTypeToString not implemented";
    }
    

// TODO:
//    - | + <expression>
   
    
    bool WasError { get { return gWasError; } set { gWasError = value; } }
    
    Reset(bool clearCode)
    {
        csp = 0;
        
        if (clearCode)
        {
            byteCodeNext = 0;
            byteCodeStart.Clear();
            byteCodeLength.Clear();
            String.Build(ref allStrings);
            
            variableNext = 0;
            variableStringValues.Clear();
            variableExists.Clear();
            variableIndices.Clear();
            BasicArrays.Clear();
        }
        
#ifdef CHECKED        
        gTronCount = 0;
        gTronCounts.Clear();
#endif
        gWasPrint = false; // should already be false
        gWasError = false; // should already be false
    }
    
    BuildString(int iStart, int iLength)
    {
        uint uCurrent = uint(iStart);
        uint uLength = uint(iLength);
        String.Build(ref sharedString);
        for (uint i=0; i < uLength; i++)
        {
           char ch = allStrings[uCurrent];
           String.Build(ref sharedString, ch);
           uCurrent++;
        }
    }
    
    TrimAndUpper(string sourceContent, ref string buffer)
    {
        String.Build(ref buffer);
        uint sourceContentLength = sourceContent.Length;
        uint iStart = sourceContentLength;
        uint iLast = 0;
        for (uint i=0; i < sourceContentLength; i++)
        {
            char ch = sourceContent[i];
            if (ch != ' ')
            {
                if (i < iStart)
                {
                    iStart = i;
                }
                iLast = i;
            }
        }
        if (iStart != sourceContentLength)
        {
            for (uint i = iStart; i <= iLast; i++)
            {
                char ch = sourceContent[i];
                ch = ch.ToUpper();
                String.Build(ref buffer, ch);        
            }
        }
    }
    SubstringTrimLeft(ref string content, uint iStart)
    {
        uint contentLength = content.Length;
        loop
        {
            if (iStart >= contentLength)
            {
                break;
            }
            if (content[iStart] != ' ')
            {
                break;    
            }
            iStart++;
        }
        if (iStart < contentLength)
        {
            content = content.Substring(iStart);
        }
        else
        {
            String.Build(ref content);
        }
    }
    
#ifdef CHECKED   
    string OpCodeToString(OpCode opCode)
    {
        string result;
        switch (opCode)
        {
            case OpCode.JMP:     { result = "JMP"; }
            case OpCode.GOSUB:   { result = "GOSUB"; }
            case OpCode.RETURN:  { result = "RETURN"; }
            case OpCode.LET:     { result = "LET"; }
        
            case OpCode.PRINT:   { result = "PRINT"; }
            case OpCode.DELAY:   { result = "DELAY"; }
            //case IF,   
            case OpCode.JNZ:     { result = "JNZ"; }
            case OpCode.JZ:      { result = "JZ"; }
            case OpCode.JLE:     { result = "JLE"; }
            case OpCode.JLT:     { result = "JLT"; }
            //case FOR,   
            //case NEXT,  
            //case WHILE, 
            //case WEND,  
            case OpCode.END:    { result = "END"; }
            //case REM,   
            case OpCode.DIM:    { result = "DIM"; }
            case OpCode.CLS:    { result = "CLS"; }
    
            case OpCode.PushImmediateInteger:    { result = "PUSH"; }
            case OpCode.PushImmediateString:     { result = "PUSH"; }
            case OpCode.PushImmediateBoolean:    { result = "PUSH"; }
            case OpCode.PushVariable:            { result = "PUSH"; }
            case OpCode.SetElement:              { result = "SET"; }
            case OpCode.GetElement:              { result = "GET"; }
            case OpCode.IncrementVariable:       { result = "INC"; }
            case OpCode.IncrementPushVariable:   { result = "INCPUSH"; }
            case OpCode.Nop:      { result = "NOP"; }
            case OpCode.Add:      { result = "ADD"; }
            case OpCode.Subtract: { result = "SUB"; }
            case OpCode.Multiply: { result = "MUL"; }
            case OpCode.Divide:   { result = "DIV"; }
            case OpCode.Modulus:  { result = "MOD"; }
            case OpCode.Negate:   { result = "NEGATE"; }
        
            case OpCode.EQ:       { result = "EQ"; }
            case OpCode.NE:       { result = "NE"; }
            case OpCode.LT:       { result = "LT"; }
            case OpCode.LE:       { result = "LE"; }
            case OpCode.GT:       { result = "GT"; }
            case OpCode.GE:       { result = "GE"; }
            
            case OpCode.MID:        { result = "MID$"; }
            case OpCode.GetMillis:  { result = "GETMILLIS"; }
            case OpCode.GetSeconds: { result = "GETSECONDS"; }
            case OpCode.SetLED:  { result = "SETLED"; }
            default:
            {
                uint oc = uint(opCode);
                NotImplemented("ToString: " + oc.ToString());
            }
        }
        return result;
    }
#ifndef H6502 
    DASM(uint lineNumber)
    {
        if (byteCodeStart.Contains(lineNumber))
        {
            uint iStart  = byteCodeStart[lineNumber];
            uint iLength   = byteCodeLength[lineNumber];
            uint iCurrent = iStart;
            
            Print("   ");
            loop
            {
                byte b = byte(byteCode[iCurrent]);
                iCurrent++;
                OpCode opCode = OpCode(b);
                byte operands = 0;
                BasicType basicType = BasicType.Undefined;
                string name = OpCodeToString(opCode);
                bool asType = false;
                switch (opCode)
                {
                    case OpCode.GOSUB:
                    case OpCode.JMP:
                    case OpCode.JNZ:
                    case OpCode.JZ:
                    case OpCode.JLE:
                    case OpCode.JLT:
                    {
                        Print(name, Color.MatrixBlue, Color.Black);
                        basicType = BasicType.LineNumber;
                    }
                    
                    case OpCode.PRINT:
                    case OpCode.DELAY:
                    case OpCode.RETURN:
                    case OpCode.END:
                    case OpCode.CLS:
                    case OpCode.Nop:
                    case OpCode.Add:
                    case OpCode.Subtract:
                    case OpCode.Multiply:
                    case OpCode.Divide:
                    case OpCode.Modulus:
                    case OpCode.Negate:
                    case OpCode.LT:
                    case OpCode.GT:
                    case OpCode.EQ:
                    case OpCode.LE:
                    case OpCode.GE:
                    case OpCode.NE:
                    case OpCode.SetLED: // behave like a function
                    case OpCode.MID:
                    {
                        Print(name, Color.MatrixBlue, Color.Black);
                    }
                    case OpCode.GetMillis: // behave link a variable
                    case OpCode.GetSeconds:
                    {
                        Print(name, Color.MatrixCyan, Color.Black);
                    }
                    
                    case OpCode.DIM:
                    {
                        Print(name, Color.MatrixBlue, Color.Black);
                        basicType = BasicType.Identifier;
                        asType = true;
                    }
                    case OpCode.LET:
                    case OpCode.PushVariable:
                    case OpCode.SetElement:
                    case OpCode.GetElement:
                    case OpCode.IncrementVariable:
                    case OpCode.IncrementPushVariable:
                    {
                        Print(name, Color.MatrixBlue, Color.Black);
                        basicType = BasicType.Identifier;
                    }
                    
                    case OpCode.PushImmediateInteger:
                    {
                        Print(name, Color.MatrixBlue, Color.Black);
                        basicType = BasicType.Integer;
                    }
                    
                    case OpCode.PushImmediateString:
                    {
                        Print(name, Color.MatrixBlue, Color.Black);
                        basicType = BasicType.String;
                    }
                    case OpCode.PushImmediateBoolean:
                    {
                        Print(name, Color.MatrixBlue, Color.Black);
                        basicType = BasicType.Boolean;
                    }
                    
                    default:
                    {
                        Die(0x0A);
                    }
                } // switch
                Print(" ");
                
                // operands
                switch (basicType)
                {
                    case BasicType.Integer:
                    {
                        int integer = byteCode[iCurrent];
                        iCurrent++;
                        Print(integer.ToString() + " ", Color.MatrixRed, Color.Black);   
                    }
                    case BasicType.Boolean:
                    {
                        int integer = byteCode[iCurrent];
                        iCurrent++;
                        if (integer == 0)
                        {
                            Print("FALSE ", Color.MatrixRed, Color.Black);
                        }
                        else
                        {
                            Print("TRUE ", Color.MatrixRed, Color.Black);
                        }
                    }
                    
                    case BasicType.LineNumber: 
                    {
                        int integer = byteCode[iCurrent];
                        iCurrent++;
                        Print("#" + integer.ToString() + " ", Color.LightestGray, Color.Black);   
                    }
                    case BasicType.String:
                    {
                        int index = byteCode[iCurrent];
                        iCurrent++;
                        int length = byteCode[iCurrent];
                        iCurrent++;
                        string str = allStrings.Substring(uint(index), uint(length));
                        str = str.Replace("" + char(0x0D), "<newline>");
                        Print('"' + str + '"' + ' ', Color.MatrixRed, Color.Black);
                    }
                    case BasicType.Identifier:
                    {
                        byte nVariable = byte(byteCode[iCurrent]);
                        iCurrent++;
                        string name = variableNames[nVariable];
                        Print(name + ' ', Color.MatrixCyan, Color.Black);
                    }
                } // switch basicType
                
                if (asType)
                {
                    // DIM (xxx) AS BOOL
                    byte iType = byte(byteCode[iCurrent]);
                    iCurrent++;
                    BasicType dimType = BasicType(iType);
                    switch (dimType)
                    {
                        case BasicType.Boolean:
                        {
                            Print("AS BOOL ", Color.MatrixRed, Color.Black);
                        }
                        case BasicType.Integer:
                        {
                            Print("AS INTEGER ", Color.MatrixRed, Color.Black);
                        }
                        default:
                        {
                            Die(0x0B);
                        }
                    }
                    
                    asType = false;     
                }
                if (iCurrent == iStart + iLength)
                {
                    break;
                }
            } // loop
            PrintLn();
        }
    }
#endif
#endif    
    uint ToStringIndex(string name)
    {
        uint iString;
        if (!allStrings.IndexOf(name, ref iString))
        {
            uint nameLength = name.Length;
            iString = allStrings.Length;
            for (uint i=0; i < nameLength; i++)
            {
                String.Build(ref allStrings, name[i]);
            }
        }
        return iString;
    }
    
    int Pop(ref BasicType basicType)
    {
#ifdef CHECKED
        if (sp == 0)
        {
            Die(0x07); // argument stack overflow
        }
#endif        
        sp--;
        basicType = BasicType(typeStack[sp]);
        return valueStack[sp];
    }
    Push(int top, BasicType basicType)
    {
#ifdef CHECKED
        if (sp == valueStackLimit-1)
        {
            Die(0x07); // argument stack overflow
        }
#endif        
        valueStack[sp] = top;
        typeStack[sp] = byte(basicType);
        sp++;
    }
    bool ScanToLineNext(ref uint lineNumber, bool skipBlanks)
    {
        bool nextLineFound;
        string currentLine;
        loop
        {
            lineNumber++;
            if (SourceLineExists(lineNumber))
            {
                if (skipBlanks)
                {
                    currentLine = gSourceCode[lineNumber];
                    if (currentLine.Length != 0) // ignore empty lines
                    {
                        nextLineFound = true;
                        break;
                    }
                }
                else
                {
                    nextLineFound = true;
                    break;
                }
            }
            if (lineNumber > gLastLine)
            {
                break;
            }
        } // loop: scan for next line
        return nextLineFound;
    }
     
    bool ScanToLineMatchingWend(ref uint lineNumber)
    {
        bool nextLineFound = false;
        int match = 1;
        loop
        {
            lineNumber++;
            if (SourceLineExists(lineNumber))
            {
                string currentLine;
                TrimAndUpper(gSourceCode[lineNumber], ref currentLine);
                if (currentLine.Length != 0) // ignore empty lines
                {
                    if (currentLine.StartsWith("WHILE"))
                    {
                        match++;
                    }
                    else if (currentLine.StartsWith("WEND"))
                    {
                        match--;
                        if (match == 0)
                        {
                            nextLineFound = true;
                        				break;
                        }
                    }
            				}
            }
            if (lineNumber > gLastLine)
            {
                break;
            }
        } // loop: scan for next line
        return nextLineFound;
    }
    bool ScanToLineMatchingNext(ref uint lineNumber, string variableName)
    {
        bool nextLineFound = false;
        // at this point variableName is always:
        //  - uppercase
        //  - trimmed
        string match = "NEXT " + variableName;
        loop
        {
            lineNumber++;
            if (SourceLineExists(lineNumber))
            {
                string currentLine;
                TrimAndUpper(gSourceCode[lineNumber], ref currentLine);
                if (currentLine.Length != 0) // ignore empty lines
                {
                    while (currentLine.Contains("  "))
                    {
                        currentLine = currentLine.Replace("  ", " ");
                    }
                    if (currentLine.StartsWith(match))
                    {
                        nextLineFound = true;
                    				break;
                    }
            				}
            }
            if (lineNumber > gLastLine)
            {
                break;
            }
        } // loop: scan for next line
        return nextLineFound;
    }
    Run()
    {
        startTime = Time.Millis;
        uint currentLineNumber = 0;
        uint lastGoodLineNumber = 0;
        loop // next line
        {
            bool nextLineFound = ScanToLineNext(ref currentLineNumber, false);
            if (!nextLineFound)
            {
                break;
            }
            lastGoodLineNumber = currentLineNumber;
            uint jumpLine;
            OpCode jumpInstruction;
            if (Instructions.RunLine(ref jumpLine, ref currentLineNumber))
            {
                // stopped at END
                break;
            }
            if (gWasError)
            {
                break; // there was an error so stop
            }
            if (jumpLine != 0)
            {
                if (!SourceLineExists(jumpLine))
                {
                    Error(11, gSourceCode[lastGoodLineNumber], lastGoodLineNumber); // destination line does not exist
                    break;
                }
                currentLineNumber = jumpLine-1; // GOTO, GOSUB and RETURN (see GetNextLine)
            }
        } // loop next line
        
        if (!gWasError)
        {
            if (!Commands.Ended && (lastGoodLineNumber != 0))
            {
                //Error(9, gSourceCode[lastGoodLineNumber], lastGoodLineNumber); // 'END' expected
            }
#ifdef CHECKED
            if (Commands.Ended && gTronState && false)
            {
                TraceLn();
                Trace(" " + gTronCount.ToString() + " opCodes executed", MatrixGreen);
                
                foreach (var kv in gTronCounts)
                {
                    TraceLn();
                    byte oc = kv.key;
                    OpCode opCode = OpCode(oc);   
                    uint count = kv.value;
                    string countString = count.ToString();
                    countString = countString.LeftPad(' ', 6);
                    string opCodeString = OpCodeToString(opCode);
                    switch (opCode)
                    {
                        case OpCode.PushVariable:
                        {
                            opCodeString = opCodeString + " (V)";
                        }
                        case OpCode.PushImmediateInteger:
                        {
                            opCodeString = opCodeString + " (I)";
                        }
                        case OpCode.PushImmediateString:
                        {
                            opCodeString = opCodeString + " (S)";
                        }
                        case OpCode.PushImmediateBoolean:
                        {
                            opCodeString = opCodeString + " (B)";
                        }
                    }
                    
                    opCodeString = opCodeString.Pad(' ', 10);
                    
                    Trace("    " + opCodeString + " " + countString, MatrixBlue);
                }
            }
#endif          
        }
    }
    
    Initialize()
    {
        reservedWords["GOTO"] = true;
        reservedWords["GOSUB"] = true;
        reservedWords["RETURN"] = true;
        reservedWords["LET"] = true;
        reservedWords["PRINT"] = true;
        reservedWords["IF"] = true;
        reservedWords["FOR"] = true;
        reservedWords["NEXT"] = true;
        reservedWords["WHILE"] = true;
        reservedWords["WEND"] = true;
        reservedWords["END"] = true;
        reservedWords["REM"] = true;
        reservedWords["DIM"] = true;
        reservedWords["DELAY"] = true;
        reservedWords["CLS"] = true;
        reservedWords["MILLIS"] = true;
        reservedWords["SECONDS"] = true;
        reservedWords["MID$"] = true;
        
        OpCodeDelegate opCodeDelegate = Instructions.OpCodeJMP;
        opCodeDelegates[byte(OpCode.JMP)] = opCodeDelegate;
        opCodeDelegate = Instructions.OpCodeGOSUB;
        opCodeDelegates[byte(OpCode.GOSUB)] = opCodeDelegate;
        opCodeDelegate = Instructions.OpCodeJNZ;
        opCodeDelegates[byte(OpCode.JNZ)] = opCodeDelegate;
        opCodeDelegate = Instructions.OpCodeJZ;
        opCodeDelegates[byte(OpCode.JZ)] = opCodeDelegate;
        opCodeDelegate = Instructions.OpCodeJLE;
        opCodeDelegates[byte(OpCode.JLE)] = opCodeDelegate;
        opCodeDelegate = Instructions.OpCodeJLT;
        opCodeDelegates[byte(OpCode.JLT)] = opCodeDelegate;
        opCodeDelegate = Instructions.OpCodeRETURN;
        opCodeDelegates[byte(OpCode.RETURN)] = opCodeDelegate;
        opCodeDelegate = Instructions.OpCodeEND;
        opCodeDelegates[byte(OpCode.END)] = opCodeDelegate;
        opCodeDelegate = Instructions.OpCodeCLS;
        opCodeDelegates[byte(OpCode.CLS)] = opCodeDelegate;
        opCodeDelegate = Instructions.OpCodeNop;
        opCodeDelegates[byte(OpCode.Nop)] = opCodeDelegate;
        opCodeDelegate = Instructions.OpCodeDIM;
        opCodeDelegates[byte(OpCode.DIM)] = opCodeDelegate;
        opCodeDelegate = Instructions.OpCodeLET;
        opCodeDelegates[byte(OpCode.LET)] = opCodeDelegate;
        opCodeDelegate = Instructions.OpCodePRINT;
        opCodeDelegates[byte(OpCode.PRINT)] = opCodeDelegate;
        opCodeDelegate = Instructions.OpCodeDELAY;
        opCodeDelegates[byte(OpCode.DELAY)] = opCodeDelegate;
        opCodeDelegate = Instructions.OpCodeIncrementVariable;
        opCodeDelegates[byte(OpCode.IncrementVariable)] = opCodeDelegate;
        opCodeDelegate = Instructions.OpCodeIncrementPushVariable;
        opCodeDelegates[byte(OpCode.IncrementPushVariable)] = opCodeDelegate;
        opCodeDelegate = Instructions.OpCodeGetMillis;
        opCodeDelegates[byte(OpCode.GetMillis)] = opCodeDelegate;
        opCodeDelegate = Instructions.OpCodeGetSeconds;
        opCodeDelegates[byte(OpCode.GetSeconds)] = opCodeDelegate;
        opCodeDelegate = Instructions.OpCodeSetLED;
        opCodeDelegates[byte(OpCode.SetLED)] = opCodeDelegate;
        opCodeDelegate = Instructions.OpCodePushVariable;
        opCodeDelegates[byte(OpCode.PushVariable)] = opCodeDelegate;
        opCodeDelegate = Instructions.OpCodeSetElement;
        opCodeDelegates[byte(OpCode.SetElement)] = opCodeDelegate;
        opCodeDelegate = Instructions.OpCodeGetElement;
        opCodeDelegates[byte(OpCode.GetElement)] = opCodeDelegate;
        opCodeDelegate = Instructions.OpCodePushImmediateString;
        opCodeDelegates[byte(OpCode.PushImmediateString)] = opCodeDelegate;
        opCodeDelegate = Instructions.OpCodePushImmediateInteger;
        opCodeDelegates[byte(OpCode.PushImmediateInteger)] = opCodeDelegate;
        opCodeDelegate = Instructions.OpCodePushImmediateBoolean;
        opCodeDelegates[byte(OpCode.PushImmediateBoolean)] = opCodeDelegate;
        opCodeDelegate = Instructions.OpCodeEQ;
        opCodeDelegates[byte(OpCode.EQ)] = opCodeDelegate;
        opCodeDelegate = Instructions.OpCodeNE;
        opCodeDelegates[byte(OpCode.NE)] = opCodeDelegate;
        opCodeDelegate = Instructions.OpCodeLT;
        opCodeDelegates[byte(OpCode.LT)] = opCodeDelegate;
        opCodeDelegate = Instructions.OpCodeLE;
        opCodeDelegates[byte(OpCode.LE)] = opCodeDelegate;
        opCodeDelegate = Instructions.OpCodeGT;
        opCodeDelegates[byte(OpCode.GT)] = opCodeDelegate;
        opCodeDelegate = Instructions.OpCodeGE;
        opCodeDelegates[byte(OpCode.GE)] = opCodeDelegate;
        opCodeDelegate = Instructions.OpCodeAdd;
        opCodeDelegates[byte(OpCode.Add)] = opCodeDelegate;
        opCodeDelegate = Instructions.OpCodeSubtract;
        opCodeDelegates[byte(OpCode.Subtract)] = opCodeDelegate;
        opCodeDelegate = Instructions.OpCodeMultiply;
        opCodeDelegates[byte(OpCode.Multiply)] = opCodeDelegate;
        opCodeDelegate = Instructions.OpCodeDivide;
        opCodeDelegates[byte(OpCode.Divide)] = opCodeDelegate;
        opCodeDelegate = Instructions.OpCodeModulus;
        opCodeDelegates[byte(OpCode.Modulus)] = opCodeDelegate;
        opCodeDelegate = Instructions.OpCodeNegate;
        opCodeDelegates[byte(OpCode.Negate)] = opCodeDelegate;
        opCodeDelegate = Instructions.OpCodeMID;
        opCodeDelegates[byte(OpCode.MID)] = opCodeDelegate;
        
    }
    
    OpCodeGOSUB()
    {
#ifdef CHECKED
        if (csp == returnStackLimit-1)
        {
            Die(0x06); // call stack overflow
        }
#endif                                
        gJumpLine = uint(byteCode[gCurrent]);
        returnStack[csp] = gCurrentLineNumber+1;
        csp++;
        gCurrent++;
#ifdef CHECKED                
        if (gTronState)
        {
            Trace(" #" + gJumpLine.ToString(), LightestGray);
        }
#endif                                 
    }
    
    
    OpCodeJZ()
    {
        // conditional jump
        BasicType basicType;
        int result = Pop(ref basicType);
        if (basicType != BasicType.Boolean)
        {
            Error(20, gCurrentLineNumber); // Boolean expresion expected.
        }
        if (result == 0)
        {
            gJumpLine = uint(byteCode[gCurrent]);
#ifdef CHECKED                
            if (gTronState)
            {
                Trace(" #" + gJumpLine.ToString(), LightestGray);
            }
#endif                                                             
        }
        else
        {
#ifdef CHECKED                
            if (gTronState)
            {
                uint ln = uint(byteCode[gCurrent]);
                Trace(" #" + ln.ToString(), DarkGray);
            }
#endif
        }
        gCurrent++;
    }
    
    OpCodeRETURN()
    {
#ifdef CHECKED
        if (csp == 0)
        {
            Die(0x06); // call stack overflow
        }
#endif                                
        csp--;
        gJumpLine = returnStack[csp];
    }
    
    OpCodeEND()
    {
        Commands.Ended = true;
        goodEND = true;
    }
    OpCodeCLS()
    {
        IO.Clear();    
    }
    
    OpCodeNop()
    {
        // NOP
    }
    
    OpCodeDIM()
    {
        gVariable = byte(byteCode[gCurrent]);
        gCurrent++;
        byte bt = byte(byteCode[gCurrent]);
        BasicType elementType = BasicType(bt);
        gCurrent++;
        
        BasicType basicType;
        int elements = Pop(ref basicType);
        if ((basicType != BasicType.Integer) || (elements <= 0))
        {
            Error(29, gCurrentLineNumber); // Positive integer expression expected.
            return;
        }
        uint address = BasicArrays.DimArray(uint(elements), elementType);
        if (address == 0)
        {
            Error(27, gCurrentLineNumber); // Out of memory.
            return;
        }
        variableTypes [gVariable] = byte(BasicType.BasicArray);
        variableValues[gVariable] = int(address); // works because RAM is 0x0000..0x7FFF
#ifdef CHECKED
        if (gTronState)
        {
            Trace(" " + variableNames[gVariable], Color.MatrixCyan);
        }
#endif   
    }
    
    OpCodeLET()
    {
        gVariable = byte(byteCode[gCurrent]);
        gCurrent++;
        
        BasicType basicType;
        int integer = Pop(ref basicType);
        variableTypes[gVariable] = byte(basicType);
        if (basicType == BasicType.String)
        {
            int length = integer;
            int index = Pop(ref basicType);
            BuildString(index, length);
            variableStringValues[gVariable] = sharedString;
        }
        else
        {
            // Integer or Boolean (integer = 0 or 1)
            variableValues[gVariable] = integer;
        }
#ifdef CHECKED                
        if (gTronState)
        {
            Trace(" " + variableNames[gVariable], Color.MatrixCyan);
        }
#endif
    }
    OpCodeMID()
    {
        BasicType basicType;
        int len = Pop(ref basicType);
        if ((basicType != BasicType.Integer) || (len < 0))
        {
            Error(29, gCurrentLineNumber); // Positive integer expression expected.
            return;
        }
        int start = Pop(ref basicType);
        if ((basicType != BasicType.Integer) || (start < 0))
        {
            Error(29, gCurrentLineNumber); // Positive integer expression expected.
            return;
        }
        
        int length = Pop(ref basicType);
        if (basicType != BasicType.String)
        {
            Error(38, gCurrentLineNumber); // String expression expected.
            return;
        }
        int index = Pop(ref basicType);
        BuildString(index, length);
        
        sharedString = sharedString.Substring(uint(start), uint(len));
        
        index = int(ToStringIndex(sharedString));
        length = int(sharedString.Length);
        Push(index,  BasicType.String);
        Push(length, BasicType.String);
    }
    OpCodePRINT()
    {
        if (gWasPrint)
        {
            WriteBoth(" ", true); // single space between , delimited print expressions
        }
        BasicType basicType;
        int integer = Pop(ref basicType);
        switch (basicType)
        {
            case BasicType.Integer:
            {
                WriteBoth(integer, true);
            }
            case BasicType.String:
            {
                int length = integer;
                int index = Pop(ref basicType);
                BuildString(index, length);
                WriteBoth(sharedString, true);
            }
            case BasicType.Boolean:
            {
                if (integer == 1)
                {
                    WriteBoth("TRUE", true);
                }
                else
                {
                    WriteBoth("FALSE", true);
                }
            }
            default:
            {
                Die(0x0B);
            }
        }
        gWasPrint = true;        
    }
    
    OpCodeDELAY()
    {
        BasicType basicType;
        int integer = Pop(ref basicType);
        if ((basicType != BasicType.Integer) || (integer < 0))
        {
            Error(29, gCurrentLineNumber); // Positive integer expression expected.
            return;
        }
        Time.Delay(uint(integer));
    }
    
    OpCodeIncrementVariable()
    {
        gVariable = byte(byteCode[gCurrent]);
        gCurrent++;
        BasicType basicType = BasicType(variableTypes[gVariable]);
        if (basicType != BasicType.Integer)
        {
            Error(18, gCurrentLineNumber); // Type mismatch.
            return;
        }
        variableValues[gVariable] = variableValues[gVariable] + 1;
#ifdef CHECKED
        if (gTronState)
        {
            Trace(" " + variableNames[gVariable], Color.MatrixCyan);
        }
#endif                  
    }
    
    OpCodeSetLED()
    {
        BasicType basicType;
        int ledValue= Pop(ref basicType);   
        if (basicType != BasicType.Boolean)
        {
            Error(20, gCurrentLineNumber); // Boolean expression expected.
        }      
#ifdef H6502
        if (ledValue == 0)
        {
            Hardware.LED = false;        
        }
        else
        {
            Hardware.LED = true;        
        }
#endif
    }
    
    OpCodeGetMillis()
    {
        long now = Time.Millis;
        long elapsed = now - startTime;
        int integer;
        if (elapsed < 32768)
        {
            integer = int(elapsed);
        }
        Push(integer, BasicType.Integer);         
    }
    
    OpCodeGetSeconds()
    {
        long now = Time.Millis;
        long elapsed = now - startTime;
        elapsed = elapsed / 1000;
        int integer;
        if (elapsed < 32768)
        {
            integer = int(elapsed);
        }
        Push(integer, BasicType.Integer);
    }
    
    
    
    
    
    
    
    
    OpCodeSetElement()
    {
        gVariable = byte(byteCode[gCurrent]);
        gCurrent++;
        BasicType basicType = BasicType(variableTypes[gVariable]);
        if (basicType != BasicType.BasicArray)
        {
            Error(33, gCurrentLineNumber); // Type mismatch.
            return;       
        } 
        uint address = uint(variableValues[gVariable]); // works because RAM is 0x0000..0x7FFF
        BasicType topType;
        int index = Pop(ref topType);
        if (topType != BasicType.Integer)
        {
            Error(33, gCurrentLineNumber); // Positive integer expression expected.
            return;
        }
        // ArraySet(uint address, uint index, int value)
        int value = Pop(ref topType);
        ArraySet(address, uint(index), value, gCurrentLineNumber, topType);
        if (gWasError)
        {
            return;
        }
#ifdef CHECKED
        if (gTronState)
        {
            Trace(" " + variableNames[gVariable], Color.MatrixCyan);
        }
#endif  
    }
    
    OpCodeGetElement()
    {
        gVariable = byte(byteCode[gCurrent]);
        gCurrent++;
        BasicType basicType = BasicType(variableTypes[gVariable]);
        if (basicType != BasicType.BasicArray)
        {
            Error(33, gCurrentLineNumber); // Type mismatch.
            return;       
        } 
        uint address = uint(variableValues[gVariable]); // works because RAM is 0x0000..0x7FFF
        BasicType topType;
        int index = Pop(ref topType);
        if (topType != BasicType.Integer)
        {
            Error(33, gCurrentLineNumber); // Positive integer expression expected.
            return;
        }
        // int ArrayGet(uint address, uint index, uint currentLineNumber, ref BasicType elementType)
        BasicType elementType;
        int value = ArrayGet(address, uint(index), gCurrentLineNumber, ref elementType);
        Push(value, elementType);
#ifdef CHECKED
        if (gTronState)
        {
            Trace(" " + variableNames[gVariable], Color.MatrixCyan);
        }
#endif
    }
    
    OpCodePushImmediateString()
    {
        int 
        index = byteCode[gCurrent];
        gCurrent++;
        Push(index, BasicType.String);
        int 
        length = byteCode[gCurrent];
        gCurrent++;
        Push(length, BasicType.String);
#ifdef CHECKED
        if (gTronState)
        {
            string str = allStrings.Substring(uint(index), uint(length));
            str = str.Replace("" + char(0x0D), "<newline>");
            Trace(" " + '"' + str + '"', Color.MatrixRed);
        }
#endif
    }
    
    
    
    OpCodePushImmediateBoolean()
    {
        int integer = byteCode[gCurrent];
        gCurrent++;
        Push(integer, BasicType.Boolean);
#ifdef CHECKED                
        if (gTronState)
        {
            if (integer == 0)
            {
                Trace(" FALSE", MatrixRed);
            }
            else
            {    
                Trace(" TRUE", MatrixRed);
            }
        }
#endif       
    }
    
    OpCodeEQ()
    {
        BasicType topType;
        int top = Pop(ref topType);
        BasicType nextType;
        int next = Pop(ref nextType);
        if (topType != nextType)
        {
            Error(18, gCurrentLineNumber); // Type mismatch.
            return;
        }
        int result = 0;
        if (topType == BasicType.String)
        {
            int topi = Pop(ref topType);
            int nexti = Pop(ref nextType);
            if ((top == next) && (topi == nexti))
            {
                result = 1;
            }
        }
        else if (top == next)
        {
            result = 1;
        }
        Push(result, BasicType.Boolean);
    }
    
    OpCodeNE()
    {
        BasicType topType;
        int top = Pop(ref topType);
        BasicType nextType;
        int next = Pop(ref nextType);
        if (topType != nextType)
        {
            Error(18, gCurrentLineNumber); // Type mismatch.
            return;
        }
        int result = 1;
        if (topType == BasicType.String)
        {
            int topi = Pop(ref topType);
            int nexti = Pop(ref nextType);
            if ((top == next) && (topi == nexti))
            {
                result = 0;
            }
        }
        else if (top == next)
        {
            result = 0;
        }
        Push(result, BasicType.Boolean);
    }
    
    OpCodeLT()
    {
        BasicType topType;
        int top = Pop(ref topType);
        BasicType nextType;
        int next = Pop(ref nextType);
        if ((topType != BasicType.Integer) || (nextType != BasicType.Integer))
        {
            Error(16, gCurrentLineNumber); // Integers expected for operation.
            return;
        }
        int result = 0;
        if (next < top)
        {
            result = 1;
        }
        Push(result, BasicType.Boolean);
    }
    
    
    
    OpCodeGT()
    {
        BasicType topType;
        int top = Pop(ref topType);
        BasicType nextType;
        int next = Pop(ref nextType);
        if ((topType != BasicType.Integer) || (nextType != BasicType.Integer))
        {
            Error(16, gCurrentLineNumber); // Integers expected for operation.
            return;
        }
        int result = 0;
        if (next > top)
        {
            result = 1;
        }
        Push(result, BasicType.Boolean);
    }
    
    OpCodeGE()
    {
        BasicType topType;
        int top = Pop(ref topType);
        BasicType nextType;
        int next = Pop(ref nextType);
        if ((topType != BasicType.Integer) || (nextType != BasicType.Integer))
        {
            Error(16, gCurrentLineNumber); // Integers expected for operation.
            return;
        }
        int result = 0;
        if (next >= top)
        {
            result = 1;
        }
        Push(result, BasicType.Boolean);
    }
    
    
    OpCodeAdd()
    {
        int top;
        int next;
        BasicType iType;
        loop
        {
            top = Pop(ref iType);
            if (iType != BasicType.Integer)
            {
                Error(16, gCurrentLineNumber); // Integers expected for operation.
                break;
            }
            next = Pop(ref iType);
            if (iType != BasicType.Integer)
            {
                Error(16, gCurrentLineNumber); // Integers expected for operation.
                break;
            }
#ifdef CHECKED
            Push(next + top, BasicType.Integer);
#else
            valueStack[sp] = next + top;
            //typeStack[sp] = byte(BasicType.Integer); // unchanged
            sp++;
#endif
            break;
        }
    }
    
    OpCodeNegate()
    {
#ifdef CHECKED
        BasicType topType;
        int top = Pop(ref topType);
        if (topType != BasicType.Integer)
        {
            Error(16, gCurrentLineNumber); // Integer expected for operation.
            return;
        }
        top = -top;
        Push(top, BasicType.Integer);
#else
        if (typeStack[sp-1] != byte(BasicType.Integer))
        {
            Error(16, gCurrentLineNumber); // Integers expected for operation.
            return;
        }
        valueStack[sp-1] = -valueStack[sp-1];
#endif
    }
    
    
    OpCodeSubtract()
    {
        int top;
        int next;
        BasicType iType;
        loop
        {
            top = Pop(ref iType);
            if (iType != BasicType.Integer)
            {
                Error(16, gCurrentLineNumber); // Integers expected for operation.
                break;
            }
            next = Pop(ref iType);
            if (iType != BasicType.Integer)
            {
                Error(16, gCurrentLineNumber); // Integers expected for operation.
                break;
            }
#ifdef CHECKED
            Push(next - top, BasicType.Integer);
#else
            valueStack[sp] = next - top;
            //typeStack[sp] = byte(BasicType.Integer); // unchanged
            sp++;
#endif
            break;
        }
    }
    
    OpCodeMultiply()
    {
        int top;
        int next;
        BasicType iType;
        loop
        {
            top = Pop(ref iType);
            if (iType != BasicType.Integer)
            {
                Error(16, gCurrentLineNumber); // Integers expected for operation.
                break;
            }
            next = Pop(ref iType);
            if (iType != BasicType.Integer)
            {
                Error(16, gCurrentLineNumber); // Integers expected for operation.
                break;
            }
#ifdef CHECKED
            Push(next * top, BasicType.Integer);
#else
            valueStack[sp] = next * top;
            //typeStack[sp] = byte(BasicType.Integer); // unchanged
            sp++;
#endif
            break;
        }
    }
    
    OpCodeDivide()
    {
        int top;
        int next;
        BasicType iType;
        loop
        {
            top = Pop(ref iType);
            if (iType != BasicType.Integer)
            {
                Error(16, gCurrentLineNumber); // Integers expected for operation.
                break;
            }
            next = Pop(ref iType);
            if (iType != BasicType.Integer)
            {
                Error(16, gCurrentLineNumber); // Integers expected for operation.
                break;
            }
            if (top == 0)
            {
                Error(17, gCurrentLineNumber);
                break;
            }
#ifdef CHECKED            
            Push(next / top, BasicType.Integer);
#else
            valueStack[sp] = next / top;
            // typeStack[sp] = byte(BasicType.Integer); // unchanged
            sp++;
#endif
            break;
        }
    }
    
    OpCodeModulus()
    {
        BasicType topType;
        int top = Pop(ref topType);
        BasicType nextType;
        int next = Pop(ref nextType);
        if ((topType != BasicType.Integer) || (nextType != BasicType.Integer))
        {
            Error(16, gCurrentLineNumber); // Integers expected for operation.
            return;
        }
        if (top == 0)
        {
            Error(17, gCurrentLineNumber);
            return;
        }
        top = next % top;
        Push(top, BasicType.Integer);
    }

    NotImplemented(string what)
    {
        WriteLn("Not implemented:" + what);
        Key key = ReadKey();
    }
    
    bool VerifyIdentifier(string identifierName, bool mustExist, ref bool isArray)
    {
        bool result;
        char ch;
        char fc;
        uint length;
        uint i;
        isArray = false;
        loop
        {
            length = identifierName.Length;
            if (length == 0)
            {
                break;
            }   
            fc = identifierName[0];
            if (!fc.IsUpper())
            {
                break;
            }
            result = true;
            for (i=0; i < length; i++)
            {
                ch = identifierName[i];
                if (!ch.IsLetterOrDigit())
                {
                    if ((i == length-1) && (ch == '$')) 
                    {
                        // string identifier
                    }
                    else
                    {
                        result = false;
                        break;       
                    }
                }
            }
            break;
        }
        if (!result)
        {
            CompileError(7); // Variable identifier expected.
        }
        else if (reservedWords.Contains(identifierName))
        {
            CompileError(8); // Variable identifier cannot be reserved word.
            result = false;
        }
        else if (mustExist)
        {
            if (!variableExists.Contains(identifierName))
            {
                CompileError(31); // Undefined variable in expression.
                result = false;
            }
            else 
            {
                isArray = variableExists[identifierName];
            }
        }
        return result;
    }
    
    CompilePrimary(ref <int> code, ref BasicType compileTimeType)
    {
#ifdef CHECKED
        // some assertions / assumptions:
        if (gCurrentContent.Length == 0)
        {
            Die(0x0B);
        }
        if (gCurrentContent[0] == ' ')
        {
            Die(0x0B);
        }
#endif  
        compileTimeType = BasicType.Undefined;
        char token = gCurrentContent[0];
        gCurrentContent = gCurrentContent.Substring(1);
        loop
        {
            if (token == '(')
            {
                SubstringTrimLeft(ref gCurrentContent, 1);
                CompileSimpleExpression(ref code, ref compileTimeType);
                if (gWasError)
                {
                    break;
                }
                if (!gCurrentContent.StartsWith(')'))
                {
                    CompileError(26); // ')' expected.
                    break;        
                }
                SubstringTrimLeft(ref gCurrentContent, 1);
            }
            else if (token == '$')
            {
                // string literal
                uint iEnd;
                if (!gCurrentContent.IndexOf(' ', ref iEnd))
                {
                    CompileError(14, gCurrentContent[0]); // unexpected token 
                    break;
                }
                uint uIndex;
                uint uLength;
                string numberString = gCurrentContent.Substring(0, iEnd);
                <string> parts = numberString.Split(':');
                
                if (!UInt.TryParse(parts[0], ref uIndex))
                {
                    CompileError(15); // internal error (should never happen)
                    break;
                }
                if (!UInt.TryParse(parts[1], ref uLength))
                {
                    CompileError(15); // internal error (should never happen)
                    break;
                }
                if (uIndex+uLength > allStrings.Length)
                {
                    CompileError(15); // internal error (should never happen)
                    break;
                }
                string uString = allStrings.Substring(uIndex, uLength);
                gCurrentContent = gCurrentContent.Substring(iEnd + 1);
                code.Append(OpCode.PushImmediateString);
                code.Append(uIndex);
                code.Append(uLength);
                compileTimeType = BasicType.String;
                // string literal
            } 
            else if (token.IsDigit())
            {   
                // integer literal
                uint iEnd;
                if (!gCurrentContent.IndexOf(' ', ref iEnd))
                {
                    CompileError(14, gCurrentContent[0]); // unexpected character in expressions            
                    break;
                }
                string numberString = token + gCurrentContent.Substring(0, iEnd);
                int integer;
                if (!Int.TryParse(numberString, ref integer))
                {
                    CompileError(15); // integer expected
                    break;
                }
                gCurrentContent = gCurrentContent.Substring(iEnd + 1);
                code.Append(OpCode.PushImmediateInteger);
                code.Append(integer);
                compileTimeType = BasicType.Integer;
                // integer literal
            } 
            else if (token.IsUpper())
            {
                uint iEnd;
                if (!gCurrentContent.IndexOf(' ', ref iEnd))
                {
                    CompileError(14, gCurrentContent[0]); // unexpected character in expressions            
                    break;
                }
                string identifierName = token + gCurrentContent.Substring(0, iEnd);
                bool isArray;
                byte nVariable;
                if ((identifierName != "MILLIS") // TODO : use a dictionary
                 && (identifierName != "SECONDS")
                 && (identifierName != "TRUE")
                 && (identifierName != "FALSE")
                 && (identifierName != "MID$")
                   )
                {
                    if (!VerifyIdentifier(identifierName, true, ref isArray))
                    {
                        break;
                    }
                    // get the variable name string index
                    nVariable = variableIndices[identifierName];
                }
                SubstringTrimLeft(ref gCurrentContent, iEnd + 1);
                if (identifierName == "TRUE")
                {
                    code.Append(OpCode.PushImmediateBoolean);   
                    compileTimeType = BasicType.Boolean;
                    code.Append(1);
                }
                else if (identifierName == "FALSE")
                {
                    code.Append(OpCode.PushImmediateBoolean);
                    compileTimeType = BasicType.Boolean;
                    code.Append(0);
                }
                else if (identifierName == "MILLIS")
                {
                    // built-in 'special' variables
                    code.Append(OpCode.GetMillis);
                    compileTimeType = BasicType.Integer;
                }
                else if (identifierName == "SECONDS")
                {
                    // built-in 'special' variables
                    code.Append(OpCode.GetSeconds);
                    compileTimeType = BasicType.Integer;
                }
                else if (identifierName == "MID$")
                {
                    uint arguments = 0;
                    if (identifierName == "MID$")
                    {
                        arguments = 3;
                    }
                    // built-in functions with arguments
                    if (!gCurrentContent.StartsWith("( "))
                    {
                        CompileError(22); // "'(' expected.";
                        break;
                    }
                    gCurrentContent = gCurrentContent.Substring(2);
                    if (!gCurrentContent.EndsWith(") "))
                    {
                        CompileError(26); // "')' expected.";
                        break;
                    }
                    gCurrentContent = gCurrentContent.Substring(0, gCurrentContent.Length-2);
                    <string> parts = gCurrentContent.Split(',');
                    if (parts.Length != arguments)
                    {
                        CompileError(30); // Incorrect number of arguments for function.
                    }
                    for (uint iArg=0; iArg < arguments; iArg++)
                    {
                        string part = parts[iArg];
                        gCurrentContent = part.TrimLeft();
                        BasicType compileTimeType;
                        if (!CompileExpression(ref code, ref compileTimeType))
                        {
                            break;
                        }
                        if (iArg == 0)
                        {
                            if ((compileTimeType != BasicType.String) && (compileTimeType != BasicType.Runtime))
                            {
                                CompileError(38); // String expression expected
                                break;
                            }
                        }
                        else
                        {
                            if ((compileTimeType != BasicType.Integer) && (compileTimeType != BasicType.Runtime))
                            {
                                CompileError(40); // Integer expression expected
                                break;
                            }
                        }
                    }
                    
                    code.Append(OpCode.MID);   
                    String.Build(ref gCurrentContent);
                    compileTimeType = BasicType.String;
                }
                else if (isArray)
                {
                    // (..) index expression
                    BasicType compileTimeType;
                    CompilePrimary(ref code, ref compileTimeType);
                    if (gWasError)
                    {
                        break;
                    }
                    code.Append(OpCode.GetElement);
                    code.Append(nVariable);
                }
                else
                {
                    code.Append(OpCode.PushVariable);
                    code.Append(nVariable);
                    if (identifierName.EndsWith('$'))
                    {
                        compileTimeType = BasicType.String;
                    }
                    else
                    {
                        compileTimeType = BasicType.Runtime; // could be Integer, Boolean, Array ..
                    }
                }
            } // identifier
            else
            {
                CompileError(14, token); // unexpected character in expressions
            }
            break;
        } // loop
    }
    CompileUnary(ref <int> code, ref BasicType compileTimeType)
    {
        loop
        {
            if (gCurrentContent.Length == 0)
            {
                break;
            }
            char operator;
            if ((gCurrentContent[0] == '-') || (gCurrentContent[0] == '+'))
            {
                operator = gCurrentContent[0];
                loop
                {
                    gCurrentContent = gCurrentContent.Substring(1);
                    if (!gCurrentContent.StartsWith(' '))
                    {
                        break;
                    }
                }
            }
            CompilePrimary(ref code, ref compileTimeType);
            if (operator == '-')
            {
                if ((compileTimeType != BasicType.Integer) && (compileTimeType != BasicType.Runtime))
                {
                    CompileError(40); // Integer expression expected
                    break;
                }
                code.Append(OpCode.Negate);
            }
            break;
        } // loop
    }
   	CompileFactor(ref <int> code, ref BasicType compileTimeType)
    {
        CompileUnary(ref code, ref compileTimeType);
        loop
        {
            if (gWasError)
            {
                break;
            }
            if (gCurrentContent.Length == 0)
            {
                break;
            }
            char operator = gCurrentContent[0];
            if ((operator == '*') || (operator == '/') || (operator == '%'))
            {
                if ((compileTimeType != BasicType.Integer) && (compileTimeType != BasicType.Runtime))
                {
                    CompileError(40); // Integer expression expected
                    break;
                }
                loop
                {
                    gCurrentContent = gCurrentContent.Substring(1);
                    if (!gCurrentContent.StartsWith(' '))
                    {
                        break;
                    }
                }
                BasicType compileTimeType2;
                CompileUnary(ref code, ref compileTimeType2);
                if ((compileTimeType2 != BasicType.Integer) && (compileTimeType2 != BasicType.Runtime))
                {
                    CompileError(40); // Integer expression expected
                    break;
                }
                if (compileTimeType != compileTimeType2)
                {
                    compileTimeType = BasicType.Runtime; // at least one must be Runtime so the result is Runtime
                }
                if (gWasError)
                {
                    break;
                }
                if (operator == '*')
                {
                    code.Append(OpCode.Multiply);
                }   
                else if (operator == '/')
                {
                    code.Append(OpCode.Divide);
                }
                else if (operator == '%')
                {
                    code.Append(OpCode.Modulus);
                }
                continue;
            }
            break;
        } // loop
    }

    CompileTerm(ref <int> code, ref BasicType compileTimeType)
    {
        CompileFactor(ref code, ref compileTimeType);
        loop
        {
            if (gWasError)
            {
                break;
            }
            if (gCurrentContent.Length == 0)
            {
                break;
            }
            char operator = gCurrentContent[0];
            if ((operator == '+') || (operator == '-'))
            {
                if ((compileTimeType != BasicType.Integer) && (compileTimeType != BasicType.Runtime))
                {
                    CompileError(40); // Integer expression expected
                    break;
                }
                loop
                {
                    gCurrentContent = gCurrentContent.Substring(1);
                    if (!gCurrentContent.StartsWith(' '))
                    {
                        break;
                    }
                }
                BasicType compileTimeType2;
                CompileFactor(ref code, ref compileTimeType2);
                if ((compileTimeType2 != BasicType.Integer) && (compileTimeType2 != BasicType.Runtime))
                {
                    CompileError(40); // Integer expression expected
                    break;
                }
                if (compileTimeType != compileTimeType2)
                {
                    compileTimeType = BasicType.Runtime; // at least one must be Runtime so the result is Runtime
                }
                if (gWasError)
                {
                    break;
                }
                if (operator == '+')
                {
                    code.Append(OpCode.Add);
                }   
                else if (operator == '-')
                {
                    code.Append(OpCode.Subtract);
                }
                continue;
            }
            break;
        } // loop
    }
    CompileComparison(ref <int> code, ref BasicType compileTimeType)
    {
        CompileTerm(ref code, ref compileTimeType);
        loop
        {
            if (gWasError)
            {
                break;
            }
            if (gCurrentContent.Length == 0)
            {
                break;
            }
            char operator = gCurrentContent[0];
            if (   (operator == '<') || (operator == '>') || (operator == '=')
                || (operator == '{') || (operator == '}') || (operator == '#'))
            {
                if ((operator != '=') && (operator != '#'))
                {
                    if ((compileTimeType != BasicType.Integer) && (compileTimeType != BasicType.Runtime))
                    {
                        CompileError(40); // Integer expression expected
                        break;
                    }
                }
                loop
                {
                    gCurrentContent = gCurrentContent.Substring(1);
                    if (!gCurrentContent.StartsWith(' '))
                    {
                        break;
                    }
                }
                BasicType compileTimeType2;
                CompileTerm(ref code, ref compileTimeType2);
                if ((operator != '=') && (operator != '#'))
                {
                    if ((compileTimeType2 != BasicType.Integer) && (compileTimeType2 != BasicType.Runtime))
                    {
                        CompileError(40); // Integer expression expected
                        break;
                    }
                }
                if (gWasError)
                {
                    break;
                }
                if (compileTimeType != compileTimeType2)
                {
                    // TODO : deal with Integer = String, Integer = Boolean, Integer = ..
                    compileTimeType = BasicType.Runtime; // if one was runtime, both become runtime
                }
                switch (operator)
                {
                    case '<':
                    {
                        code.Append(OpCode.LT);
                    }
                    case '>':
                    {
                        code.Append(OpCode.GT);
                    }
                    case '=':
                    {
                        code.Append(OpCode.EQ);
                    }
                    case '{':
                    {
                        code.Append(OpCode.LE);
                    }
                    case '}':
                    {
                        code.Append(OpCode.GE);
                    }
                    case '#':
                    {
                        code.Append(OpCode.NE);
                    }
                }
                compileTimeType = BasicType.Boolean;
            }
            break;
        } // loop
    }
    
    CompileSimpleExpression(ref <int> code, ref BasicType compileTimeType)
    {
        CompileComparison(ref code, ref compileTimeType);
    }
    
    
    bool CompileExpression(ref <int> code, ref BasicType compileTimeType)
    {
        
#ifdef CHECKED
        // some assertions / assumptions:
        if (gCurrentContent.Length == 0)
        {
            WriteLn("'" + gCurrentContent + "'");
            Die(0x0B); // can't be ""
        }
        if (gCurrentContent[0] == ' ')
        {
            WriteLn("'" + gCurrentContent + "'");
            Die(0x0B);
        }
        if (gCurrentContent[gCurrentContent.Length-1] != ' ')
        {
            WriteLn("'" + gCurrentContent + "'");
            Die(0x0B); // needs trailing space, end of buffer
        }
        if (gCurrentContent.Length == 1)
        {
            WriteLn("'" + gCurrentContent + "'");
            Die(0x0B); // can't be " "
        }
        
#endif    
        CompileSimpleExpression(ref code, ref compileTimeType);
        if (gCurrentContent == " ") // end of buffer
        {
            String.Build(ref gCurrentContent);
        }
        return !gWasError;
    }
    PrepareBuffer(ref string buffer)
    {
        // Special character use:
        //   '~'           used in reserved words checking
        //   '$' and ':'   used to substitute for literal ".." strings
        //   '{'           swapped out for '<='
        //   '}'           swapped out for '>='
        //   '#'           swapped out for '<>'
        
        string strFrom;
        string strTo;
        
        // easier if all operators are single character
        if (buffer.Contains('<'))
        {
            if (buffer.Contains("<="))
            {
                String.Build(ref strFrom);
                String.Build(ref strFrom, '<');
                String.Build(ref strFrom, '=');
                String.Build(ref strTo);
                String.Build(ref strTo, '{');
                buffer = buffer.Replace(strFrom, strTo); 
            }
            if (buffer.Contains("<>"))
            {
                String.Build(ref strFrom);
                String.Build(ref strFrom, '<');
                String.Build(ref strFrom, '>');
                String.Build(ref strTo);
                String.Build(ref strTo, '#');
                buffer = buffer.Replace(strFrom, strTo);
            }    
        }
        if (buffer.Contains('>'))
        {
            if (buffer.Contains(">="))
            {
                String.Build(ref strFrom);
                String.Build(ref strFrom, '>');
                String.Build(ref strFrom, '=');
                String.Build(ref strTo);
                String.Build(ref strTo, '}');
                buffer = buffer.Replace(strFrom, strTo);
            }
        }
        
        // spaces are now the only delimiter
        foreach (var v in "(){}#<>=+-*/%,")
        {
            char ch = v;
            if (buffer.Contains(ch))
            {
                String.Build(ref strFrom);
                String.Build(ref strFrom, ch);
                String.Build(ref strTo);
                String.Build(ref strTo, ' ');
                String.Build(ref strTo, ch);
                String.Build(ref strTo, ' ');
                buffer = buffer.Replace(strFrom, strTo);
            }
        }
        if (buffer.Contains('$') && false)
        {
            String.Build(ref strFrom);
            String.Build(ref strFrom, '$');
            String.Build(ref strTo);
            String.Build(ref strTo, ' ');
            String.Build(ref strTo, '$');
            buffer = buffer.Replace(strFrom, strTo);
        }
        // only single spaces
        String.Build(ref strFrom);
        String.Build(ref strFrom, ' ');
        String.Build(ref strFrom, ' ');
        String.Build(ref strTo);
        String.Build(ref strTo, ' ');
        loop
        {
            if (!buffer.Contains(strFrom))
            {
                break;
            }
            buffer = buffer.Replace(strFrom, strTo);
        }
        buffer = buffer.Trim(); // no leading space
        // add end marker
        String.Build(ref buffer, ' ');
    }
    
    CompileError(uint number)
    {
        Error(number, gOriginalLine, gCurrentLineNumber);
    }
    CompileError(uint number, char token)
    {
        Error(number, gOriginalLine, gCurrentLineNumber, token);
    }
    
    Compile()
    {
        <int> code;
        loop
        {
            
            gOriginalLine = gSourceCode[gCurrentLineNumber];
            gCurrentContent = gOriginalLine.ToUpper();
            
            loop
            {
                // deal with the strings
                string mixedLineContent = gOriginalLine;
                uint iQuote1;
                while (gCurrentContent.IndexOf('"', ref iQuote1))
                {
                   uint iQuote2;
                   if (!gCurrentContent.IndexOf('"', iQuote1+1, ref iQuote2))
                   {
                       CompileError(12); // closing " expected
                       break;
                   }
                   // remove the string
                   string stringContent = mixedLineContent.Substring(iQuote1+1, iQuote2 - iQuote1 - 1);
                   uint iStringIndex = ToStringIndex(stringContent);
                   uint stringLength = stringContent.Length;
                   mixedLineContent = mixedLineContent.Substring(0, iQuote1) + 
                                 " $" + iStringIndex.ToString() + ":" + stringLength.ToString() +
                                 mixedLineContent.Substring(iQuote2+1);
                   gCurrentContent = gCurrentContent.Substring(0, iQuote1) + 
                                      " $" + iStringIndex.ToString()  + ":" + stringLength.ToString() +
                                      gCurrentContent.Substring(iQuote2+1);
                }
                break;
            }
            if (gWasError)
            {
                break;
            }
            
            // once "prepared", the line always starts with an uppercase word followed by a space
            PrepareBuffer(ref gCurrentContent);
            bool isEmptyLine = (gCurrentContent == " ");
            uint iSpace;
            if (!gCurrentContent.IndexOf(' ', ref iSpace))
            {
                Die(0x0B);
            }
            string word;
            for (uint i = 0; i < iSpace; i++)
            {
                String.Build(ref word, gCurrentContent[i]);
            }
            
            SubstringTrimLeft(ref gCurrentContent, iSpace);
            if (word == "LET")
            {
                if (!gCurrentContent.IndexOf(' ', ref iSpace))
                {
                    Die(0x0B);
                }
                word = gCurrentContent.Substring(0, iSpace);
                SubstringTrimLeft(ref gCurrentContent, iSpace);
            }
            
            // Scenarios:
            //   variable =
            //   variable(expression) =
            //   <reserved word>
            //   empty line
            if (isEmptyLine)
            {
                code.Append(OpCode.Nop);
                AppendTailJumpNext(ref code);
            }
            else if (!reservedWords.Contains(word))
            {
                string letVariable = word;
                bool isArray;
                if (!VerifyIdentifier(letVariable, false, ref isArray))
                {
                    break;
                }
                <int> setIndexCode;
                byte nVariable;
                bool isVariable;
                if (variableExists.Contains(letVariable))
                {
                    nVariable = variableIndices[letVariable];
                    isVariable = true; 
                    isArray = variableExists[letVariable];
                }
                if (gCurrentContent.StartsWith('('))
                {
                    // array variable
                    if (!isArray || !isVariable)
                    {
                        CompileError(33); // Array variable identifier expected.
                        break;
                    }
                    BasicType compileTimeType;
                    CompilePrimary(ref setIndexCode, ref compileTimeType);
                    if (gWasError)
                    {
                        break;
                    }
                }
                // assign expression to variable:
                if (!gCurrentContent.StartsWith('='))
                {
                    CompileError(6); // = expected
                    break;
                }
                SubstringTrimLeft(ref gCurrentContent, 1);
                BasicType compileTimeType;
                if (!CompileExpression(ref code, ref compileTimeType))
                {
                    break;
                }
                if (setIndexCode.Length > 0)
                {
                    foreach (var v in setIndexCode)
                    {
                        code.Append(v);
                    }
                    code.Append(OpCode.SetElement);
                    code.Append(nVariable);
                }
                else if (letVariable == "LED")
                {
                    code.Append(OpCode.SetLED);
                }
                else
                {
                    code.Append(OpCode.LET);
                    if (!isVariable)
                    {
#ifdef CHECKED                            
                        variableNames [variableNext] = letVariable;
#endif                        
                        variableExists[letVariable] = false;     // not array
                        variableIndices[letVariable] = variableNext;     
                        variableValues[variableNext] = 0;        // good default
                        variableStringValues[variableNext] = ""; // good default
                        
                        nVariable = variableNext;
                        variableNext++;
#ifdef CHECKED
                        if (variableNext == variableLimit)
                        {
                            CompileError(35); // Out of memory for variables.
                            break;
                        }
#endif                                
                    }
                    code.Append(nVariable);
                }   
                
                
                
                if (!gWasError)
                {
                    AppendTailJumpNext(ref code);
                }
            }
            else 
            {   // word is reserved word
                switch (word[0])
                {
                    case 'C': // CLS
                    {
                        code.Append(OpCode.CLS);
                    }
                    case 'D': // DIM
                    {
                        if (word[1] == 'I')
                        {
                            uint i = 0;
                            char delimiter;
                            if (!gCurrentContent.IndexOf(' ', ref i))
                            {
                                CompileError(7); // "Variable identifier expected.";
                                break;
                            }
                            string dimVariable = gCurrentContent.Substring(0, i);
                            dimVariable = dimVariable.Trim();
                            SubstringTrimLeft(ref gCurrentContent, i);
                            if (!gCurrentContent.StartsWith('('))
                            {
                                CompileError(22); // "'(' expected.";
                                break;
                            }
                            BasicType elementType = BasicType.Integer;
                            if (gCurrentContent.Contains(" AS BOOL "))
                            {
                                gCurrentContent = gCurrentContent.Replace(" AS BOOL ", " ");
                                elementType = BasicType.Boolean;
                            }
                            if (!gCurrentContent.EndsWith(") "))
                            {
                                CompileError(26); // "')' expected.";
                                break;
                            }
                            BasicType compileTimeType;
                            CompilePrimary(ref code, ref compileTimeType);
                            if (gWasError)
                            {
                                break;
                            }
                            if (compileTimeType != BasicType.Integer)
                            {
                                CompileError(32); // Constant integer expression expected.
                                break;    
                            }
                            byte nVariable;
                            code.Append(OpCode.DIM);
                            if (variableIndices.Contains(dimVariable))
                            {
                                nVariable = variableIndices[dimVariable];
                            }
                            else
                            {
    #ifdef CHECKED                                
                                variableNames [variableNext] = dimVariable;
    #endif
                                variableIndices[dimVariable] = variableNext;     
                                variableStringValues[variableNext] = ""; // good default
                                nVariable = variableNext;
                                variableNext++;
    #ifdef CHECKED
                                if (variableNext == variableLimit)
                                {
                                    CompileError(35); // Out of memory for variables.
                                    break;
                                }
    #endif                                
                            }
                            variableValues[variableNext] = 0;   // good default
                            variableExists[dimVariable] = true; // isArray
                            code.Append(nVariable);
                            code.Append(elementType); // Boolean or Integer
                            
                            if (!gWasError)
                            {
                                AppendTailJumpNext(ref code);
                            }
                        } // DIM
                        if (word[1] == 'E')
                        {
                            
                            BasicType compileTimeType;
                            if (!CompileExpression(ref code, ref compileTimeType))
                            {
                                break;
                            }
                            if ((compileTimeType != BasicType.Integer) && (compileTimeType != BasicType.Runtime))
                            {
                                CompileError(40); // Integer expression expected
                                break;
                            }
                            code.Append(OpCode.DELAY);
                        
                            String.Build(ref gCurrentContent);
                            if (!gWasError)
                            {
                                AppendTailJumpNext(ref code);
                            }
                        } // DELAY
                    }
                    case 'E': // END
                    {
                        code.Append(OpCode.END);
                    }
                    case 'F': // FOR
                    {
                        // identifier in FOR (no arrays allowed)
                        uint i = 0;
                        if (!gCurrentContent.IndexOf(' ', ref i))
                        {
                            CompileError(6); // "'=' expected.";
                            break;
                        }
                        string letVariable = gCurrentContent.Substring(0, i);
                        bool isArray;
                        if (!VerifyIdentifier(letVariable, false, ref isArray))
                        {
                            CompileError(7); // "Variable identifier expected.";
                            break;
                        }
                        SubstringTrimLeft(ref gCurrentContent, i);
                        
                        if (!gCurrentContent.StartsWith('='))
                        {
                            CompileError(6); // "'=' expected.";
                            break;
                        }
                        // consume '='
                        SubstringTrimLeft(ref gCurrentContent, 1);
                        BasicType compileTimeType;
                        if (!CompileExpression(ref code, ref compileTimeType))
                        {
                            break;
                        }
                        if ((compileTimeType != BasicType.Integer) && (compileTimeType != BasicType.Runtime))
                        {
                            CompileError(40); // Integer expression expected
                            break;
                        }
                        byte nVariable;
                        code.Append(OpCode.LET);
                        if (variableIndices.Contains(letVariable))
                        {
                            nVariable = variableIndices[letVariable];
                        }
                        else
                        {
#ifdef CHECKED                                
                            variableNames [variableNext] = letVariable;
#endif
                            variableExists[letVariable] = false;     // not array
                            variableIndices[letVariable] = variableNext;     
                            variableValues[variableNext] = 0;        // good default
                            variableStringValues[variableNext] = ""; // good default
                            
                            nVariable = variableNext;
                            variableNext++;
#ifdef CHECKED
                            if (variableNext == variableLimit)
                            {
                                CompileError(35); // Out of memory for variables.
                                break;
                            }
#endif                                
                        }
                        code.Append(nVariable);
                        
                        
                        uint blockLineNumber = gCurrentLineNumber;
                        if (!ScanToLineNext(ref blockLineNumber, false))
                        {
                            CompileError(25); // NEXT expected.
                            break;
                        }
                        
                        uint nextLineNumber = gCurrentLineNumber;
                        if (!ScanToLineMatchingNext(ref nextLineNumber, letVariable))
                        {
                            CompileError(25); // NEXT expected.
                            break;
                        }
                        code.Append(OpCode.JMP);
                        code.Append(blockLineNumber);
                        
                        <int> nextCode;
                        nextCode.Append(OpCode.IncrementVariable);
                        nextCode.Append(nVariable);
                        
                        nextCode.Append(OpCode.PushVariable);
                        nextCode.Append(nVariable);
                        
                        if (!gCurrentContent.StartsWith("TO "))
                        {
                            CompileError(30); // TO expected.
                            break;    
                        }
                        SubstringTrimLeft(ref gCurrentContent, 2); // trim the 'TO '
                        BasicType compileTimeType;
                        if (!CompileExpression(ref nextCode, ref compileTimeType))
                        {
                            break;
                        }
                        if ((compileTimeType != BasicType.Integer) && (compileTimeType != BasicType.Runtime))
                        {
                            CompileError(40); // Integer expression expected
                            break;
                        }
                        if (gCurrentContent.Length > 0)
                        {
                            CompileError(10); // End of line expected.
                            break;
                        }
                        
                        nextCode.Append(OpCode.JLE); // LE + JNZ
                        nextCode.Append(blockLineNumber);
                        
                        AppendTailJumpNext(ref nextCode, nextLineNumber);
                        
                        AssignCode(nextLineNumber, ref nextCode);
                    }
                    case 'G': // GOTO or GOSUB
                    {
                        gCurrentContent = gCurrentContent.Trim();
                        uint lineNumber;
                        if (!TryParseLineNumber(gCurrentContent, ref lineNumber))
                        {
                            CompileError(37); // line number expected
                            break;
                        }
                        String.Build(ref gCurrentContent);
                        if (!SourceLineExists(lineNumber))
                        {
                            CompileError(13);
                            break;
                        }
                        if (word[2] == 'T') // GOTO
                        {
                            code.Append(OpCode.JMP);
                        }
                        else
                        {
                            code.Append(OpCode.GOSUB);
                        }
                        code.Append(lineNumber);
                    }
                    case 'I': // IF
                    {
                        BasicType compileTimeType;
                        if (!CompileExpression(ref code, ref compileTimeType))
                        {
                            break;
                        }
                        if ((compileTimeType != BasicType.Boolean) && (compileTimeType != BasicType.Runtime))
                        {
                            CompileError(20); // Boolean expression expected
                            break;
                        }
                        if (!gCurrentContent.StartsWith("THEN "))
                        {
                            CompileError(19); // THEN expected.
                            break;
                        }
                        gCurrentContent = gCurrentContent.Substring(5);
                        gCurrentContent = gCurrentContent.Trim();
                        
                        // line to jump to:
                        uint lineNumber;
                        if (!TryParseLineNumber(gCurrentContent, ref lineNumber))
                        {
                            CompileError(3); // Illegal line number. Must be 1..9999.
                            break;
                        }
                        String.Build(ref gCurrentContent);
                        if (!SourceLineExists(lineNumber))
                        {
                            CompileError(13);
                            break;
                        }
                        code.Append(OpCode.JNZ);
                        code.Append(lineNumber);
                        if (!gWasError)
                        {
                            AppendTailJumpNext(ref code);
                        }
                    }
                    case 'N': // NEXT
                    {
                        // should never find NEXT in uncompiled line
                        CompileError(24); // Unmatched NEXT.
                        break;
                    }
                    case 'P': // PRINT
                    {
                        if (gCurrentContent.EndsWith("; "))
                        {
                            gCurrentContent = gCurrentContent.Substring(0, gCurrentContent.Length-2) + " ";
                        }
                        else
                        {
                            uint iStringIndex = ToStringIndex("" + char(0x0D));
                            uint stringLength = 1;
                            gCurrentContent = gCurrentContent + ", "+ "$" + iStringIndex.ToString() + ":" + stringLength.ToString() + " ";
                        }
                        <string> parts = gCurrentContent.Split(',');
                        foreach (var part in parts)
                        {
                            gCurrentContent = part.TrimLeft();
                            BasicType compileTimeType;
                            if (!CompileExpression(ref code, ref compileTimeType))
                            {
                                break;
                            }
                            code.Append(OpCode.PRINT);
                        }
                        String.Build(ref gCurrentContent);
                        if (!gWasError)
                        {
                            AppendTailJumpNext(ref code);
                        }
                    }
                    case 'R': // RETURN or REM
                    {
                        if (word[2] == 'T') // RETURN
                        {
                            code.Append(OpCode.RETURN);
                        }
                        else // REM
                        {
                            code.Append(OpCode.Nop);
                            String.Build(ref gCurrentContent);
                            AppendTailJumpNext(ref code);
                        }
                    }
                    case 'W':
                    {
                        if (word[1] == 'E') // WEND
                        {
                            // should never find WEND in uncompiled line
                            CompileError(21); // Unmatched WEND.
                            break;
                        }
                        // WHILE
                        <int> wendCode;
                        BasicType compileTimeType;
                        if (!CompileExpression(ref wendCode, ref compileTimeType))
                        {
                            break;
                        }
                        if ((compileTimeType != BasicType.Boolean) && (compileTimeType != BasicType.Runtime))
                        {
                            CompileError(20); // Boolean expression expected
                            break;
                        }
                        if (gCurrentContent.Length > 0)
                        {
                            CompileError(10); // End of line expected.
                            break;
                        }
                        uint blockLineNumber = gCurrentLineNumber;
                        if (!ScanToLineNext(ref blockLineNumber, false))
                        {
                            CompileError(23); // WEND expected.
                            break;
                        }
                        uint wendLineNumber = gCurrentLineNumber;
                        if (!ScanToLineMatchingWend(ref wendLineNumber))
                        {
                            CompileError(23); // WEND expected.
                            break;
                        }
                        code.Append(OpCode.JMP);
                        code.Append(wendLineNumber);
                        
                        wendCode.Append(OpCode.JNZ);
                        wendCode.Append(blockLineNumber);
                        
                        AppendTailJumpNext(ref wendCode, wendLineNumber);
                        
                        AssignCode(wendLineNumber, ref wendCode);
                    }
                    default:
                    {
                        Die(0x0B);
                    }
                } // switch
                if (gWasError)
                {
                    break;
                }
                if (gCurrentContent.Length != 0)
                {
                    CompileError(10); // unexpected trailing content
                }
            }
            break;
        } // loop
        if (!gWasError)
        {
            AssignCode(gCurrentLineNumber, ref code);
        }
    }
	   bool InByteRange(int i)
    {
        return (i >= 0) && (i <= 255);
    }
    Optimize(ref <int> code)
    {
        loop
        {
            uint cLength = code.Length;
            if ((cLength >= 7) && InByteRange(code[0]))
            {
                OpCode first = OpCode(byte(code[0]));
                if ((first == OpCode.PushVariable) && InByteRange(code[2]))
                {
                    OpCode push1 = OpCode(byte(code[2]));
                    if ((push1 == OpCode.PushImmediateInteger) && InByteRange(code[4]))
                    {
                        OpCode add = OpCode(byte(code[4]));
                        if ((add == OpCode.Add)  && InByteRange(code[5]))
                        {
                            OpCode let = OpCode(byte(code[5]));
                            if (let == OpCode.LET)
                            {
                                // PUSH <V> PUSH 1 ADD LET <V> -> INC <V>
                                if ((code[1] == code[6]) && (code[3] == 1))
                                {
                                    code.SetItem(0, int(OpCode.IncrementVariable));
                                    code.Remove(2);
                                    code.Remove(2);
                                    code.Remove(2);
                                    code.Remove(2);
                                    code.Remove(2);
                                    continue; // another?
                                }
                            }
                        }
                    }
                }
                if (   InByteRange(code[cLength - 7])
                    && InByteRange(code[cLength - 5])
                    && InByteRange(code[cLength - 4])
                   )
                {
                    OpCode push = OpCode(byte(code[cLength - 7]));
                    OpCode eq   = OpCode(byte(code[cLength - 5]));
                    OpCode jnz  = OpCode(byte(code[cLength - 4]));
                    if ((push == OpCode.PushImmediateBoolean) 
                     && (code[cLength - 6] == 0)
                     && (eq == OpCode.EQ) 
                     && (jnz == OpCode.JNZ))
                    {
                        // PUSH FALSE EQ JNZ # JMP # -> JZ # JMP #
                        code.SetItem(cLength - 4, int(OpCode.JZ));
                        code.Remove(cLength - 5);     // EQ
                        code.Remove(code.Length - 5); // FALSE
                        code.Remove(code.Length - 5); // PUSH
                        continue; // another?
                    }
                }
            }
            if (   (cLength >= 5)
                && InByteRange(code[cLength - 5])
                && InByteRange(code[cLength - 4])
               )
            {
                OpCode le   = OpCode(byte(code[cLength - 5]));
                OpCode jnz  = OpCode(byte(code[cLength - 4]));
                
                if ((le  == OpCode.LE) && (jnz == OpCode.JNZ))
                {
                    // LE JNZ # JMP # -> JLE # JMP #
                    code.SetItem(cLength - 4, int(OpCode.JLE));
                    code.Remove(code.Length - 5); // LE
                    continue; // another?
                }
                if ((le  == OpCode.LT) && (jnz == OpCode.JNZ))
                {
                    // LT JNZ # JMP # -> JLT # JMP #
                    code.SetItem(cLength - 4, int(OpCode.JLT));
                    code.Remove(code.Length - 5); // LT
                    continue; // another?
                }
            }
            if (   (cLength >= 4)
                && InByteRange(code[0])
                && InByteRange(code[2])
               )
            {
                OpCode first = OpCode(byte(code[0]));
                if (first == OpCode.IncrementVariable)
                {
                    OpCode next = OpCode(byte(code[2]));
                    if (next == OpCode.PushVariable)
                    {
                        // same variable?
                        if (code[1] == code[3])
                        {
                            // INC <V> PUSH <V> -> INC_PUSH <V>
                            code.SetItem(0, int(OpCode.IncrementPushVariable));
                            code.Remove(2);
                            code.Remove(2);
                            continue; // another?
                        }
                    }
                }
            }
            if (   (cLength >= 3)
                && InByteRange(code[0])
                && InByteRange(code[1])
               )
            {
                OpCode first = OpCode(byte(code[0]));
                if (first == OpCode.Nop)
                {
                    OpCode next = OpCode(byte(code[1]));
                    if (next == OpCode.JMP)
                    {
                        // NOP JMP # -> JMP #
                        code.Remove(0);
                        continue; // another?
                    }
                }
            }
            
            break;
        } // loop
    }

    AssignCode(uint currentLineNumber, ref <int> code)
    {
        Optimize(ref code);
        
        uint iStart  = byteCodeNext;
        uint iLength = code.Length;
        foreach (var bc in code)
        {
#ifdef CHECKED
            if (byteCodeNext == byteCodeLimit)
            {
                Error(34, currentLineNumber); // Out of memory for code.
                break;
            }
#endif            
            byteCode[byteCodeNext] = bc;
            byteCodeNext++;
        }
        byteCodeStart[currentLineNumber] = iStart;
        byteCodeLength[currentLineNumber] = iLength;
    }
	
    AppendTailJumpNext(ref <int> code)
    {
        AppendTailJumpNext(ref code, gCurrentLineNumber);
    }
    AppendTailJumpNext(ref <int> code, uint lineNumber)
    {
        bool foundNextLine = ScanToLineNext(ref lineNumber, false);
        if (foundNextLine)
        {
            code.Append(OpCode.JMP);
            code.Append(lineNumber);
        }
    }
    
    
    // Inlined:
    OpCodeJMP()
    {
        gJumpLine = uint(byteCode[gCurrent]);
        gCurrent++;
        
#ifdef CHECKED                
        if (gTronState)
        {
            Trace(" #" + gJumpLine.ToString(), LightestGray);
        }
#endif                 
    }
    
    OpCodeJNZ()
    {
        // conditional jump
#ifdef CHECKED        
        BasicType basicType;
        int result = Pop(ref basicType);
        if (basicType != BasicType.Boolean)
        {
            Error(20, gCurrentLineNumber); // Boolean expresion expected.
        }
        if (result != 0)
        {
            gJumpLine = uint(byteCode[gCurrent]);

            if (gTronState)
            {
                Trace(" #" + gJumpLine.ToString(), LightestGray);
            }
        }
        else
        {
            if (gTronState)
            {
                uint ln = uint(byteCode[gCurrent]);
                Trace(" #" + ln.ToString(), DarkGray);
            }
        }
        gCurrent++;
#else
        sp--;
        if (typeStack[sp] != byte(BasicType.Boolean))
        {
            Error(20, gCurrentLineNumber); // Boolean expresion expected.
        }
        if (valueStack[sp] != 0)
        {
            gJumpLine = uint(byteCode[gCurrent]);
        }
        gCurrent++;
#endif
    }
    
    OpCodeJLE()
    {
        // conditional jump
#ifdef CHECKED        
        BasicType topType;
        int top = Pop(ref topType);
        BasicType nextType;
        int next = Pop(ref nextType);
        if ((topType != BasicType.Integer) || (nextType != BasicType.Integer))
        {
            Error(16, gCurrentLineNumber); // Integers expected for operation.
            return;
        }
        
        if (next <= top)
        {
            gJumpLine = uint(byteCode[gCurrent]);

            if (gTronState)
            {
                Trace(" #" + gJumpLine.ToString(), LightestGray);
            }

        }
        else
        {
            if (gTronState)
            {
                uint ln = uint(byteCode[gCurrent]);
                Trace(" #" + ln.ToString(), DarkGray);
            }
        }
        gCurrent++;
#else
        sp--;
        sp--;
        if ((typeStack[sp] != byte(BasicType.Integer)) || (typeStack[sp+1] != byte(BasicType.Integer)))
        {
            Error(16, gCurrentLineNumber); // Integers expected for operation.
            return;
        }
        if (valueStack[sp] <= valueStack[sp+1])
        {
            gJumpLine = uint(byteCode[gCurrent]);
        }
        gCurrent++;
#endif
    }
    
    OpCodeJLT()
    {
        // conditional jump
#ifdef CHECKED        
        BasicType topType;
        int top = Pop(ref topType);
        BasicType nextType;
        int next = Pop(ref nextType);
        if ((topType != BasicType.Integer) || (nextType != BasicType.Integer))
        {
            Error(16, gCurrentLineNumber); // Integers expected for operation.
            return;
        }
        
        if (next < top)
        {
            gJumpLine = uint(byteCode[gCurrent]);
            if (gTronState)
            {
                Trace(" #" + gJumpLine.ToString(), LightestGray);
            }
        }
        else
        {
            if (gTronState)
            {
                uint ln = uint(byteCode[gCurrent]);
                Trace(" #" + ln.ToString(), DarkGray);
            }
        }
        gCurrent++;
#else
        sp--;
        sp--;
        if ((typeStack[sp] != byte(BasicType.Integer)) || (typeStack[sp+1] != byte(BasicType.Integer)))
        {
            Error(16, gCurrentLineNumber); // Integers expected for operation.
            return;
        }
        if (valueStack[sp] < valueStack[sp+1])
        {
            gJumpLine = uint(byteCode[gCurrent]);
        }
        gCurrent++;
#endif
    }
    OpCodeIncrementPushVariable()
    {
        // IncrementVariable followed by PushVariable
#ifdef CHECKED
        gVariable = byte(byteCode[gCurrent]);
        gCurrent++;
        BasicType basicType = BasicType(variableTypes[gVariable]);
        if (basicType != BasicType.Integer)
        {
            Error(18, gCurrentLineNumber); // Type mismatch.
            return;
        }
        variableValues[gVariable] = variableValues[gVariable] + 1;
        Push(variableValues[gVariable], basicType);
        

        if (gTronState)
        {
            Trace(" " + variableNames[gVariable], Color.MatrixCyan);
        }
#else
        gVariable = byte(byteCode[gCurrent]);
        gCurrent++;
        if (variableTypes[gVariable] != byte(BasicType.Integer))
        {
            Error(18, gCurrentLineNumber); // Type mismatch.
            return;
        }
        
        int vValue = variableValues[gVariable];
        vValue++;
        variableValues[gVariable] = vValue;
        valueStack[sp] = vValue;
        typeStack [sp] = variableTypes [gVariable];

        sp++;
#endif                  
    }
	
   	OpCodePushVariable()
    {
        gVariable = byte(byteCode[gCurrent]);
        gCurrent++;
        byte basicType = variableTypes[gVariable];
        if (basicType == byte(BasicType.String))
        {
            sharedString = variableStringValues[gVariable];
            int iIndex = int(ToStringIndex(sharedString));
            uint iLength = sharedString.Length;
            Push(iIndex,  BasicType.String);
            Push(int(iLength), BasicType.String);
        }
        else
        {
#ifdef CHECKED
            Push(variableValues[gVariable], BasicType(basicType));
#else            
            valueStack[sp] = variableValues[gVariable];
            typeStack[sp] = basicType;
            sp++;
#endif            
        }
#ifdef CHECKED
        if (gTronState)
        {
            Trace(" " + variableNames[gVariable], Color.MatrixCyan);
        }
#endif  
    }
    
    OpCodeLE()
    {
#ifdef CHECKED        
        BasicType topType;
        int top = Pop(ref topType);
        BasicType nextType;
        int next = Pop(ref nextType);
        if ((topType != BasicType.Integer) || (nextType != BasicType.Integer))
        {
            Error(16, gCurrentLineNumber); // Integers expected for operation.
            return;
        }
        int result = 0;
        if (next <= top)
        {
            result = 1;
        }
        Push(result, BasicType.Boolean);
#else
        sp--;
        int top = valueStack[sp];
        if ((typeStack[sp] != byte(BasicType.Integer)) || (typeStack[sp-1] != byte(BasicType.Integer)))
        {
            Error(16, gCurrentLineNumber); // Integers expected for operation.
            return;
        }
        if (valueStack[sp-1] <= top)
        {
            valueStack[sp-1] = 1;
        }
        else
        {
            valueStack[sp-1] = 0;
        }
        typeStack[sp-1] = byte(BasicType.Boolean);
#endif        
    }
    
    OpCodePushImmediateInteger()
    {
#ifdef CHECKED                
        int integer = byteCode[gCurrent];
        gCurrent++;
        Push(integer, BasicType.Integer);

        if (gTronState)
        {
            Trace(" " + integer.ToString(), MatrixRed);
        }
#else
        valueStack[sp] = byteCode[gCurrent];
        gCurrent++;
        typeStack [sp] = byte(BasicType.Integer);
        sp++;
#endif     
    }
    
    bool RunLine(ref uint argJumpLine, ref uint argCurrentLineNumber)
    {
        goodEND = false;
#ifdef CHECKED
        gTronState = TraceOn;
#endif
        gJumpLine = argJumpLine;
        gCurrentLineNumber = argCurrentLineNumber;
        
        loop // line block
        {

#ifdef CHECKED
            if (gTronState)
            {
                TraceLn();
                string currentLineNumberString;
                UInt.ToString(gCurrentLineNumber, ref currentLineNumberString);
                currentLineNumberString = currentLineNumberString.LeftPad(' ', 4);
                Trace(currentLineNumberString, LightestGray);
                Trace(':', LightestGray);
            }
#endif            
            if (!byteCodeStart.Contains(gCurrentLineNumber))
            {
                // compile currentLine -> code
#ifdef CHECKED
                if (gTronState)
                {
                    Trace("JIT:", LightestGray);
                }
#endif                
                Compile();
                if (gWasError)
                {
                    break;
                }
            }
            else
            {
#ifdef CHECKED
                if (gTronState)
                {
                    Trace("RUN:", LightestGray);
                }
#endif          
            }  
            gCurrent = byteCodeStart[gCurrentLineNumber];
            gEnd = gCurrent + byteCodeLength[gCurrentLineNumber];
            sp = 0;
            
            loop // byteCode loop
            {
#ifdef CHECKED
                if ((gCurrent < 0) || (gCurrent >= byteCode.Count))
                {
                    PrintLn("gCurrent out of byteCode range " + gCurrent.ToString());
                    Error(36, gCurrentLineNumber);
                    return false;
                }
#endif          
                gOpCode = byte(byteCode[gCurrent]);
                
                
#ifdef CHECKED
                if (!opCodeDelegates.Contains(gOpCode))
                {
                    OpCode opCode = OpCode(gOpCode);
                    PrintLn("No entry in opCodeDelegates for " + gOpCode.ToString());
                    Error(36, gCurrentLineNumber);
                    return false;
                }
#endif                
                OpCodeDelegate opCodeMethod = opCodeDelegates[gOpCode];
                gCurrent++;
                
#ifdef CHECKED                
                if (gTronState)
                {
                    OpCode opCode = OpCode(gOpCode);
                    Trace(" " + OpCodeToString(opCode), MatrixBlue);
                    gTronCount++;
                    if (!gTronCounts.Contains(gOpCode))
                    {
                        gTronCounts[gOpCode] = 0;
                    }
                    gTronCounts[gOpCode] = gTronCounts[gOpCode]+1;
                }
#endif  
                opCodeMethod(); // call delegate
                if (gWasError)
                {
                    break;
                }
                
                bool jumpToNextLine = false;
                if (gCurrent == gEnd)
                {
                    // line done
                    if ((gJumpLine != 0) && byteCodeStart.Contains(gJumpLine) && (byteCodeLength[gJumpLine] > 0)
                       )
                    {
                        jumpToNextLine = true;
                    }
                    else
                    {
                        break; // exit code loop to scan for next line
                    }
                } 
                else if (gJumpLine != 0)
                {   
                    // !(line done): like a +ve IF .. THEN .. (pc < code.Length)
                    if (byteCodeStart.Contains(gJumpLine) && (byteCodeLength[gJumpLine] > 0))
                    {
                        jumpToNextLine = true;
                    }
                    else
                    {    
                        break; 
                    }
                }
                
                if (jumpToNextLine)
                {
                    // jump to byteCode of next line without exiting RunLine(..) back to Run(..)
                    if (!gWasError)
                    {
                        gWasPrint = false;
                        if (IsBreak()) // check for <ctrl><X>
                        {
                            WriteLn();
                            WriteLn("BREAK");
                            WasError = true;
                            break;
                        }
                    }
                    // tail link to next line without exiting this function
                    gCurrent  = byteCodeStart[gJumpLine];
                    gEnd      = gCurrent + byteCodeLength[gJumpLine];

                    // Was it a JMP to a line starting with a JMP?
                    if (byteCode[gCurrent] == byte(OpCode.JMP))
                    {
                        // skip it next time
                        PatchJump(gCurrentLineNumber, gJumpLine, byteCode[gCurrent+1]);
                    }
                    
                    gCurrentLineNumber = gJumpLine;
#ifdef CHECKED
                    if (gTronState)
                    {
                        TraceLn();
                        string currentLineNumberString = gCurrentLineNumber.ToString();
                        currentLineNumberString = currentLineNumberString.LeftPad(' ', 4);
                        Trace(currentLineNumberString + "    :", LightestGray);
                    }
#endif   
                    gJumpLine = 0;
                    
                } // jumpToNextLine
                
            } // byteCode loop
            
            if (!gWasError)
            {
                gWasPrint = false;
                if (IsBreak()) // check for <ctrl><X>
                {
                    WriteLn();
                    WriteLn("BREAK");
                    WasError = true;
                }
            }
            break;
            
        } // line block
        argCurrentLineNumber = gCurrentLineNumber;
        argJumpLine = gJumpLine;
        
        return goodEND; // did we stop because of END?
    }
    
    PatchJump(uint patchLineNumber, uint oldJumpLine, int newJumpLine)
    {
        uint iStart  = byteCodeStart[patchLineNumber];
        uint iLength   = byteCodeLength[patchLineNumber];
        uint iCurrent = iStart;
        loop
        {
            byte b = byte(byteCode[iCurrent]);
            iCurrent++;
            OpCode opCode = OpCode(b);
            switch (opCode)
            {
                case OpCode.GOSUB:
                case OpCode.JMP:
                case OpCode.JNZ:
                case OpCode.JZ:
                {
                    if (oldJumpLine == uint(byteCode[iCurrent]))
                    {
                        byteCode[iCurrent] = newJumpLine;
                    }
                    iCurrent++;
                }
                
                case OpCode.DIM:
                case OpCode.PushImmediateString:
                {
                    iCurrent++;
                    iCurrent++;
                }
                case OpCode.LET:
                case OpCode.PushVariable:
                case OpCode.SetElement:
                case OpCode.GetElement:
                case OpCode.IncrementVariable:
                case OpCode.IncrementPushVariable:
                case OpCode.PushImmediateInteger:
                case OpCode.PushImmediateBoolean:
                {
                    iCurrent++;
                }
                
            } // switch
            if (iCurrent == iStart + iLength)
            {
                break;
            }
        } // loop
    }
}
