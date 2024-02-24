unit Output
{
    uses "/Source/System/Screen"
    
    uses "/Source/Debugger/Source"
    uses "/Source/Debugger/6502/Pages"
    
    uses "/Source/Editor/Highlighter"
    uses "/Source/Editor/MessageBox"
    
    bool passThrough;
    
    const byte maxDataWidth = 40;
    const uint paddingWidth = 120;
    
    byte outputLeft;
    byte outputTop;
    byte outputWidth;
    byte outputHeight;
    byte consoleHeight;
    byte currentX;
    byte currentY;
    
    <uint,bool>    freeBlocks;
    <uint,bool>    accountedFor;
    
    <uint, string> codeClicks;
    <uint, string> dataClicks;
    uint codeClickFirstCurrent;
    
    //const uint arraySize     = 8192; // > Screen.Rows*Screen.Columns
    const uint arraySize     = 32768; // > 66 * 240?
    char[arraySize] outputCharacters;
    uint[arraySize] outputForeColors;
    uint[arraySize] outputBackColors;
    
    bool stack8;
    bool Is8BitSP { get { return stack8; } }
    bool stack32;
    bool Is32BitStackSlot { get { return stack32; } }
    bool isMCU;
    bool IsMCU { get { return isMCU; } }
    
    byte RemoteConsoleHeight
    {
        get 
        { 
            uint remoteConsoleHeight = Screen.Rows * 11;
            remoteConsoleHeight /= 20;
            return byte(remoteConsoleHeight); // 24 when 44
        } 
    }
    
    Initialize()
    {
        HopperFlags hopperFlags = HopperFlags(Pages.GetZeroPage("FLAGS"));
        isMCU   = (hopperFlags & HopperFlags.MCUPlatform) == HopperFlags.MCUPlatform;
        stack32 = false;
        if (isMCU)
        {
            stack32 = (hopperFlags & HopperFlags.StackSlot32Bit) == HopperFlags.StackSlot32Bit;
        }
        stack8 = ZeroPageContains("BP8");
    }
    
    SetPassThrough()
    {
        passThrough = true;
    }
    Locate(byte left, byte top, byte width, byte height)
    {
        outputLeft   = left+1;
        outputTop    = top;
        outputWidth  = width-1;
        outputHeight = height;
        consoleHeight = RemoteConsoleHeight;
        if (consoleHeight > height)
        {
            consoleHeight = height;
        }
        currentX     = outputLeft;
        currentY     = outputTop;
        Token.Initialize();
    }
    byte Left   { get { return outputLeft; } }
    byte Top    { get { return outputTop; } }
    byte Width  { get { return outputWidth; } }
    byte Height { get { return outputHeight; } }
    
    Clear()
    {
        if (passThrough)
        {
            Screen.Clear();
        }
        else
        {
            
            for (byte r = outputTop; r < outputTop + outputHeight; r++)
            {
                uint bcolour = Black;
                if (r >= outputTop + consoleHeight)
                {
                    bcolour = LightestGray;
                }
                for (byte c = outputLeft; c < outputLeft + outputWidth; c++)
                {
                    uint index  = r * Screen.Columns + c;
                    outputCharacters[index] = ' ';
                    outputForeColors[index]  = bcolour;
                    outputBackColors[index]  = bcolour;
                }
            }
            Draw();
            currentX = outputLeft;
            currentY = outputTop;
        }
        codeClicks.Clear();
        dataClicks.Clear();
    }
    
    Scroll()
    {
        for (byte r = outputTop+1; r < outputTop + consoleHeight; r++)
        {
            for (byte c = outputLeft; c < outputLeft + outputWidth; c++)
            {
                uint indexTo    = (r-1) * Screen.Columns + c;
                uint indexFrom  = r * Screen.Columns + c;
                outputCharacters[indexTo] = outputCharacters[indexFrom];
                outputForeColors[indexTo] = outputForeColors[indexFrom];
                outputBackColors[indexTo] = outputBackColors[indexFrom];
            }
        }
        for (byte c = outputLeft; c < outputLeft + outputWidth; c++)
        {
            uint index = (outputTop + consoleHeight-1) * Screen.Columns + c;
            outputCharacters[index] = ' ';
            outputForeColors[index] = Black;
            outputBackColors[index] = Black;
        }
        currentY--;
    }    
    
    Draw()
    {
        Screen.Suspend();
        for (byte r = outputTop; r < outputTop + outputHeight; r++)
        {
            DrawChar(outputLeft-1, r, ' ',  Colour.MarginFace, Colour.MarginFace);
            for (byte c = outputLeft; c < outputLeft + outputWidth; c++)
            {
                uint index  = r * Screen.Columns + c;
                DrawChar(c, r, outputCharacters[index],  outputForeColors[index], outputBackColors[index]);
            }
        }
        Screen.Resume(true);
    }
    Print(char c, uint foreColor, uint backColor)
    {
#ifdef DEBUGGER        
        if (DebugOptions.IsCaptureConsoleMode)
        {
            ConsoleCapture.AppendToLog(c);
        }
#endif
        if (passThrough)
        {
            if ((byte(c) == 0x0D) || (byte(c) == 0x0A))
            {
                Screen.PrintLn();
            }
            else
            {
                Screen.Print(c, foreColor, backColor);
            }
        }
        else
        {
            bool doDraw = false;
            if (currentX < outputLeft)
            {
                currentX = outputLeft;
            }
            if (currentX > outputLeft + outputWidth)
            {
                currentX = outputLeft;
                currentY++;
                if (currentY >= consoleHeight)
                {
                    Scroll();
                    doDraw = true;
                }
            }
            if (currentY < outputTop)
            {
                currentY = outputTop;
            }
            if ((byte(c) == 0x0D) || (byte(c) == 0x0A))
            {
                // enter
                currentY++;
                currentX = outputLeft;
                if (currentY >= consoleHeight)
                {
                    Scroll();
                    doDraw = true;
                }
            }
            else if (byte(c) == 0x08)
            {
                // backspace
                if (currentX > outputLeft)
                {
                    currentX--;
                    /*
                    uint index  = currentY * Screen.Columns + currentX;
                    outputCharacters[index] = ' ';
                    outputForeColors[index] = foreColor;
                    outputBackColors[index] = backColor;
                    doDraw = true;
                    */
                }
                else if (currentY > outputTop)
                {
                    currentX = outputLeft + outputWidth-1;
                    currentY--;
                    /*
                    uint index  = currentY * Screen.Columns + currentX;
                    outputCharacters[index] = ' ';
                    outputForeColors[index] = foreColor;
                    outputBackColors[index] = backColor;
                    doDraw = true;
                    */
                }
                else
                {
                    currentY = outputTop;
                    currentX = outputLeft;
                }
            }
            else
            {
                uint index  = currentY * Screen.Columns + currentX;
                outputCharacters[index] = c;
                outputForeColors[index] = foreColor;
                outputBackColors[index] = backColor;
                currentX++;
                doDraw = true;
            }
            if (doDraw)
            {
                Draw();
            }
        }
        
    }    
    Print(char c)
    {
        Output.Print(c, MatrixGreen, Black);
    }
    Print(string s, uint foreColor, uint backColor)
    {
        foreach (var c in s)
        {
            Output.Print(c, foreColor, backColor);
        }
    }
    Print(string s)
    {
        Output.Print(s, MatrixGreen, Black);
    }
    ClearWatchArea()
    {
        for (byte r = outputTop + consoleHeight; r < outputTop + outputHeight; r++)
        {
            for (byte c = outputLeft; c < outputLeft + outputWidth; c++)
            {
                uint index  = r * Screen.Columns + c;
                outputCharacters[index] = ' ';
                outputForeColors[index]  = Black;
                outputBackColors[index]  = LightestGray;
            }
        }
    }   
    PrintWatch(byte x, byte y, string content, uint foreColor, uint backColor)
    {
        byte row = y + outputTop + consoleHeight;
        if (row < outputTop + outputHeight)
        {
            content = content.Replace('`', ',');
            uint blockCommentNesting;
            <uint> colours = Highlighter.HopperSource(content, "", backColor, ref blockCommentNesting);
            
            for (byte i = 0; i < content.Length; i++)
            {
               byte col = x + outputLeft + i;
               if (col < outputLeft + outputWidth)
               {
                   uint index  = row * Screen.Columns + col;
                   outputCharacters[index] = content[i];
                   outputForeColors[index] = colours[i];
                   outputBackColors[index] = backColor;
               }
            }
        }
    }
    ShadeWatch(byte y, uint backColor)
    {
        byte row = y + outputTop + consoleHeight;
        if (row < outputTop + outputHeight)
        {
            for (byte col = outputLeft; col < outputLeft + outputWidth; col++)
            {
                uint index  = row * Screen.Columns + col;
                outputBackColors[index] = backColor;
            }
        }
    }
    
    
    string generateMethodString(uint methodIndex, uint bp, bool verbose)
    {
        <string,variant> methodSymbols = Code.GetMethodSymbols(methodIndex);
        string methodName = methodSymbols["name"];
        string content = methodName + "(";
        if (methodSymbols.Contains("arguments"))
        {
            < < string > > overloadArguments = Symbols.GetOverloadArguments(methodIndex);
            
            <string, <string> > argumentInfo = methodSymbols["arguments"];
            bool first = true;     
            uint iArgument;
            foreach (var kv in argumentInfo)
            {
                if (!first)
                {
                    content += "` ";
                }
                <string> argumentList = kv.value;
                <string> overloadArgument;
                string vtype = argumentList[1];
                if (overloadArguments.Count > iArgument)
                {
                    overloadArgument = overloadArguments[iArgument];
                    vtype = overloadArgument[1];
                }
                
                int delta;
                if (Int.TryParse(kv.key, ref delta))
                {
                }
                if (Is32BitStackSlot)
                {
                    delta = delta * 2;
                }
                uint voffset = uint(int(bp) +  delta);
                if (voffset >= 0x4000) // 16K
                {
                    OutputDebug("generateMethodString: " + methodIndex.ToHexString(4) + " " + bp.ToHexString(4) + " " + delta.ToString());
                    content += "t? n? = v?";
                    continue;
                }
                uint value0 = GetPageWord(voffset);
                uint value1 = Is32BitStackSlot ? GetPageWord(voffset+2) : 0;
                
                bool isReference = (argumentList[0] == "true");
                if (verbose)
                {
                    if (isReference)
                    {
                        content += "ref ";
                    }
                    content += vtype + " ";
                }
                content += argumentList[2] + "=";
                content += TypeToString(value0, value1, vtype, isReference, maxDataWidth);
                
                first = false;
                iArgument++;
            }
        }
        content += ")";
        return content;
    }   
    
    DebugDump(uint csp, uint currentMethodIndex)
    {
        if (csp > 0)
        {
            OutputDebug("");
            OutputDebug("Call Stack: csp=" + csp.ToHexString(4) + ", currentMethodIndex=" + currentMethodIndex.ToHexString(4));
            uint icsp = csp+2;
            loop
            {
                uint address = Pages.GetPageWord(0x0400+icsp);
                string prefix = "      ";
                if (icsp == csp)
                {
                    prefix = "CSP-> ";
                }
                string suffix;
                if (icsp % 4 == 0)
                {
                    suffix = " " + (address + 0x0600).ToHexString(4);
                }
                OutputDebug(prefix + icsp.ToHexString(4) + " " + address.ToHexString(4) + suffix);
                if (icsp == 0)
                {
                    break;
                }
                icsp = icsp - 2;
            }
        }
        
        if (false) // dump stack and types
        {
            uint sp  = Pages.GetZeroPage("SP") - 0x0600;
            uint bp  = Pages.GetZeroPage("BP") - 0x0600;
            if (sp > 0)
            {
                OutputDebug("Stack:");
                uint isp = sp - 2;
                loop
                {
                    uint value = Pages.GetPageWord(0x0600+isp);
                    byte htype = Pages.GetPageByte(0x0500+(isp/2));
                    
                    string reference;
                    if (IsMachineReferenceType(htype))
                    {
                        uint address = value;
                        uint rtype  = Pages.GetPageByte(address+0);    
                        uint rcount = Pages.GetPageByte(address+1);  
                        reference = rtype.ToHexString(2) + " " + rcount.ToHexString(2);
                    }
                    string prefix = "    ";
                    if (isp == bp)
                    {
                        prefix = "BP  ";
                    }
                    OutputDebug(prefix + (isp + 0x0600).ToHexString(4) + " " + value.ToHexString(4) + ":" + htype.ToHexString(2) + " " + reference);
                    if (isp == 0)
                    {
                        break;
                    }
                    isp = isp - 2;
                }
            }
        }
    }
    bool IsMachineReferenceType(uint typeValue)
    {
        if (Is32BitStackSlot)
        {
            return typeValue >= 0x0F;
        }
        return typeValue >= 0x0D;
    }
    bool IsMachineReferenceType(string typeName)
    {
        if (Is32BitStackSlot)
        {
            if ((typeName == "long") || (typeName == "float"))
            {
                return false;
            }
        }
        return !Types.IsValueType(typeName);
    }
    
    CallStack(uint csp)
    {
        ClearWatchArea();
        codeClicks.Clear();
        dataClicks.Clear();
        byte yCurrent = 0;
        
        uint pc = Pages.GetZeroPage("PC") - (Pages.GetZeroPage("CODESTART") << 8);
        uint currentMethodIndex = Code.LocationToIndex(pc);
        
        <string,variant> methodSymbols = Code.GetMethodSymbols(currentMethodIndex);
        string currentMethodName = methodSymbols["name"];
        string namespacePlusDot;
        uint iDot;
        if (currentMethodName.IndexOf('.', ref iDot))
        {
            namespacePlusDot = currentMethodName.Substring(0, iDot+1);
        }
        
        //DebugDump(csp, currentMethodIndex);
        
        uint bp;
        uint tsp;
        uint address;
        uint methodIndex;
        string sourceIndex;
        string methodString;
        
        bool trimming = false;
        
        // callstack methods
        bool lastIsRetFast = ((csp - 2) % 4 != 0);
        uint icsp = 2;
        while (icsp < csp)
        {
            // icsp = [2,   6, 10, 14, .. <max>]  where <max> = csp-4
            //         02, 06, 0A, 0E, 12, 16, ~18
            // nth frame up the stack would be would be icsp = csp - 4*n
            if ((csp <= 14) || (icsp > csp - (4 * 4))) 
            {
                address = Pages.GetPageWord(0x0400+icsp);
                bp      = Pages.GetPageWord(0x0400+icsp+2);
                if (Is8BitSP || IsMCU)
                {
                    if (Is32BitStackSlot)
                    {
                        bp = bp * 2;
                    }
                    bp  = 0x600 + bp;
                }
                
                if (lastIsRetFast && (icsp + 4 >= csp))
                {
                    // if last method is RETFAST then 2nd last method can use current BP
                    if (Is8BitSP)
                    {
                        bp  = 0x600 + Pages.GetZeroPage("BP8");
                    }
                    else
                    {
                        bp  = Pages.GetZeroPage("BP");
                    }
                }
                
                address = address - (Pages.GetZeroPage("CODESTART") << 8);
                methodIndex = Code.LocationToIndex(address);
                sourceIndex = Code.GetSourceIndex(Source.CallAddressFromReturnToAddress(methodIndex, address), methodIndex); 
                codeClicks[yCurrent] = sourceIndex;
                methodString = generateMethodString(methodIndex, bp, false);
                if (trimming || (methodString.Length > outputWidth))
                {
                    methodString = methodString.Replace(namespacePlusDot, "");
                    trimming = true;
                }
                PrintWatch(0, yCurrent, methodString.Pad(' ', outputWidth), Black, Colour.ListGray);
                //OutputDebug(bp.ToHexString(4) + " " + icsp.ToHexString(4) + " " + methodString);
                yCurrent++;
            }
            icsp = icsp + 4;
        }
        
        // current method
        if (Is8BitSP)
        {
            bp  = 0x600 + Pages.GetZeroPage("BP8");
        }
        else
        {
            bp  = Pages.GetZeroPage("BP");
        }
        methodString = generateMethodString(currentMethodIndex, bp, true);
        sourceIndex = Code.GetSourceIndex(pc, currentMethodIndex);
        if (trimming || (methodString.Length > outputWidth))
        {
            methodString = methodString.Replace(namespacePlusDot, "");
            trimming = true;
        }
        codeClickFirstCurrent = yCurrent;
        
        //OutputDebug(bp.ToHexString(4) + " " + icsp.ToHexString(4) + " " + methodString);
        
        if ((methodString.Length > outputWidth) && methodString.Contains('`'))
        {
            bool fa = true;
            uint iParen;
            string sPad;
            <string> parts = methodString.Split('`');
            for (uint i = 0; i < parts.Count; i++)
            {
                string content = parts[i];
                if (i == 0)
                {
                    if (content.IndexOf('(', ref iParen))
                    {
                        if (iParen > 1)
                        {
                            iParen--;
                        }
                        if (iParen > 1)
                        {
                            iParen--;
                        }
                        sPad = sPad.Pad(' ', iParen);
                    }
                }
                if (i != 0) // not first
                {
                    content = sPad +"` " + content;
                }
                codeClicks[yCurrent] = sourceIndex;
                PrintWatch(0, yCurrent, content.Pad(' ', outputWidth), Colour.Black, Colour.ActiveListGray);
                yCurrent++;       
            }
        }
        else
        {
            codeClicks[yCurrent] = sourceIndex;
            PrintWatch(0, yCurrent, methodString.Pad(' ', outputWidth), Colour.Black, Colour.ActiveListGray);
            yCurrent++;
        }
        
        // locals currently in scope
        uint startAddress;
        <string, <string> > localCandidates = Source.GetLocals(currentMethodIndex, ref startAddress);
        <string> winningRanges;
        <string> localsUsed;
        foreach (var kv in localCandidates)
        {
            string range = kv.key;
            <string> parts = range.Split('-');
            uint fromRange;
            uint toRange;
            if (UInt.TryParse(parts[0], ref fromRange) && UInt.TryParse(parts[1], ref toRange))
            {
                fromRange = fromRange + startAddress;
                toRange   = toRange + startAddress;
                if ((pc > fromRange) && (pc <= toRange))
                {
                    <string> localList = kv.value;
                    string lname = localList[0];
                    if (!localsUsed.Contains(lname))
                    {
                        winningRanges.Append(range);
                        localsUsed.Append(lname);
                    }
                }
            }
        }
        for (uint iLocal = 0; iLocal < winningRanges.Count; iLocal++)
        {
            string range = winningRanges[iLocal];
            <string> localList = localCandidates[range];
            string lname = localList[0];
            string ltype = localList[1];
            string loffset = localList[2];
            
            uint offset;
            if (!UInt.TryParse(loffset, ref offset))
            {
                Die(0x0B);
            }
            if (Is32BitStackSlot)
            {
                offset = offset * 2;
            }
            uint lvalue0;
            uint lvalue1;
            uint cPtr;
            string cName = lname;
            string lcontent;
            string vType;
            string kType;
            bool wasKV = false;
            if (lname.EndsWith("_c") && (ltype == "string"))
            {
                // foreach (var ch in str) case
                string irange = winningRanges[iLocal+1];
                <string> ilocalList = localCandidates[irange];
                iLocal++;
                string ioffsets = ilocalList[2];
                uint ioffset;
                if (!UInt.TryParse(ioffsets, ref ioffset))
                {
                    Die(0x0B);
                }
                if (Is32BitStackSlot)
                {
                    ioffset = ioffset * 2;
                }
                // index in string
                uint ivalue = Pages.GetPageWord(bp + ioffset);
                
                lname = lname.Substring(0, lname.Length-2);
                ltype = "char";
                uint sPtr = Pages.GetPageWord(bp + offset);
                char ch = Source.StringGetChar(sPtr, ivalue);
                lcontent = "'" + ch + "'";
            }
            else if (lname.EndsWith("_c") && Source.IsListType(ltype, ref vType) && (vType != ""))
            {                // foreach (var item in <str>) case
                lvalue0 = Pages.GetPageWord(bp + offset);
                string irange = winningRanges[iLocal+1];
                <string> ilocalList = localCandidates[irange];
                iLocal++;
                string ioffsets = ilocalList[2];
                uint ioffset;
                if (!UInt.TryParse(ioffsets, ref ioffset))
                {
                    Die(0x0B);
                }
                if (Is32BitStackSlot)
                {
                    ioffset = ioffset * 2;
                }
                // index in list
                uint ivalue = Pages.GetPageWord(bp + ioffset);
                lname = lname.Substring(0, lname.Length-2);
                ltype = vType;
                
                uint lPtr = Pages.GetPageWord(bp + offset);   
                uint item1;
                uint item0 = Source.ListGetItem(lPtr, ivalue, ref item1);
                lcontent = TypeToString(item0, item1, ltype, false, maxDataWidth);
                cPtr = item0;
                cName = lname;
            }
            else if (lname.EndsWith("_c") && Source.IsArrayType(ltype, ref vType) && (vType != ""))
            {
                // foreach (var item in [bool]) case
                lvalue0 = Pages.GetPageWord(bp + offset);
                string irange = winningRanges[iLocal+1];
                <string> ilocalList = localCandidates[irange];
                iLocal++;
                string ioffsets = ilocalList[2];
                uint ioffset;
                if (!UInt.TryParse(ioffsets, ref ioffset))
                {
                    Die(0x0B);
                }
                if (Is32BitStackSlot)
                {
                    ioffset = ioffset * 2;
                }
                // index in array
                uint ivalue = Pages.GetPageWord(bp + ioffset);
                lname = lname.Substring(0, lname.Length-2);
                ltype = vType;
                
                uint aPtr = Pages.GetPageWord(bp + offset);   
                uint item = Source.ArrayGetItem(aPtr, ivalue);
                lcontent = TypeToString(item, 0, ltype, false, maxDataWidth);
                cPtr = item;
                cName = lname;
            }
            else if (   lname.EndsWith("_c") && Source.IsDictionaryType(ltype, ref kType, ref vType) 
                     && (kType != "") && (vType != ""))
            {
                // foreach (var item in <char,uint>) case
                lvalue0 = Pages.GetPageWord(bp + offset);
                string irange = winningRanges[iLocal+1];
                <string> ilocalList = localCandidates[irange];
                iLocal++;
                string ioffsets = ilocalList[2];
                uint ioffset;
                if (!UInt.TryParse(ioffsets, ref ioffset))
                {
                    Die(0x0B);
                }
                if (Is32BitStackSlot)
                {
                    ioffset = ioffset * 2;
                }
                
                uint ivalue = Pages.GetPageWord(bp + ioffset);
                string kname = lname.Substring(0, lname.Length-2) + ".key";
                string vname = lname.Substring(0, lname.Length-2) + ".value";
                
                uint dPtr = Pages.GetPageWord(bp + offset);   
                uint iterator = 0;
                uint kvalue;
                uint vvalue0;
                uint vvalue1;
                loop
                {
                    if (Source.DictionaryNextItem(dPtr, ref iterator, ref kvalue, ref vvalue0, ref vvalue1))
                    {
                    }
                    if (ivalue == iterator)
                    {
                        break;
                    }
                }
                lcontent = TypeToString(kvalue, 0, kType, false, maxDataWidth);
                PrintWatch(0, yCurrent, "  " + kType + " " + kname + "=" + lcontent, Black, LightestGray);
                yCurrent++;
                
                if (IsMachineReferenceType(kType))
                {
                    dataClicks[yCurrent-1] = kType + "`" + kname + "`" + kvalue.ToString();
                }
                
                lcontent = TypeToString(vvalue0, vvalue1, vType, false, maxDataWidth);
                PrintWatch(0, yCurrent, "  " + vType + " " + vname + "=" + lcontent, Black, LightestGray);
                ltype  = vType;
                cPtr = vvalue0;
                cName = vname;
                yCurrent++;
                wasKV = true;
            }
            else
            {
                lvalue0 = GetPageWord(bp + offset);
                lvalue1 = Is32BitStackSlot ? GetPageWord(bp + offset + 2) : 0;
                lcontent = TypeToString(lvalue0, lvalue1, ltype, false, maxDataWidth);
                cPtr = lvalue0;
            }
            lcontent = lcontent.Replace(namespacePlusDot, "");
            string dcontent = ltype + " " + lname + "=" + lcontent;
            if (ltype.IndexOf('.', ref iDot))
            {
                string namedTypePlusDot = ltype.Substring(0, iDot+1); // qualified enum or flags name
                dcontent = dcontent.Replace(namedTypePlusDot, "");   
            }
            if (!wasKV)
            {
                PrintWatch(0, yCurrent, "  " + dcontent, Black, LightestGray);
                yCurrent++;
            }
            if (IsMachineReferenceType(ltype))
            {
                dataClicks[yCurrent-1] = ltype + "`" + cName + "`" + cPtr.ToString();
            }
            
        }
        // globals used in this method
        <uint, <string> > usedGlobals = Source.GetGlobals(currentMethodIndex, pc);
        foreach (var kv in usedGlobals)
        {
            uint goffset = kv.key;
            <string> globalList = kv.value;
            if (globalList.Count == 0)
            {
                continue;
            }
            if (Is32BitStackSlot)
            {
                goffset = goffset * 2;
            }
            uint gaddress = goffset + 0x0600;
            uint gvalue0 = GetPageWord(gaddress);
            uint gvalue1 = GetPageWord(gaddress+2);
            
            string gtype = globalList[0];
            
            // is global a qualified named type like enum, flags or delegate?
            string gdefinition= globalList[2];
            if (gdefinition.Contains('.')) 
            {
                gtype = gdefinition;
            }
            
            string identifier = globalList[1];
            string gcontent = TypeToString(gvalue0, gvalue1, gtype, false, maxDataWidth);
            
            gcontent = gtype + " " + identifier + "=" + gcontent;
            
            // namespacePlusDot is the current namespace (not that of the global)
            // In this case, "HopperCode." is successfully removed but "Instructions." is not
            //    gcontent="Instructions.Instruction HopperCode.gLastInstruction0=Instructions.Instruction.ADDI"
            if (identifier.StartsWith(namespacePlusDot) && gtype.StartsWith(namespacePlusDot))
            {
                gcontent = gcontent.Replace(namespacePlusDot, "");
            }
            if (trimming || (gcontent.Length > outputWidth))
            {
                gcontent = gcontent.Replace(namespacePlusDot, "");
                trimming = true;
            }
            if (gtype.IndexOf('.', ref iDot))
            {
                string namedTypePlusDot = gtype.Substring(0, iDot+1); // qualified enum or flags name
                gcontent = gcontent.Replace(namedTypePlusDot, "");   
            }
            
            PrintWatch(0, yCurrent, "  " + gcontent, Black, LightestGray);
            yCurrent++;
            if (IsMachineReferenceType(gtype))
            {
                dataClicks[yCurrent-1] = gtype + "`" + identifier + "`" + gvalue0.ToString();
            }
        }
        Draw();
    }
    bool ConsoleHitTest(uint x, uint y)
    {
        if (   (x >= outputLeft) 
            && (x <= outputLeft+outputWidth)
            && (y >= outputTop) 
            && (y <  outputTop+RemoteConsoleHeight)
           )
        {
            return true;
        }
        return false;
    }
    
    GotoSourceIndex(string sourceIndex, bool setActiveLine)
    {
        if (sourceIndex.Length != 0)
        {
            <string> parts = sourceIndex.Split(':');
            string hsPath = parts[0];
            uint gotoLine;
            if (UInt.TryParse(parts[1], ref gotoLine))
            {
                if (Editor.GetCurrentPath() != hsPath)
                {
                    Editor.LoadFile(hsPath);
                }
                Editor.SetActiveLine(gotoLine, hsPath, setActiveLine);
            }
        }
    }
    ShowData(string dataClick)
    {
        <string> parts = dataClick.Split('`');
        string ctype = parts[0];
        string cname = parts[1];
        uint cvalue;
        if (UInt.TryParse(parts[2], ref cvalue))
        {
            string gcontent = TypeToString(cvalue, 0, ctype, false, 2048);
            <string> buttons;
            buttons.Append("OK");
            <string, variant> mb = MessageBox.New(ctype + " " + cname, ".hs:" + gcontent, buttons);
            DisplayCursor(false);
            
            string result = MessageBox.Execute(mb);
            Editor.Draw();
            DisplayCursor(true);
        }
    }
    
    bool OnKey(Key key)
    {
        bool consumed = false;
        if ((key == Key.Click) && Keyboard.ClickUp)
        {
            uint xClick = Keyboard.ClickX;
            uint yClick = Keyboard.ClickY;
            if (   (xClick >= outputLeft) 
                && (xClick <= outputLeft+outputWidth)
                && (yClick >= outputTop+RemoteConsoleHeight) 
                && (yClick <  outputTop+outputHeight)
               )
            {
                uint index = yClick - outputTop - consoleHeight;
                if (codeClicks.Contains(index))
                {
                    foreach (var kv in codeClicks)
                    {
                        uint y = kv.key;
                        if (y == index)
                        {
                            ShadeWatch(byte(y), Colour.ActiveListGray);
                        }
                        else if ((index >= codeClickFirstCurrent) && (y >= codeClickFirstCurrent))
                        {
                            ShadeWatch(byte(y), Colour.ActiveListGray);
                        }
                        else
                        {
                            ShadeWatch(byte(y), Colour.ListGray);
                        }    
                    }
                    Draw();
                    GotoSourceIndex(codeClicks[index], false);
                }
                if (dataClicks.Contains(index))
                {
                    ShowData(dataClicks[index]);
                }
                consumed = true;
            }
        }
        return consumed;
    }
    
    bool WalkStack(<uint> stackValues0, <uint> stackValues1, <byte> stackTypes, ref uint sp, ref uint bp)
    {
        bool success = true;
        stackValues0.Clear();
        stackValues1.Clear();
        stackTypes.Clear();
        loop
        {
            uint tspInc = 2;
            if (Is8BitSP)
            {
                sp  = Pages.GetZeroPage("SP8");
                bp  = Pages.GetZeroPage("BP8");
            }
            else
            {
                sp  = Pages.GetZeroPage("SP") - 0x0600;
                bp  = Pages.GetZeroPage("BP") - 0x0600;
                if (Is32BitStackSlot)
                {
                    sp = sp / 2;
                    bp = bp / 2;
                }
                tspInc = 1;
            }
            if (sp % 2 != 0)
            {
                break;
            }
            if (sp == 0)
            {
                success = true; // empty stack
                break;
            }
            uint sCurrent = 0x0600;
            uint tCurrent = 0x0500;
            uint count = sp / 2;
            loop
            {
                if (Is32BitStackSlot)
                {
                    uint v = Pages.GetPageWord(sCurrent);
                    sCurrent = sCurrent + 2;
                    stackValues0.Append(v);
                    v = Pages.GetPageWord(sCurrent);
                    sCurrent = sCurrent + 2;
                    stackValues1.Append(v);
                }
                else
                {
                    uint v = Pages.GetPageWord(sCurrent);
                    stackValues0.Append(v);
                    stackValues1.Append(0);
                    sCurrent = sCurrent + 2;
                }
                
                byte t = Pages.GetPageByte(tCurrent);
                stackTypes.Append(t);
                tCurrent = tCurrent + tspInc;
                count--;
                if (count == 0)
                {
                    success = true;
                    break;
                }
            }
            break;
        }
        return success;
    }
    SafePad(ref string content, uint tillWidth)
    {
        uint length = content.Length;
        if (tillWidth > length)
        {
            content = content.Pad(' ', tillWidth);
        }
    }
    
    
    string WalkHeader(uint address)
    {
        string content;
        uint heapSize = Pages.GetPageWord(address - 2);
        content += "0x" + heapSize.ToHexString(4) + " ";   
        byte tp = Pages.GetPageByte(address + 0);
        content += "0x" + tp.ToHexString(2) + " ";   
        byte referenceCount = Pages.GetPageByte(address + 1);
        content += "0x" + referenceCount.ToHexString(2) + " ";   
        return content;
    }
    byte[8] setSlots;
    byte[8] clearSlots;
    uint WalkMemoryArray(file memoryFile, uint address, uint tp, uint indent)
    {
        accountedFor[address - 2] = true;
        string content;
        uint size = Pages.GetPageWord(address - 2);
        content = content.Pad(' ', indent);
        content += WalkHeader(address);
        uint count = Pages.GetPageWord(address + 2);
        content += "0x" + count.ToHexString(4) + " ";
        byte memberType = Pages.GetPageByte(address + 4);
        content += "0x" + memberType.ToHexString(2) + " ";
        uint memberBits = 16;
        uint columns = 8;
        if ((memberType == 1) || (memberType == 3))
        {
            memberBits = 8;
            columns = 16;
        }
        else if (memberType == 6)
        {
            memberBits = 1;
            columns = 32;
        }
        SafePad(ref content, paddingWidth);
        content += " (" + size.ToString() + " bytes)";         
        memoryFile.Append(content + char(0x0A));
        DebugFlush(memoryFile);
        indent = indent + 2;
        content = "";
        content = content.Pad(' ', indent);
        uint index = 0;
        uint laps = 0;
        loop
        {
            if (index >= count)
            {
                break;
            }
            switch (memberBits)
            {
                case 1:
                {
                    byte data = Pages.GetPageByte(address + 5 + (index >> 3));
                    byte slotIndex = byte(index & 0x07);
                    byte mask = setSlots[slotIndex];
                    data = data & mask;
                    if (data != 0)
                    {
                        content += "1 ";
                    }
                    else
                    {
                        content += "0 ";
                    }
                }
                case 8:
                {
                    byte data = Pages.GetPageByte(address + 5 + index);           
                    content += "0x" + data.ToHexString(2) + " ";
                }
                case 16:
                {
                    uint data = Pages.GetPageWord(address + 5 + index*2);           
                    content += "0x" + data.ToHexString(4) + " ";
                }
            }
            laps++;
            if (laps == columns)
            {
                content += " ";
            }
            else if (laps == columns*2)
            {
                memoryFile.Append(content + char(0x0A));
                DebugFlush(memoryFile);
                content = "";
                content = content.Pad(' ', indent);
                laps = 0;
            }
            
            index++;
        } // loop
        if (content.Length != 0)
        {
            SafePad(ref content, paddingWidth);
            memoryFile.Append(content + char(0x0A));
            DebugFlush(memoryFile);
        }
        return size;   
    }
    uint WalkMemoryDictionary(file memoryFile, uint address, uint tp, uint indent)
    {
        accountedFor[address - 2] = true;
        string content;
        uint size = Pages.GetPageWord(address - 2);
        content = content.Pad(' ', indent);
        content += WalkHeader(address);
        uint count = Pages.GetPageWord(address + 4);
        content += "0x" + count.ToHexString(4) + " ";
        uint capacity = Pages.GetPageWord(address + 6);
        content += "0x" + capacity.ToHexString(4) + " ";
        uint pEntries = Pages.GetPageWord(address + 8);
        content += "0x" + pEntries.ToHexString(4) + " ";
        byte keyType = Pages.GetPageByte(address + 2);
        content += "0x" + keyType.ToHexString(2) + " ";
        byte valueType = Pages.GetPageByte(address + 3);
        content += "0x" + valueType.ToHexString(2) + " ";
        
        if (pEntries != 0)
        {
            accountedFor[pEntries - 2] = true;
            size = size + Pages.GetPageWord(pEntries - 2);
        }
        SafePad(ref content, paddingWidth);
        content += " (" + size.ToString() + " bytes)";         
        memoryFile.Append(content + char(0x0A));
        DebugFlush(memoryFile);
        if (pEntries != 0)
        {
            content = "";
            content = content.Pad(' ', indent+2);
            content +=  WalkHeader(pEntries);
            memoryFile.Append(content + char(0x0A));
            DebugFlush(memoryFile);
        }
        
        bool first = true;
        uint iterator = 0;
        uint kValue;
        uint vValue0;
        uint vValue1;
        uint pVariant;
        indent = indent + 2;
        while (Source.DictionaryNextItem(address, ref iterator, ref kValue, ref vValue0, ref vValue1))
        {
            uint itemSize = 0;
            content = "";
            content = content.Pad(' ', indent);
            string keyText   = "K:0x" +  kValue.ToHexString(4);
            string valueText = "V:0x";
            if (Is32BitStackSlot && ((valueType == 0x0D) || (valueType == 0x0E)))
            {
                valueText = valueText +  vValue1.ToHexString(4);
            }
            valueText = valueText + vValue0.ToHexString(4);
            content += keyText;
        
            if (!IsMachineReferenceType(valueType))    
            {
                content += " -> " + valueText + " " + ValueTypeToString(valueType, vValue0, vValue1);
            }
         
            memoryFile.Append(content + char(0x0A));
            DebugFlush(memoryFile);
            if (IsMachineReferenceType(keyType) && (kValue != 0))
            {
                byte kt = Pages.GetPageByte(kValue);
                itemSize = itemSize + WalkMemory(memoryFile, kValue, kt, indent + 2);
            }
            if (IsMachineReferenceType(valueType))
            {
                content = "";
                content = content.Pad(' ', indent);
                content += valueText;
                memoryFile.Append(content + char(0x0A));
                DebugFlush(memoryFile);
                if (vValue0 != 0) // reference types
                {
                    byte vt = Pages.GetPageByte(vValue0);
                    itemSize = itemSize + WalkMemory(memoryFile, vValue0, vt, indent + 2);
                }
            }
            size = size + itemSize;
        } // next item
        content = "";
        SafePad(ref content, paddingWidth);
        content += " (" + size.ToString() + " bytes)";         
        memoryFile.Append(content + char(0x0A));
        DebugFlush(memoryFile);
        return size;   
    }
    uint WalkMemoryListItem(file memoryFile, uint address, uint listItemTypes, uint indent)
    {
        // ListItem memory map:
        //   0000 heap allocator size
        //   xxxx inline for value types, pData for reference types and when item type is variant
        //   xxxx 2nd half of 'long' and 'float' if item type is not variant
        //   0000 pNext
        
        uint size;
        loop
        {
            accountedFor[address-2] = true;
            string content;
            uint itemSize = Pages.GetPageWord(address - 2);
            size = size + itemSize;
            content = content.Pad(' ', indent);
            
            uint pData0  = Pages.GetPageWord(address + 0);
            uint pData1  = Is32BitStackSlot ? Pages.GetPageWord(address + 2) : 0;
            uint pNext   = Is32BitStackSlot ? Pages.GetPageWord(address + 4) : Pages.GetPageWord(address + 2);
            content += "0x" + address.ToHexString(4) + " ";
            content += "0x";
            if (Is32BitStackSlot && ((listItemTypes == 0x0D) || (listItemTypes == 0x0E)))
            {
                content += pData1.ToHexString(4);
            }
            content += pData0.ToHexString(4) + " ";
            content += "0x" + pNext.ToHexString(4) + " ";
            if (!IsMachineReferenceType(listItemTypes))
            {
                content += ValueTypeToString(byte(listItemTypes), pData0, pData1) + " ";
            }
            
            SafePad(ref content, paddingWidth);
            content += " (" + itemSize.ToString() + " bytes)";         
            memoryFile.Append(content + char(0x0A));
            DebugFlush(memoryFile);
            if (IsMachineReferenceType(listItemTypes) && (pData0 != 0)) // reference types
            {
                byte tp2 = Pages.GetPageByte(pData0);
                size = size + WalkMemory(memoryFile, pData0, tp2, indent + 2);
            }
            if (pNext != 0)
            {
                address = pNext; // tail call
                continue;
            }
            break;
        }
        return size;         
    }
    uint WalkMemoryList(file memoryFile, uint address, uint tp, uint indent)
    {
        // List memory map:
        //   0000 heap allocator size
        //   19   type = tList
        //   00   GC reference count
        //   0000 current number of items
        //   xx   type of items
        //   xxxx pFirst
        //   xxxx pRecent
        //   xxxx iRecent
        
        accountedFor[address-2] = true;        
        string content;
        uint size = Pages.GetPageWord(address - 2);
        content = content.Pad(' ', indent);
        content += WalkHeader(address);
        uint length = Pages.GetPageWord(address + 2);
        content += "0x" + length.ToHexString(4) + " ";
        byte listItemTypes = Pages.GetPageByte(address + 4);
        content += "0x" + listItemTypes.ToHexString(2) + " ";
        
        uint pFirst  = Pages.GetPageWord(address + 5);
        uint pRecent = Pages.GetPageWord(address + 7);
        uint iRecent = Pages.GetPageWord(address + 9);
        content += "0x" + pFirst.ToHexString(4) + " ";
        content += "0x" + pRecent.ToHexString(4) + " ";
        content += "0x" + iRecent.ToHexString(4) + " ";
        SafePad(ref content, paddingWidth);
        content += " (" + size.ToString() + " bytes)";         
        memoryFile.Append(content + char(0x0A));
        DebugFlush(memoryFile);
        if (pFirst != 0)
        {
            size = size + WalkMemoryListItem(memoryFile, pFirst, listItemTypes, indent + 2);
        }
        content = "";
        SafePad(ref content, paddingWidth);
        content += " (" + size.ToString() + " bytes)";         
        memoryFile.Append(content + char(0x0A));
        DebugFlush(memoryFile);
        return size;   
    }
    uint WalkMemoryDirectory(file memoryFile, uint address, uint tp, uint indent)
    {
        // Directory memory map:
        //   0000 heap allocator size
        //   0F   type = tFile
        //   00   GC reference count
        //   00   bool:   isValid
        //   0000 string: path
        accountedFor[address-2] = true;        
        string content;
        uint size = Pages.GetPageWord(address - 2);
        content = content.Pad(' ', indent);
        content += WalkHeader(address);
        uint isValid = Pages.GetPageByte(address + 2);
        content += ((isValid != 0) ? "Valid" : "Invalid") + " ";
        uint path = Pages.GetPageWord(address + 3);
        SafePad(ref content, paddingWidth);
        content += " (" + size.ToString() + " bytes)";         
        memoryFile.Append(content + char(0x0A));
        DebugFlush(memoryFile);
        if (isValid != 0)
        {
            size = size + WalkMemoryString(memoryFile, path, 0x0F, indent + 2);
        }
        content = "";
        SafePad(ref content, paddingWidth);
        content += " (" + size.ToString() + " bytes)";         
        memoryFile.Append(content + char(0x0A));
        DebugFlush(memoryFile);
        return size;   
    }
    uint WalkMemoryFile(file memoryFile, uint address, uint tp, uint indent)
    {
        // File memory map:
        //   0F   type = tFile
        //   00   GC reference count
        //   00   bool:   isValid
        //   00   bool:   isReading: 32 bit pos is position of next byte to read (Read and ReadLine)
        //   00   bool:   isWriting
        //   00   bool:   isCode:    16 bit pos is length and buffer is start in codeSegment
        //   0000     string: path
        //   00000000 uint32: pos
        //   0000     uint:   string (buffer)
        //   00000000 uint32: size: 32 bit file size in bytes used to read (Read and ReadLine)
        
        accountedFor[address-2] = true;        
        string content;
        uint size = Pages.GetPageWord(address - 2);
        content = content.Pad(' ', indent);
        content += WalkHeader(address); // heapsize type referencecount (0x000E 0x15 0x01)
        uint isValid = Pages.GetPageByte(address + 2);
        content += ((isValid != 0) ? "Valid" : "Invalid") + " ";
        uint isReading = Pages.GetPageByte(address + 3);
        content += ((isReading != 0) ? "Reading" : "") + " ";
        uint isWriting = Pages.GetPageByte(address + 4);
        content += ((isWriting != 0) ? "Writing" : "") + " ";
         uint isCode = Pages.GetPageByte(address + 5);
        content += ((isCode != 0) ? "Code" : "") + " ";

        uint path = Pages.GetPageWord(address + 6);
        uint pos0 = Pages.GetPageWord(address + 8);
        uint pos1 = Pages.GetPageWord(address + 10);
        uint sz0   = Pages.GetPageWord(address + 14);
        uint sz1   = Pages.GetPageWord(address + 16);
        content += "0x" + pos1.ToHexString(4) + pos0.ToHexString(4) + " 0x" + sz1.ToHexString(4) + sz0.ToHexString(4) + " ";
        uint buffer = Pages.GetPageWord(address + 12);
        
        SafePad(ref content, paddingWidth);
        content += " (" + size.ToString() + " bytes)";         
        memoryFile.Append(content + char(0x0A));
        DebugFlush(memoryFile);
        if (isValid != 0)
        {
            size = size + WalkMemoryString(memoryFile, path, 0x0F, indent + 2);
            size = size + WalkMemoryString(memoryFile, buffer, 0x0F, indent + 2);
        }
        content = "";
        SafePad(ref content, paddingWidth);
        content += " (" + size.ToString() + " bytes)";         
        memoryFile.Append(content + char(0x0A));
        DebugFlush(memoryFile);
        return size;   
    }
    uint WalkMemoryString(file memoryFile, uint address, uint tp, uint indent)
    {
        accountedFor[address-2] = true;
        string content;
        uint size = Pages.GetPageWord(address - 2);
        content = content.Pad(' ', indent);
        uint length = Pages.GetPageWord(address + 2);
        content += "0x" + address.ToHexString(4) + " ";
        content += WalkHeader(address);
        content += "0x" + length.ToHexString(4) + " " + '"';
        uint characters = address + 4;
        for (uint i = 0; i < length; i++)
        {
            byte b = Pages.GetPageByte(characters + i);
            if (b > 31)
            {
                content += char(b);
            }
            else
            {
                content += ' ';
            }
        }
        content += '"';
        SafePad(ref content, paddingWidth);
        content += " (" + size.ToString() + " bytes)";
        memoryFile.Append(content + char(0x0A));
        DebugFlush(memoryFile);
        return size;
    }
    uint WalkMemoryLong(file memoryFile, uint address, uint tp, uint indent)
    {
        accountedFor[address-2] = true;
        string content;
        uint size = Pages.GetPageWord(address - 2);
        content = content.Pad(' ', indent);
        content += WalkHeader(address);
        byte b0 = Pages.GetPageByte(address + 2 + 0);
        byte b1 = Pages.GetPageByte(address + 2 + 1);
        byte b2 = Pages.GetPageByte(address + 2 + 2);
        byte b3 = Pages.GetPageByte(address + 2 + 3);
        content += b3.ToHexString(2);
        content += b2.ToHexString(2);
        content += b1.ToHexString(2);
        content += b0.ToHexString(2);
        content += ' ';
        long l = Long.FromBytes(b0, b1, b2, b3);
        content += l.ToString();
        SafePad(ref content, paddingWidth);
        content += " (" + size.ToString() + " bytes)";
        memoryFile.Append(content + char(0x0A));
        DebugFlush(memoryFile);
        return size;
    }
    uint WalkMemoryFloat(file memoryFile, uint address, uint tp, uint indent)
    {
        string content;
        content = content.Pad(' ', indent);
        
        accountedFor[address-2] = true;
        uint size = Pages.GetPageWord(address - 2);
        content += WalkHeader(address);
        byte b0 = Pages.GetPageByte(address + 2 + 0);
        byte b1 = Pages.GetPageByte(address + 2 + 1);
        byte b2 = Pages.GetPageByte(address + 2 + 2);
        byte b3 = Pages.GetPageByte(address + 2 + 3);
    
        content += b3.ToHexString(2);
        content += b2.ToHexString(2);
        content += b1.ToHexString(2);
        content += b0.ToHexString(2);
        content += ' ';
        float f = Float.FromBytes(b0, b1, b2, b3);
        content += f.ToString();
        
        SafePad(ref content, paddingWidth);
        content += " (" + size.ToString() + " bytes)";
    
        memoryFile.Append(content + char(0x0A));
        DebugFlush(memoryFile);
        return size;
    }
    uint WalkMemoryVariant(file memoryFile, uint address, uint tp, uint indent)
    {
        accountedFor[address-2] = true;
        string content;
        uint size = Pages.GetPageWord(address - 2);
        content = content.Pad(' ', indent);
        content += WalkHeader(address);
        byte vt = Pages.GetPageByte(address + 2 + 0);
        content += vt.ToHexString(2);   
        content += ' ';
        byte b0 = Pages.GetPageByte(address + 2 + 1);
        byte b1 = Pages.GetPageByte(address + 2 + 2);
        content += b1.ToHexString(2);   
        content += b0.ToHexString(2);   
        
        SafePad(ref content, paddingWidth);
        content += " (" + size.ToString() + " bytes)";
        memoryFile.Append(content + char(0x0A));
        DebugFlush(memoryFile);
        return size;
    }
    uint WalkMemory(file memoryFile, uint value0, uint tp, uint indent)
    {
        uint size = 0;
        switch (tp)
        {
            case 0x0D:
            {
                if (!Is32BitStackSlot)
                {
                    size = size + WalkMemoryFloat(memoryFile, value0, tp, indent + 2);
                }
            }
            case 0x0E:
            {
                if (!Is32BitStackSlot)
                {
                    size = size + WalkMemoryLong(memoryFile, value0, tp, indent + 2);
                }
            }
            case 0x0F:
            {
                size = size + WalkMemoryString(memoryFile, value0, tp, indent + 2);
            }
            case 0x12:
            {
                size = size + WalkMemoryArray(memoryFile, value0, tp, indent + 2);
            }
            case 0x13:
            {
                size = size + WalkMemoryDictionary(memoryFile, value0, tp, indent + 2);
            }
            case 0x14:
            {
                size = size + WalkMemoryVariant(memoryFile, value0, tp, indent + 2);
            }
            case 0x15:
            {
                size = size + WalkMemoryFile(memoryFile, value0, tp, indent + 2);
            }
            case 0x16:
            {
                size = size + WalkMemoryDirectory(memoryFile, value0, tp, indent + 2);
            }
            case 0x19:
            {
                size = size + WalkMemoryList(memoryFile, value0, tp, indent + 2);
            }
        }
        return size;
    }
    
    WalkFreeList()
    {
        uint freePtr = Pages.GetZeroPage("FREELIST");
        loop
        {
            if (freePtr == 0)
            {
                break;
            }
            uint blockSize = Pages.GetPageWord(freePtr);
            uint nextPtr = Pages.GetPageWord(freePtr + 2); // next
            OutputDebug("freePtr=0x" + freePtr.ToHexString(4) + ", blockSize=0x" + blockSize.ToHexString(4)+ ", nextPtr=0x" + nextPtr.ToHexString(4));
            freeBlocks[freePtr] = true;
            freePtr = nextPtr;
        }
    }
    
    uint WalkDumpFreeList(file memoryFile, uint indent)
    {
        uint size = 0;
        uint heapStart  = Pages.GetZeroPage("HEAPSTART");
        uint heapSize   = Pages.GetZeroPage("HEAPSIZE");
        long heapPtr    = heapStart;
        long heapEndPtr = long(heapStart) + long(heapSize);
        loop
        {
            if (heapPtr >= heapEndPtr)
            {
                break;
            }
            uint blockSize = Pages.GetPageWord(heapPtr);
            if (freeBlocks.Contains(heapPtr))
            {
                string content;
                content = content.Pad(' ', indent);
                
                uint next = Pages.GetPageWord(heapPtr+2);
                content += "0x" + heapPtr.ToHexString(4) + " 0x" + blockSize.ToHexString(4)+ " 0x" + next.ToHexString(4);

                uint pCurrent = heapPtr+4;
                uint more = blockSize - 4;
                uint count = 0;
                while (more > 0) // always?
                {
                    uint data = Pages.GetPageByte(pCurrent);
                    content += " 0x";
                    content +=  data.ToHexString(2);
                    more--;
                    pCurrent++;
                    count++;
                    if (count == 16)
                    {
                        break;
                    }
                }
                SafePad(ref content, paddingWidth);
                content += " (" + blockSize.ToString() + " bytes)";
                memoryFile.Append(content + char(0x0A));
                DebugFlush(memoryFile);
                memoryFile.Flush(); // TODO REMOVE
                size = size + blockSize;
            }
            heapPtr = heapPtr + blockSize;
        }
        return size;
    }
    
    uint WalkHeap(file memoryFile, uint indent)
    {
        uint size = 0;
        uint heapStart  = Pages.GetZeroPage("HEAPSTART");
        uint heapSize   = Pages.GetZeroPage("HEAPSIZE");
        long heapPtr    = heapStart;
        long heapEndPtr = long(heapStart) + long(heapSize);
        
        OutputDebug("heapStart  = 0x" + heapStart.ToHexString(4));
        OutputDebug("heapSize   = 0x" + heapSize.ToHexString(4));
        OutputDebug("heapEndPtr = 0x" + heapEndPtr.ToHexString(5));
        loop
        {
            if (heapPtr >= heapEndPtr)
            {
                OutputDebug("heapPtr    = 0x" + heapPtr.ToHexString(4) + " LAST");
                break;
            }
            uint blockSize = Pages.GetPageWord(heapPtr);
            if (accountedFor.Contains(heapPtr))
            {
                // we already walked it
                OutputDebug("stackPtr   = 0x" + heapPtr.ToHexString(4) + ", blockSize=0x" + blockSize.ToHexString(4));
            }
            else if (freeBlocks.Contains(heapPtr))
            {
                // accounted for
                OutputDebug("freePtr    = 0x" + heapPtr.ToHexString(4) + ", blockSize=0x" + blockSize.ToHexString(4));
            }
            else if (blockSize <= 2)
            {
                OutputDebug("heapPtr    = 0x" + heapPtr.ToHexString(4) + ", blockSize=0x" + blockSize.ToHexString(4) + " WTF?!");
                break; // WTF?!
            }
            else
            {
                OutputDebug("heapPtr    = 0x" + heapPtr.ToHexString(4) + ", blockSize=0x" + blockSize.ToHexString(4));
                // allocated by Memory.Allocate(..) so not reachable from the stack
                string content;
                content = content.Pad(' ', indent);
                content += "0x" + heapPtr.ToHexString(4) + " 0x" + blockSize.ToHexString(4);
                uint pCurrent = heapPtr+2;
                uint more = blockSize - 2;
                uint count = 0;
                while (more > 0) // always?
                {
                    uint data = Pages.GetPageByte(pCurrent);
                    content += " 0x";
                    content +=  data.ToHexString(2);
                    more--;
                    pCurrent++;
                    count++;
                    if (count == 16)
                    {
                        break;
                    }
                }
                SafePad(ref content, paddingWidth);
                content += " (" + blockSize.ToString() + " bytes)";
                memoryFile.Append(content + char(0x0A));
                DebugFlush(memoryFile);
                memoryFile.Flush(); // TODO REMOVE
                size = size + blockSize;
            }
            heapPtr = heapPtr + blockSize;
        }
        return size;
    }
    DebugFlush(file memoryFile)
    {
        //memoryFile.Flush();
    }
    
    string ValueTypeToString(byte vtype, uint value0, uint value1)
    {
        string content;
        switch (vtype)
        {
            case 0x01: // char
            {
                content = "'" + char(value0) + "'";
            }
            case 0x03: // byte
            case 0x02: // uint
            {
                content = "(" + value0.ToString() + ")";
            }
            case 0x06: // bool
            {
                content = ((value0 == 0) && (value1 == 0)) ? "false" : "true";
            }
            case 0x04: // int
            {
                int ivalue = Int.FromBytes(byte(value0 & 0xFF), byte(value0 >> 8));
                content = "(" + ivalue.ToString() + ")";
            }
            case 0x0D: // float
            {
                byte b0 = byte(value0 & 0xFF);
                byte b1 = byte(value0 >> 8);
                byte b2 = byte(value1 & 0xFF);
                byte b3 = byte(value1 >> 8);
                float fvalue = Float.FromBytes(b0, b1, b2, b3);
                content = "(" + fvalue.ToString() + ")";
            }
            case 0x0E: // long
            {
                byte b0 = byte(value0 & 0xFF);
                byte b1 = byte(value0 >> 8);
                byte b2 = byte(value1 & 0xFF);
                byte b3 = byte(value1 >> 8);
                long lvalue = Long.FromBytes(b0, b1, b2, b3);
                content = "(" + lvalue.ToString() + ")";
            }
        }
        return content;
    }
    
    bool DumpMemory()
    {
        setSlots[0] = 0x01;
        setSlots[1] = 0x02;
        setSlots[2] = 0x04;
        setSlots[3] = 0x08;
        setSlots[4] = 0x10;
        setSlots[5] = 0x20;
        setSlots[6] = 0x40;
        setSlots[7] = 0x80;
        clearSlots[0] = 0xFE;
        clearSlots[1] = 0xFD;
        clearSlots[2] = 0xFB;
        clearSlots[3] = 0xF7;
        clearSlots[4] = 0xEF;
        clearSlots[5] = 0xDF;
        clearSlots[6] = 0xBF;
        clearSlots[7] = 0x7F;
        
        accountedFor.Clear();
        freeBlocks.Clear();
        
        <string, variant > globalLists;
        <string, <string,variant> > debugSymbols = Code.GetDebugSymbols();
        if (debugSymbols.Contains("globals"))
        {
            globalLists = debugSymbols["globals"];
        }
        
        bool success;
        loop
        {
            Pages.ClearPageData();
            Pages.LoadZeroPage(false); // for CSP and PC
            Output.Initialize();
            if (   !ZeroPageContains("PC") 
                || !ZeroPageContains("CSP") 
                || !ZeroPageContains("CODESTART")
               )
            {
                break;
            }
            string hexPath = Monitor.CurrentHexPath;
            if (hexPath.Length == 0)
            {
                break;
            }
            string fileName = Path.GetFileName(hexPath);
            string extension = Path.GetExtension(fileName);
            fileName = fileName.Replace(extension, "");
            
            string memoryPath = "/Debug/" + fileName + ".mem";
            File.Delete(memoryPath);
            file memoryFile = File.Create(memoryPath);
            if (!memoryFile.IsValid())
            {
                break;
            }
            
            // walk the stack
            <uint> stackValues0;
            <uint> stackValues1;
            <byte> stackTypes;
            uint bp;
            uint sp;
            if (!WalkStack(stackValues0, stackValues1, stackTypes, ref sp, ref bp))
            {
                break;
            }
            uint l = stackValues0.Count;
            uint size = 0;
            string content;
            for (uint i = 0; i < l; i++)
            {
                uint v0 = stackValues0[i];
                uint v1 = stackValues1[i];
                uint t = stackTypes[i];
                uint i2 = i * 2;
                
                content = "0x" + i2.ToHexString(2) + " 0x";
                if (Is32BitStackSlot)
                {
                    content += v1.ToHexString(4);
                }
                content += v0.ToHexString(4);
                content += " 0x" + t.ToHexString(2);
                if (i2 == bp)
                {
                    content = "BP  " + content;
                }
                else
                {
                    content = "    " + content;
                }
                if (!IsMachineReferenceType(t))
                {
                    content += " " + ValueTypeToString(byte(t), v0, v1);
                }
                string key = i2.ToString();
                if (globalLists.Contains(key))
                {
                    <string> globalList = globalLists[key];
                    content += "  // " + globalList[2] + " " + globalList[1];
                }
                memoryFile.Append(content + char(0x0A));
                DebugFlush(memoryFile);
                size = size + WalkMemory(memoryFile, v0, t, 9);
                Parser.ProgressTick(".");
            }
            content = "";
            SafePad(ref content, paddingWidth);
            content += " (" + size.ToString() + " bytes)";
            memoryFile.Append(content + char(0x0A));
            memoryFile.Append("" + char(0x0A));
            DebugFlush(memoryFile);
            
            Parser.ProgressTick(".");
            WalkFreeList();
            Parser.ProgressTick(".");
            uint heapExtras = WalkHeap(memoryFile, 4);
            if (heapExtras > 0)
            {
                size = size + heapExtras;
                content = "";
                SafePad(ref content, paddingWidth);
                content += " (" + size.ToString() + " bytes)";
                memoryFile.Append(content + char(0x0A));
                memoryFile.Append("" + char(0x0A));
                DebugFlush(memoryFile);
            }
            Parser.ProgressTick(".");
            uint freeSize = WalkDumpFreeList(memoryFile, 4);
            if (freeSize > 0)
            {
                size = size + freeSize;
                content = "";
                SafePad(ref content, paddingWidth);
                content += " (" + size.ToString() + " bytes)";
                memoryFile.Append(content + char(0x0A));
                DebugFlush(memoryFile);
            }
            Parser.ProgressTick(".");
            
            if (memoryFile.IsValid())
            {
                memoryFile.Flush();
                success = true;
            }
            break;
        }
        return success;
    }
}
