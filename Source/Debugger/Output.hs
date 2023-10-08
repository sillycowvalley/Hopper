unit Output
{
    uses "/Source/System/Screen"
    
    uses "/Source/Debugger/Source"
    uses "/Source/Debugger/6502/Pages"
    
    uses "/Source/Editor/Highlighter"
    uses "/Source/Editor/MessageBox"
    
    bool passThrough;
    
    const byte remoteConsoleHeight = 24;
    const byte maxDataWidth = 40;
    
    byte outputLeft;
    byte outputTop;
    byte outputWidth;
    byte outputHeight;
    byte consoleHeight;
    byte currentX;
    byte currentY;
    
    <uint, string> codeClicks;
    <uint, string> dataClicks;
    uint codeClickFirstCurrent;
    
    const uint arraySize     = 8192; // > Screen.Rows*Screen.Columns
    char[arraySize] outputCharacters;
    uint[arraySize] outputForeColors;
    uint[arraySize] outputBackColors;
    
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
        consoleHeight = remoteConsoleHeight;
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
                uint bcolor = Black;
                if (r >= outputTop + consoleHeight)
                {
                    bcolor = LightestGray;
                }
                for (byte c = outputLeft; c < outputLeft + outputWidth; c++)
                {
                    uint index  = r * Screen.Columns + c;
                    outputCharacters[index] = ' ';
                    outputForeColors[index]  = bcolor;
                    outputBackColors[index]  = bcolor;
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
            DrawChar(outputLeft-1, r, ' ',  Color.MarginFace, Color.MarginFace);
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
        if (passThrough)
        {
            if (byte(c) == 0x0D)
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
            if (byte(c) == 0x0D)
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
                    uint index  = currentY * Screen.Columns + currentX;
                    outputCharacters[index] = ' ';
                    outputForeColors[index] = foreColor;
                    outputBackColors[index] = backColor;
                    doDraw = true;
                }
                else if (currentY > outputTop)
                {
                    currentX = outputLeft + outputWidth-1;
                    currentY--;
                    uint index  = currentY * Screen.Columns + currentX;
                    outputCharacters[index] = ' ';
                    outputForeColors[index] = foreColor;
                    outputBackColors[index] = backColor;
                    doDraw = true;
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
            <uint> colors = Highlighter.Hopper(content, backColor);
            
            for (byte i = 0; i < content.Length; i++)
            {
               byte col = x + outputLeft + i;
               if (col < outputLeft + outputWidth)
               {
                   uint index  = row * Screen.Columns + col;
                   outputCharacters[index] = content[i];
                   outputForeColors[index] = colors[i];
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
            <string, <string> > argumentInfo = methodSymbols["arguments"];
            bool first = true;
            foreach (var kv in argumentInfo)
            {
                if (!first)
                {
                    content = content + "` ";
                }
                <string> argumentList = kv.value;
                
                int delta;
                if (Int.TryParse(kv.key, ref delta))
                {
                }
                uint voffset = uint(int(bp) +  delta);
                uint value = GetPageWord(voffset);
                string vtype = argumentList[1];
                bool isReference = (argumentList[0] == "true");
                if (verbose)
                {
                    if (isReference)
                    {
                        content = content + "ref ";
                    }
                    content = content + vtype + " ";
                }
                content = content + argumentList[2] + "=";
                content = content + TypeToString(value, vtype, isReference, maxDataWidth);
                
                first = false;
            }
        }
        content = content + ")";
        return content;
    }   
    
    CallStack(bool stack8, uint csp)
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
        
        uint bp;
        uint tsp;
        uint address;
        uint methodIndex;
        string sourceIndex;
        string methodString;
        
        bool trimming = false;
        
        // callstack methods
        uint icsp = 2;
        while (icsp < csp)
        {
            address = Pages.GetPageWord(0x0400+icsp);
            bp      = Pages.GetPageWord(0x0400+icsp+2);
            if (stack8)
            {
                bp  = 0x600 + bp;
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
            PrintWatch(0, yCurrent, methodString.Pad(' ', outputWidth), Black, Color.ListGray);
            
            yCurrent++;
            icsp = icsp + 4;
        }
        
        // current method
        if (stack8)
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
        if ((methodString.Length > outputWidth) && methodString.Contains('`'))
        {
            bool fa = true;
            uint iParen;
            string sPad;
            <string> parts = methodString.Split('`');
            for (uint i = 0; i < parts.Length; i++)
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
                PrintWatch(0, yCurrent, content.Pad(' ', outputWidth), Color.Black, Color.ActiveListGray);
                yCurrent++;       
            }
        }
        else
        {
            codeClicks[yCurrent] = sourceIndex;
            PrintWatch(0, yCurrent, methodString.Pad(' ', outputWidth), Color.Black, Color.ActiveListGray);
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
        for (uint iLocal = 0; iLocal < winningRanges.Length; iLocal++)
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
            uint lvalue;
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
                // index in string
                uint ivalue = Pages.GetPageWord(bp + ioffset);
                
                lname = lname.Substring(0, lname.Length-2);
                ltype = "char";
                uint sPtr = Pages.GetPageWord(bp + offset);
                char ch = Source.StringGetChar(sPtr, ivalue);
                lcontent = "'" + ch + "'";
            }
            else if (lname.EndsWith("_c") && Source.IsListType(ltype, ref vType) && (vType != ""))
            {
                // foreach (var item in <str>) case
                lvalue = Pages.GetPageWord(bp + offset);
                string irange = winningRanges[iLocal+1];
                <string> ilocalList = localCandidates[irange];
                iLocal++;
                string ioffsets = ilocalList[2];
                uint ioffset;
                if (!UInt.TryParse(ioffsets, ref ioffset))
                {
                    Die(0x0B);
                }
                // index in list
                uint ivalue = Pages.GetPageWord(bp + ioffset);
                lname = lname.Substring(0, lname.Length-2);
                ltype = vType;
                
                uint lPtr = Pages.GetPageWord(bp + offset);   
                uint item = Source.ListGetItem(lPtr, ivalue);
                lcontent = TypeToString(item, ltype, false, maxDataWidth);
                cPtr = item;
                cName = lname;
            }
            else if (lname.EndsWith("_c") && Source.IsArrayType(ltype, ref vType) && (vType != ""))
            {
                // foreach (var item in [bool]) case
                lvalue = Pages.GetPageWord(bp + offset);
                string irange = winningRanges[iLocal+1];
                <string> ilocalList = localCandidates[irange];
                iLocal++;
                string ioffsets = ilocalList[2];
                uint ioffset;
                if (!UInt.TryParse(ioffsets, ref ioffset))
                {
                    Die(0x0B);
                }
                // index in array
                uint ivalue = Pages.GetPageWord(bp + ioffset);
                lname = lname.Substring(0, lname.Length-2);
                ltype = vType;
                
                uint aPtr = Pages.GetPageWord(bp + offset);   
                uint item = Source.ArrayGetItem(aPtr, ivalue);
                lcontent = TypeToString(item, ltype, false, maxDataWidth);
                cPtr = item;
                cName = lname;
            }
            else if (   lname.EndsWith("_c") && Source.IsDictionaryType(ltype, ref kType, ref vType) 
                     && (kType != "") && (vType != ""))
            {
                // foreach (var item in <char,uint>) case
                lvalue = Pages.GetPageWord(bp + offset);
                string irange = winningRanges[iLocal+1];
                <string> ilocalList = localCandidates[irange];
                iLocal++;
                string ioffsets = ilocalList[2];
                uint ioffset;
                if (!UInt.TryParse(ioffsets, ref ioffset))
                {
                    Die(0x0B);
                }
                
                uint ivalue = Pages.GetPageWord(bp + ioffset);
                string kname = lname.Substring(0, lname.Length-2) + ".key";
                string vname = lname.Substring(0, lname.Length-2) + ".value";
                
                uint dPtr = Pages.GetPageWord(bp + offset);   
                uint iterator = 0;
                uint kvalue;
                uint vvalue;
                loop
                {
                    if (Source.DictionaryNextItem(dPtr, ref iterator, ref kvalue, ref vvalue))
                    {
                    }
                    if (ivalue == iterator)
                    {
                        break;
                    }
                }
                lcontent = TypeToString(kvalue, kType, false, maxDataWidth);
                PrintWatch(0, yCurrent, "  " + kType + " " + kname + "=" + lcontent, Black, LightestGray);
                yCurrent++;
                
                if (!Types.IsValueType(kType))
                {
                    dataClicks[yCurrent-1] = kType + "`" + kname + "`" + kvalue.ToString();
                }
                
                lcontent = TypeToString(vvalue, vType, false, maxDataWidth);
                PrintWatch(0, yCurrent, "  " + vType + " " + vname + "=" + lcontent, Black, LightestGray);
                ltype  = vType;
                cPtr = vvalue;
                cName = vname;
                yCurrent++;
                wasKV = true;
            }
            else
            {
                lvalue = GetPageWord(bp + offset);
                lcontent = TypeToString(lvalue, ltype, false, maxDataWidth);
                cPtr = lvalue;
            }
            lcontent = lcontent.Replace(namespacePlusDot, "");
            if (!wasKV)
            {
                PrintWatch(0, yCurrent, "  " + ltype + " " + lname + "=" + lcontent, Black, LightestGray);
                yCurrent++;
            }
            if (!Types.IsValueType(ltype))
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
            if (globalList.Length == 0)
            {
                continue;
            }
            uint gaddress = goffset + 0x0600;
            uint gvalue = GetPageWord(gaddress);
            
            string gtype = globalList[0];
            
            string gcontent = TypeToString(gvalue, gtype, false, maxDataWidth);
            string identifier = globalList[1];
            if (trimming || (identifier.Length > outputWidth))
            {
                identifier = identifier.Replace(namespacePlusDot, "");
                trimming = true;
            }
            
            PrintWatch(0, yCurrent, "  " + gtype + " " + identifier + "=" + gcontent, Black, LightestGray);
            yCurrent++;
            if (!Types.IsValueType(gtype))
            {
                dataClicks[yCurrent-1] = gtype + "`" + identifier + "`" + gvalue.ToString();
            }
        }
        Draw();
    }
    bool ConsoleHitTest(uint x, uint y)
    {
        if (   (x >= outputLeft) 
            && (x <= outputLeft+outputWidth)
            && (y >= outputTop) 
            && (y <  outputTop+remoteConsoleHeight)
           )
        {
            return true;
        }
        return false;
    }
    
    GotoSourceIndex(string sourceIndex, bool setActiveLine)
    {
        if (sourceIndex.Length > 0)
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
            string gcontent = TypeToString(cvalue, ctype, false, 2048);
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
                && (yClick >= outputTop+remoteConsoleHeight) 
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
                            ShadeWatch(byte(y), Color.ActiveListGray);
                        }
                        else if ((index >= codeClickFirstCurrent) && (y >= codeClickFirstCurrent))
                        {
                            ShadeWatch(byte(y), Color.ActiveListGray);
                        }
                        else
                        {
                            ShadeWatch(byte(y), Color.ListGray);
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
}
