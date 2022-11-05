unit Editor
{
    uses "/Source/System/Keyboard"
    uses "/Source/System/Screen"
    uses "/Source/Editor/TextBuffer"
    uses "/Source/Compiler/Tokens/Token"
    uses "/Source/Editor/StatusBar"
    uses "/Source/Editor/MenuBar"
    uses "/Source/Editor/MessageBox"
    uses "/Source/Editor/Highlighter"
    uses "/Source/System/Diagnostics"
    
    uses "/Source/Editor/Debugger"
    
    // '/' excluded  on purpose to make path selection easier
    string wordDelimiters = ";,.:?(){}[]<>=!&|~^+-*% '\""; 
    
    uint cursorX;
    uint cursorY;
    
    uint selectionX;
    uint selectionY;
    bool selectionActive;
    
    string clipboard;
    
    uint bufferTopLeftX;
    uint bufferTopLeftY;
    
    uint lineNumberWidth;
    
    uint height;
    uint width;
    uint x0;
    uint y0;
    uint background;
    
    string projectPath;
    string currentPath;
    
    bool isHopperSource;
    
    bool ignoreNextClick;
    bool wasDown;
    uint  downX;
    uint  downY;
    
    string findString;
    string replaceString;
    
#ifdef PROFILER    
    bool isProfiler;
    <string, long> profileHits;
    <string, long> profileTime;
    long perfTotal = 0;
#endif
    
    <string, variant> statusbar;
    <string, variant> menubar;
    
    New(<string, variant> sb, <string, variant> mb)
    {
        Token.Initialize();
        statusbar = sb;
        menubar = mb;
        x0 = 0;
        y0 = 1;
        width  = Screen.Columns;
        height = (Screen.Rows-2);
        background = Color.LightestGray;
        
        TextBuffer.Initialize();
        
        selectionActive = false;

        bufferTopLeftX = 0;
        bufferTopLeftY = 0;
        
        // initial cursor is at the end of the last line (0-based position)
        cursorX = TextBuffer.GetLineLength(TextBuffer.GetLineCount()-1);
        cursorY = TextBuffer.GetLineCount()-1; 

        CalculateLineNumberWidth();
    }
    
    
    
    bool IsWordDelimiter(char c)
    {
        return wordDelimiters.Contains(c);
    }
    
    DisplayCursor(bool show)
    {
        Suspend();
        SetCursor(x0 + cursorX - bufferTopLeftX + lineNumberWidth, y0 + cursorY - bufferTopLeftY);
        // TODO: show and hide
        Resume(true); // probably interactive (waiting for a key)
    }
    
    bool IsSelected(uint column, uint row)
    {
        bool selected = false;
        if (selectionActive)
        {
            if (selectionY == cursorY) // simple case
            {
                if (row == cursorY)
                {
                    if (selectionX < cursorX)
                    {
                        selected = (column >= selectionX) && (column < cursorX);
                    }
                    else if (cursorX < selectionX)
                    {
                        selected = (column >= cursorX) && (column < selectionX);
                    }
                }
            }
            else if (selectionY < cursorY)
            {
                if ((row >= selectionY) && (row <= cursorY))
                {
                    if (row == selectionY)
                    {
                        selected = column >= selectionX;
                    }
                    else if (row == cursorY)
                    {
                        selected = column < cursorX;
                    }
                    else
                    {
                        selected = true;
                    }
                }
            }
            else if (cursorY < selectionY)
            {
                if ((row >= cursorY) && (row <= selectionY))
                {
                    if (row == cursorY)
                    {
                        selected = column >= cursorX;
                    }
                    else if (row == selectionY)
                    {
                        selected = column < selectionX;
                    }
                    else
                    {
                        selected = true;
                    }
                }
            }
        }
        return selected;
    }
    uint  NormalizeSelection()
    {
        uint length = 0;
        if (selectionY == cursorY) // simple case
        {
            if (selectionX < cursorX)
            {
                uint tmp = selectionX;
                selectionX = cursorX;
                cursorX = tmp;
            }
            length = selectionX - cursorX;
        }
        else 
        {
            if (selectionY < cursorY)
            {
                uint tmp = selectionX;
                selectionX = cursorX;
                cursorX = tmp;
                tmp = selectionY;
                selectionY = cursorY;
                cursorY = tmp;
            }
            length = TextBuffer.GetLineLength(cursorY)-cursorX+1;
            if (selectionY - cursorY > 1)
            {
                for (uint i= cursorY+1; i < selectionY; i++)
                {
                    length = length + TextBuffer.GetLineLength(i)+1;
                }
            }
            length = length + selectionX;
        }
        return length;
    }
    bool IsCursorVisible() 
    { 
       return cursorVisible; 
    }
    
    TextBufferUpdated(uint x, uint y, bool draw)
    {
        Suspend();
        DisplayCursor(false); // hide cursor if visible
        
        uint yLineLength = TextBuffer.GetLineLength(y);
        if (x > yLineLength)
        {
            x = yLineLength;
        }

        cursorX = x;
        cursorY = y;
            
        if (x < bufferTopLeftX)
        {
            bufferTopLeftX = x;
            draw = true;
        }
        if (y < bufferTopLeftY)
        {
            bufferTopLeftY = y;
            draw = true;
        }
        while (y >= bufferTopLeftY+height)
        {
            bufferTopLeftY = bufferTopLeftY + 1;
            draw = true;
        }
        if (x == 0)
        {
            if (bufferTopLeftX != 0)
            {
                bufferTopLeftX = 0;
                draw = true;
            }
        }
        else if (x-1 < bufferTopLeftX)
        {
            bufferTopLeftX = x-1;
            draw = true;
        }
        while (x >= bufferTopLeftX+width-lineNumberWidth+1)
        {
            bufferTopLeftX = bufferTopLeftX + 1;
            draw = true;
        } 
        if (draw)
        {
            CalculateLineNumberWidth();
            Draw();
        }
        DisplayCursor(true);
        StatusBar.SetLocation(statusbar, cursorX, cursorY);
        Resume(true); // probably interactive (waiting for a key)
    }
    
    
    bool CanUndo() 
    { 
        return TextBuffer.CanUndo(); 
    }
    bool CanRedo() 
    { 
        return TextBuffer.CanRedo(); 
    }
    bool HasText() 
    {
        uint lineCount = TextBuffer.GetLineCount();
        for (uint i=0; i < lineCount; i++)
        {
            if (TextBuffer.GetLineLength(i) > 0)
            {
                return true;
            }
        }
        return false;
    }
    bool HasSelection() 
    {
        return selectionActive && ((selectionY != cursorY) || (selectionX != cursorX));
    }
    bool HasClipboardText() 
    {
        return clipboard.Length > 0;
    }
    string GetClipboardText() 
    { 
        return clipboard; 
    }
    SetClipboardText(string text)
    {
        clipboard = text;
    }
    string GetSelectedText()
    {
        string text = "";
        bool wasSelected = false;
        uint lineCount = TextBuffer.GetLineCount();
        for (uint row = 0; row < lineCount; row++)
        {
            string ln = TextBuffer.GetLine(row);
            for (uint column = 0; column < ln.Length; column++)
            {
                bool isSelected = IsSelected(column, row);
                if (isSelected)
                {
                    if ((column == 0) && wasSelected)
                    {
                        // first character on new line isSelected and last character on previous line wasSelected so
                        text = text + char(0x0A);
                    }
                    text = text + ln[column];
                }
                wasSelected = isSelected;
            }
        }
        return text;
    }
    uint GetTextLength()
    {
        uint length = 0;
        uint lineCount = TextBuffer.GetLineCount();
        for (uint i=0; i < lineCount; i++)
        {
            length = length + TextBuffer.GetLineLength(i);
        }
        return length;
    }
    
    Undo()
    {
        < <string, uint > > record = TextBuffer.GetUndo();
        bool draw = false;
        uint x = cursorX;
        uint y = cursorY;
        uint ri = record.Length;
        loop // reverse order
        {
            if (ri == 0) 
            {
                break;
            }
            ri--;
            <string, uint > entry = record.GetItem(ri);
            if (entry["t"] == 0)
            {
                // was insertion so undo is a deletion
                x = entry["x"];
                y = entry["y"];
                if (TextBuffer.Delete(ref x, ref y))
                {
                    draw = true;
                }
            }
            else
            {
                // was deletion so undo is insertion
                x = entry["x"];
                y = entry["y"];
                uint c = entry["c"];
                TextBuffer.Insert(ref x, ref y, char(c));
                draw = true;
            }
        }
        if (selectionActive)
        {
            draw = true;
            selectionActive = false;
        }
        if ((x != cursorX) || (y != cursorY) || draw)
        {
            TextBufferUpdated(x, y, draw);
        }
        Editor.UpdateTitle(); // file has been modified
    }
    Redo()
    {
        bool draw = false;
        uint x = cursorX;
        uint y = cursorY;
        < <string, uint > > record = TextBuffer.GetRedo();
        foreach (var entry in record) // reverse reverse order -> regular order
        {
            if (entry["t"] == 0)
            {
                // was insertion so redo is a insertion
                x = entry["x"];
                y = entry["y"];
                uint c = entry["c"];
                TextBuffer.Insert(ref x, ref y, char(c));
                draw = true;
            }
            else
            {
                // was deletion so redo is deletion
                x = entry["x"];
                y = entry["y"];
                if (TextBuffer.Delete(ref x, ref y))
                {
                    draw = true;
                }
            }
        }
        if (selectionActive)
        {
            draw = true;
            selectionActive = false;
        }
        if ((x != cursorX) || (y != cursorY) || draw)
        {
            TextBufferUpdated(x, y, draw);
        }
        Editor.UpdateTitle(); // file has been modified
    }
    
    SelectAll()
    {
        selectionActive = true;
        selectionX = 0;
        selectionY = 0;
        uint lineCount = TextBuffer.GetLineCount();
        cursorX = TextBuffer.GetLineLength(lineCount-1);
        cursorY = lineCount-1;
        TextBufferUpdated(cursorX, cursorY, true);
    }
    Copy()
    {
        clipboard = "";

        bool wasSelected = false;
        uint lineCount = TextBuffer.GetLineCount();
        for (uint row = 0; row < lineCount; row++)
        {
            string ln = TextBuffer.GetLine(row);
            for (uint column = 0; column < ln.Length; column++)
            {
                bool isSelected = IsSelected(column, row);
                if (isSelected)
                {
                    if ((column == 0) && wasSelected)
                    {
                        // first character on new line isSelected and last character on previous line wasSelected so
                        clipboard = clipboard + char(0x0A);
                    }
                    clipboard = clipboard + ln[column];
                }
                wasSelected = isSelected;
            }
        }
    }
    Cut()
    {
        TextBuffer.StartJournal();
        Copy();
        DeleteSelection();
        TextBuffer.EndJournal();
    }
    Paste()
    {
        if (HasSelection())
        {
            DeleteSelection();
        }
        TextBuffer.StartJournal();
        uint x = cursorX;
        uint y = cursorY;
        selectionX = x;
        selectionY = y;
        for (uint i=0; i < clipboard.Length; i++)
        {
            TextBuffer.Insert(ref x, ref y, clipboard[i]);
        }
        selectionActive = false;
        TextBuffer.EndJournal();
        TextBufferUpdated(x, y, true);
    }
    DeleteSelection()
    {
        TextBuffer.StartJournal();
        uint length = NormalizeSelection();
        uint cX = cursorX;
        uint cY = cursorY;
        while (length > 0)
        {
            bool success = TextBuffer.Delete(ref cX, ref cY);
            length--;
        }
        TextBuffer.EndJournal();
        selectionActive = false;
        cursorX = cX;
        cursorY = cY;
        TextBufferUpdated(cursorX, cursorY, true);
    }
    uint GetCurrentLineNumber() 
    { 
        return cursorY+1; 
    }
    
    bool GotoLineNumber()
    {
        return GotoLineNumber(0, 0, true, true);
    }
    
    bool GotoLineNumber(uint gotoLine)
    {
        return GotoLineNumber(gotoLine, 0, false, true);
    }
    
    bool GotoLineNumber(uint gotoLine, uint gotoColumn)
    {
        return GotoLineNumber(gotoLine, gotoColumn, false, false);
    }
    
    bool GotoLineNumber(uint gotoLine, uint gotoColumn, bool defaultLine, bool defaultColumn)
    {
        // gotoLine = -1 : TextBuffer.GetLineCount()
        // gotoColumn    : first nonSpace on line
        uint x = cursorX;
        uint y = cursorY;
        if (defaultLine)
        {
            gotoLine = TextBuffer.GetLineCount();
        }
        if (gotoLine-1 <  TextBuffer.GetLineCount())
        {
            y = gotoLine-1;
            if (defaultColumn)
            {
                x = gotoColumn-1;
                if (x  > TextBuffer.GetLineLength(gotoLine-1))
                {
                    x = TextBuffer.GetLineLength(gotoLine-1);
                }
                DisplayCursor(false);
                bufferTopLeftY = y - (height / 2);
                if (bufferTopLeftY < 0)
                {
                    bufferTopLeftY = 0;
                }
            }
            else
            {
                // -1 means first non-space
                string ln = TextBuffer.GetLine(gotoLine-1);
                x = 0;
                foreach (var c in ln)
                {
                    if (c != ' ')
                    {
                        break;
                    }
                    x++;
                }
            }
            
            TextBufferUpdated(x, y, true);
            return true;
        }
        return false;
    }
    
    
    Dump(<string, variant> this)
    {
        < string, uint> values = this["values"];
        
        PrintLn("MenuBar:");
        Panel.Dump(this);
    }
    
    bool Contains(uint x, uint y)
    {
        bool contained = false;
        loop
        {
            if (x < x0+lineNumberWidth)
            {
                break;
            }
            if (y < y0)
            {
                break;
            }
            if (x >= x0 + width)
            {
                break;
            }
            if (y >= y0 + height)
            {
                break;
            }
            contained = true;
            break;
        }
        return contained;
    }
        
    bool OnKey(Key key)
    {
        bool isShifted = (Key.Shift == (key & Key.Shift));
        bool isControlled = (Key.Control == (key & Key.Control));
        bool isAlted = (Key.Alt == (key & Key.Alt));

        uint x = cursorX;
        uint y = cursorY;
        
        bool draw = false;
        string currentLine = TextBuffer.GetLine(cursorY);

        //string clickstring;
        Key unmaskedKey = (key & Key.Mask);
        if (unmaskedKey == Key.Click)
        {
            //clickstring = " [" + ToClickString() + "]";
            
            uint cx = ClickX;
            uint cy = ClickY;
            if (!Contains(cx, cy))
            {
                ignoreNextClick = true; // this click is not for you
            }
        }
        
        //string keystring = KeyToString(key);
        //string modifiers = ModifiersToString(key);
        //OutputDebug("OnKey: " + keystring + ", " + modifiers + clickstring);
        
        bool hasSelection = HasSelection();
        if (!isShifted && !ignoreNextClick)
        {
            // any key without shift down clears the selection
            if (selectionActive)
            {
                draw = true;
            }
            selectionActive = false;
        }
        switch (unmaskedKey) 
        {
        case Key.Scroll:
            {
                uint cx = ClickX;
                uint cy = ClickY;
                if (Contains(cx, cy))
                {
                    int delta = ScrollDelta;
                    if (delta > 0)
                    {
                        int ticks = 3 * delta;
                        loop
                        {
                            if ((bufferTopLeftY == 0) || (ticks == 0))
                            {
                                break;
                            }
                            bufferTopLeftY--;
                            if (y >= height + bufferTopLeftY)
                            {
                                y--;
                            }
                            draw = true;
                            ticks--;
                        }
                    }
                    else if (delta < 0)
                    {
                        int ticks = 3 * -delta;
                        loop
                        {
                            if ((bufferTopLeftY + height >= TextBuffer.GetLineCount()) || (ticks == 0))
                            {
                                break;
                            }
                            bufferTopLeftY++;
                            if (y < bufferTopLeftY)
                            {
                                y++;
                            }
                            draw = true;
                            ticks--;
                        }
                    }
                }
            }
        case Key.Click:
            {
                if (ignoreNextClick)
                {
                    ignoreNextClick = false;
                }
                else
                {
                    uint cx = ClickX;
                    uint cy = ClickY;
                        
                    if (Contains(cx, cy))
                    {
                        uint editX = cx - x0 - lineNumberWidth + bufferTopLeftX;
                        uint editY = cy - y0 + bufferTopLeftY;
                        if (editY > TextBuffer.GetLineCount()-1)
                        {
                            // clicked below the content of the file
                        }
                        else if (ClickUp)
                        {
                            x = editX;
                            y = editY;
                            draw = true;
                            if (isShifted)
                            {
                                // isShifted - select
                                if (!selectionActive)
                                {
                                    selectionX = cursorX; 
                                    selectionY = cursorY;
                                }
                                selectionActive = true;
                            }
                            else if (ClickDouble && (currentLine.Length > 0) && (x > 0) && (x < currentLine.Length) && !IsWordDelimiter(currentLine[x]))
                            {
                                // select current word
                                selectionX = x; 
                                selectionY = y; 
                                selectionActive = true;
                                
                                // find the left end of the current word
                                loop
                                {
                                    uint iLeft = selectionX-1;
                                    if (IsWordDelimiter(currentLine[iLeft]))
                                    {
                                        break;
                                    }
                                    selectionX = iLeft;
                                    if (selectionX == 0)
                                    {
                                        break;
                                    }
                                }
                                if (x < currentLine.Length)
                                {
                                    // find right end of current word
                                    loop
                                    {
                                        uint iRight = x + 1;
                                        if ((iRight < currentLine.Length) && IsWordDelimiter(currentLine[iRight]))
                                        {
                                            x = iRight;
                                            break;
                                        }
                                        x = iRight;
                                        if (x == currentLine.Length)
                                        {
                                            break;
                                        }
                                    }
                                }
                                ignoreNextClick = true; // single Up click follows the double click
                            }
                            else if (wasDown)
                            {
                                // click-drag-release to select
                                selectionX = downX; selectionY = downY; 
                                selectionActive = (x != selectionX) || (y != selectionY);
                                wasDown = false;
                            }
                        }
                        else // !ClickUp
                        {
                            if (!isShifted) // keep things simple
                            {
                                x = editX;
                                y = editY;
                                draw = true;
                                downX = x;
                                downY = y;
                                wasDown = true;
                            }
                        }
                    }
                }
            }
        case Key.Tab:
            {
                TextBuffer.StartJournal();
                if (hasSelection)
                {
                    // TODO: Tab should probably never delete
                    //     - <Tab> : indent selection by 4 spaces - all selected lines
                    //     - <Shift><Tab> : remove 4 spaces on left side of selection (if they exist) - all selected lines
                    uint ymin = y;
                    uint ymax = y;
                    if (selectionY < ymin)
                    {
                        ymin = selectionY;
                    }
                    if (selectionY > ymax)
                    {
                        ymax = selectionY;
                    }
                    uint yt = ymin;
                    if (!isShifted)
                    {
                        // <Tab> : block indent
                        loop
                        {
                            uint xt = 0;
                            uint ytr = yt;
                            for (uint i=0; i < 4; i++)
                            {
                                TextBuffer.Insert(ref xt, ref ytr, ' ');
                            }
                            if (yt == ymax)
                            {
                                break;
                            }
                            yt++;
                        }
                        selectionX = selectionX + 4;
                        x = x + 4; // to cause update below
                    }
                    else
                    {
                        // <Shift><Tab>
                        loop
                        {
                            uint xt = 0;
                            uint ytr = yt;
                            for (uint i=0; i < 4; i++)
                            {
                               string indentLine = TextBuffer.GetLine(ytr);
                               if ((indentLine.Length > 0) && (indentLine[0] == ' '))
                               {
                                   xt = 0;
                                   bool success = TextBuffer.Delete(ref xt, ref ytr);
                               }
                            }
                            if (yt == ymax)
                            {
                                break;
                            }
                            yt++;
                        }
                        for (uint i=0; i < 4; i++)
                        {
                            if (x > 0)
                            {
                                x--;
                            }
                            if (selectionX > 0)
                            {
                                selectionX--;
                            }
                        }
                    }
                    selectionActive = true;
                }
                else if (!isShifted)
                {
                    for (uint i=0; i < 4; i++)
                    {
                        TextBuffer.Insert(ref x, ref y, ' ');
                    }
                }
                else
                {
                    for (uint i=0; i < 4; i++)
                    {
                        uint xt = 0;
                        uint yt = y;
                        string indentLine = TextBuffer.GetLine(y);
                        if ((indentLine.Length > 0) && (indentLine[0] == ' '))
                        {
                            bool success = TextBuffer.Delete(ref xt, ref yt);
                            if (x > 0)
                            {
                                x--;
                            }
                        }
                    }
                }
                TextBuffer.EndJournal();
                draw = true;
            }
        case Key.Home:
            {
                if (isShifted)
                {
                    if (!selectionActive)
                    {
                        selectionX = cursorX; 
                        selectionY = cursorY;
                    }
                    selectionActive = true;
                }
                if (isControlled)
                {
                    y = 0;
                }
                if (currentLine.Length > 0)
                {
                    // go to first non whitespace first, then start of line
                    uint iNonSpace = 0;
                    for (uint i=0; i < currentLine.Length; i++)
                    {
                        if (currentLine[i] != ' ')
                        {
                            iNonSpace = i;
                            break;
                        }
                    }
                    if (x > iNonSpace)
                    {
                        x = iNonSpace;
                    }
                    else
                    {
                        x = 0;
                    }
                }
                else
                {
                    x = 0; 
                }
            }
        case Key.End:
            {
                if (isShifted)
                {
                    if (!selectionActive)
                    {
                        selectionX = cursorX; selectionY = cursorY; 
                    }
                    selectionActive = true;
                }
                if (isControlled)
                {
                    y = TextBuffer.GetLineCount()-1;
                    x = TextBuffer.GetLineLength(TextBuffer.GetLineCount()-1);
                }
                else
                {
                    x = currentLine.Length; // beyond end of current line
                }
            }
        case Key.Left: 
            {
                if (isShifted)
                {
                    if (!selectionActive)
                    {
                        selectionX = cursorX; selectionY = cursorY; 
                    }
                    selectionActive = true;
                }
                if (x > 0) 
                {
                    if (isControlled && (currentLine.Length > 0) && (x > 1))
                    {
                        // jump to the left end of the current word
                        uint iLeftEnd = x-1;
                        bool nonDelimiterSeen = false;
                        loop
                        {
                            if (iLeftEnd == 0)
                            {
                                break;
                            }
                            if (IsWordDelimiter(currentLine[iLeftEnd]))
                            {
                                if (nonDelimiterSeen)
                                {
                                    iLeftEnd++;
                                    break;
                                }
                            }
                            else 
                            {
                                nonDelimiterSeen = true;
                            }
                            iLeftEnd--;
                        }
                        x = x + iLeftEnd - x;
                    }
                    else
                    {
                        x = x - 1;
                    }
                    
                }
                else if (y > 0)
                {
                    y--;
                    x = TextBuffer.GetLineLength(y);
                }
            }
        case Key.Right: 
            {
                if (isShifted)
                {
                    if (!selectionActive)
                    {
                        selectionX = cursorX; selectionY = cursorY; 
                    }
                    selectionActive = true;
                }
                if (x < currentLine.Length) 
                {   
                    uint dx = 1;
                    if (isControlled)
                    {
                        if ((currentLine.Length > 0) && (x+1 < currentLine.Length-1))
                        {
                            // jump to the right end of the current word
                            uint iRightEnd = x+1;
                            bool startedAtDelimiter = IsWordDelimiter(currentLine[x]);
                            bool nonDelimiterSeen = false;
                            loop
                            {
                                if (iRightEnd == currentLine.Length-1)
                                {
                                    iRightEnd++;
                                    break;
                                }
                                if (IsWordDelimiter(currentLine[iRightEnd]))
                                {
                                    if (nonDelimiterSeen)
                                    {
                                        //iRightEnd++;
                                        break;
                                    }
                                }
                                else 
                                {
                                    if (startedAtDelimiter)
                                    {
                                        break;
                                    }
                                    nonDelimiterSeen = true;
                                }
                                iRightEnd++;
                            }
                            dx = iRightEnd - x;
                        }
                    }
                    x = x + dx;
                }
                else if (y < TextBuffer.GetLineCount()-1) 
                {
                    y++;
                    x = 0;
                }
            }
        case Key.Up: 
            {
                if (isShifted && !selectionActive)
                {
                    selectionX = cursorX; selectionY = cursorY; selectionActive = true;
                }
                if (y > 0) 
                {
                    if (isControlled)
                    {
                        if (bufferTopLeftY > 0)
                        {
                            bufferTopLeftY--;
                            if (y >= height + bufferTopLeftY)
                            {
                                y--;
                            }
                            draw = true;
                        }
                    }
                    else
                    {
                        y--;
                    }
                }
            } 
        case Key.Down: 
            {
                if (isShifted && !selectionActive)
                {
                    selectionX = cursorX; selectionY = cursorY; selectionActive = true;
                }
                if (isControlled)
                {
                    if (bufferTopLeftY + height < TextBuffer.GetLineCount())
                    {
                        bufferTopLeftY++;
                        if (y < bufferTopLeftY)
                        {
                            y++;
                        }
                        draw = true;
                    }
                }
                else if (y < TextBuffer.GetLineCount()-1) 
                {
                    y++;
                }
            }
        case Key.PageDown:
            {
                if (isShifted && !selectionActive)
                {
                    selectionX = cursorX; selectionY = cursorY; selectionActive = true;
                }
                y = y + height;
                if (y > TextBuffer.GetLineCount()-1)
                {
                    y = TextBuffer.GetLineCount()-1;
                }
            }
        case Key.PageUp:
            {
                if (isShifted && !selectionActive)
                {
                    selectionX = cursorX; selectionY = cursorY; selectionActive = true;
                }
                if (y > height)
                {
                    y = y - height;
                }
                else
                {
                    y = 0;
                }
            }
        case Key.Enter:
            {
                if (hasSelection)
                {
                    DeleteSelection();
                    x = cursorX;
                    y = cursorY;
                }
                uint spaceCount;
                if (y > 0)
                {
                    string spaceLine = currentLine;
                    uint slen = spaceLine.Length;
                    uint xs = x;
                    bool usePrevious = false;
                    char rc;
                    while (xs < slen)
                    {
                        rc = spaceLine[xs];
                        if (rc != ' ')
                        {
                            usePrevious = true;
                            break;                        
                        }
                        xs++;
                    }
                    if ((xs == x) && ((rc == '}') || (rc == '{')))
                    {
                        // if we are sitting just left of '{' or '}' then
                        // maintain their position
                        spaceCount = x; 
                    }
                    else
                    {
                        if (usePrevious)
                        {
                            spaceLine = TextBuffer.GetLine(y-1);
                        }
                        foreach (var cp in spaceLine)
                        {
                            if (cp == ' ')
                            {
                                spaceCount++;
                            }
                            else
                            {
                                if (cp == '{')
                                {
                                    spaceCount = spaceCount + 4;
                                }
                                break;                         
                            }
                        }
                    }
                }
                uint length = currentLine.Length;
                char c = char(0x0A);
                TextBuffer.StartJournal();
                TextBuffer.Insert(ref x, ref y, c);
                x = x + spaceCount;
                loop
                {
                    // the above insert incremented y
                    if (spaceCount == 0)
                    {
                        break;
                    }
                    spaceCount--;
                    uint xi = 0;
                    TextBuffer.Insert(ref xi, ref y, ' ');
                }
                TextBuffer.EndJournal();
                draw = true;
            }
        case Key.Backspace:
            {
                if (hasSelection)
                {
                    DeleteSelection();
                    x = cursorX;
                    y = cursorY;
                }
                else
                {
                    // delete the character before the cursor - move the cursor back one, then delete what is at the cursor
                    if (x > 0)
                    {
                        x--;
                        TextBuffer.StartJournal();
                        if (TextBuffer.Delete(ref x, ref y))
                        {
                            draw = true;
                        }
                        TextBuffer.EndJournal();
                    }
                    else if (y > 0)
                    {
                        y--;
                        x = TextBuffer.GetLineLength(y);
                        TextBuffer.StartJournal();
                        if (TextBuffer.Delete(ref x,ref y))
                        {
                            draw = true;
                        }
                        TextBuffer.EndJournal();
                    }
                }
            }
        case Key.Delete:
            {
                if (hasSelection)
                {
                    DeleteSelection();
                    x = cursorX;
                    y = cursorY;
                }
                else
                {
                    // delete what is at the cursor
                    TextBuffer.StartJournal();
                    if (TextBuffer.Delete(ref x, ref y))
                    {
                        draw = true;
                    }
                    TextBuffer.EndJournal();
                }
            }
        default:
            {
                if (isAlted || isControlled)
                {
                    // ignore here (don't allow <ctrl><a> ->'a', etc)
                }
                else
                {
                    if (key == (Key.ModSpace | Key.Shift))
                    {
                        // must be shifted (see above)
                        key = Key.Space;
                    }
                    uint ik = uint(key);
                    if ((ik > 31) && (ik < 255))
                    {
                        char character = char(ik);
                        if (hasSelection)
                        {
                            DeleteSelection();
                            x = cursorX;
                            y = cursorY;
                        }
                        TextBuffer.StartJournal();
                        TextBuffer.Insert(ref x, ref y, character);
                        TextBuffer.EndJournal();
                        draw = true;
                    }
                }
            } // default
        } // switch
        if (selectionActive)
        {
            draw = true;
        }
        if ((x != cursorX) || (y != cursorY) || draw)
        {
            if (y >= 0)
            {
                TextBufferUpdated(x, y, draw);
            }
            if ((Key.Click == unmaskedKey) && ClickDouble && HasSelection() && (selectionY == cursorY)) // one line selection?
            {
                string candidate = GetSelectedText();
                if (candidate.Contains("/"))
                {
                    string extension = Path.GetExtension(candidate);
                    if (extension == ".")
                    {
                        candidate = candidate + ".hs";
                    }
                    if (File.Exists(candidate))
                    {
                        if (Editor.CanUndo())
                        {
                            Editor.OpenPath(candidate); // offer undo
                        }
                        else
                        {
                            Editor.LoadFile(candidate); // just open it
                        }
                        
                    }
                }
            }
            return true; // used this key
        }
        return false;
    }

    CalculateLineNumberWidth()
    {
        lineNumberWidth = 3;
        uint lineCount = TextBuffer.GetLineCount();
        if (lineCount > 999)
        {
            lineNumberWidth = 5;
        }
        else if (lineCount > 99)
        {
            lineNumberWidth = 4;
        }
#ifdef PROFILER        
        if (isProfiler)
        {
            lineNumberWidth = lineNumberWidth + 20;
        }
#endif
        
    }
    DrawAll()
    {
        MenuBar.Draw(menubar);
        StatusBar.Draw(statusbar);
        Editor.Draw();
    }
    
    Draw()
    {
        Draw(height);
    }
    Draw(uint h)
    {
        <uint> colours;
        
        Suspend();
        
        // render the text buffer
        uint lineCount = TextBuffer.GetLineCount();
        for (uint r=0; r < h; r++)
        {
            // does this line in the window have a line in the TextBuffer?
            uint lineIndex = r + bufferTopLeftY;

            uint lineNumber = lineIndex+1;
            char bp = ' ';
            uint breakColor = Color.Red;
            uint runColor = Color.MarginGray;
            
            // draw line number column on left
            uint c = lineNumberWidth-1;
            Screen.DrawChar(x0+c, r+y0, bp, breakColor, runColor); 
            
            string ln;
            if (lineIndex < lineCount)
            {
                ln = TextBuffer.GetLine(lineIndex);
            }
            
            if (lineIndex >= lineCount)
            {
                lineNumber = 0;
            }
            while (c > 0)
            {
                c--;
                char character = ' ';
                if (lineNumber != 0) 
                {
                    uint digit = (lineNumber % 10) + 48; // '0'
                    character = char(digit);
                }
                Screen.DrawChar(x0+c, r+y0, character, Color.SlateBlue, Color.MarginGray); 
                lineNumber = lineNumber / 10;
            }
#ifdef PROFILER            
            if (isProfiler && (ln.Length >= 6) && ln.StartsWith("0x"))
            {
                string address = ln.Substring(0, 6);
                if (profileHits.Contains(address))
                {
                    long hits = profileHits[address];
                    long us   = profileTime[address];
                    string perf = us.ToString() + "us, " + hits.ToString();
                    uint xx = 1;
                    
                    uint perfColor = 0xF66;
                    if (us > 50)
                    {
                        perfColor = 0xF33;
                    }
                    if (us > 100)
                    {
                        perfColor = 0xF00;
                    }
                    if (us > 500)
                    {
                        perfColor = 0xF3F;
                    }
                    float percent = 100.0 * us / perfTotal;
                    if (percent > 0.99)
                    {
                        us = us / hits;
                        string ps = percent.ToString();
                        uint iDot;
                        if (ps.IndexOf('.', ref iDot))
                        {
                            if (iDot + 3 < ps.Length)
                            {
                                ps = ps.Substring(0, iDot+3);
                            }
                        }
                        perf = us.ToString() + "us, " + hits.ToString();
                        perf = perf + " " + ps + "%";
                    }
                    
                    foreach (var ch in perf)
                    {
                        Screen.DrawChar(x0+xx, r+y0, ch, perfColor, Color.MarginGray); 
                        xx++;
                    }
                }
            }
#endif            
            
            // draw text buffer content
            c = 0;
            if (lineIndex < lineCount)
            {
                if (isHopperSource)
                {
                    colours = Highlighter.Hopper(ln, background);
                }
                uint colourOffset = 0;
                
                if (ln.Length <= bufferTopLeftX)
                {
                    ln = "";
                }
                else
                {
                    ln = ln.Substring(bufferTopLeftX);
                    colourOffset = colourOffset + bufferTopLeftX;
                }
                while ((c < ln.Length) && (c < width - lineNumberWidth))
                {
                    bool isSelected = IsSelected(c, lineIndex);
                    uint bColor = background;
                    if (isSelected)
                    {
                        bColor = Color.Gray;
                    }
                    uint textColor = Color.Black;
                    if (isHopperSource)
                    {
                        textColor = colours[colourOffset + c];
                    }
                    Screen.DrawChar(c+x0+lineNumberWidth, r+y0, ln[c], textColor, bColor); 
                    c++;
                }
            }
            while (c < width- lineNumberWidth)
            {
                // pad the rest of the line with blacks
                Screen.DrawChar(c+x0+lineNumberWidth, r+y0, ' ', Color.Black, background); 
                c++;
            }
        }
        
        DisplayCursor(true);
        Resume(true); // we're probably interactive (waiting for a key press)
    }
    
    string OfferSave()
    {
        <string> buttons;
        buttons.Append("Yes");
        buttons.Append("No");
        buttons.Append("Cancel");
        
        <string, variant> mb = MessageBox.New("Save", "Save changes?", "  '" + currentPath + "'", buttons);
        DisplayCursor(false);
        string result = MessageBox.Execute(mb);
        DisplayCursor(true);
        Draw();
        if (result == "Yes")
        {
            Editor.Save();
        }
        return result;
    }

    Save()
    {
        if (File.Exists(currentPath))
        {
            File.Delete(currentPath);
        }
        
        file textFile = File.Create(currentPath);
        if (textFile.IsValid())
        {
            uint lines = TextBuffer.GetLineCount();
            for (uint i = 0; i < lines; i++)
            {
                string ln = TextBuffer.GetLine(i);
                ln = ln + char(0x0A);
                textFile.Append(ln);
            }
            textFile.Flush();
            TextBuffer.ClearUndo();
            Editor.UpdateTitle(); // file is no longer modified
            StatusBar.SetText(statusbar, "Saved");
        }
    }
    
    SetStatusBarText(string text)
    {
        StatusBar.SetText(statusbar, text);
    }
    
    SetNewPath(string path)
    {
        path = GetFullPath(path); // correct case
        currentPath = path;   
    }
    string GetCurrentPath()
    {
        return currentPath;
    }
    string GetProjectPath()
    {
        return projectPath;
    }
    LoadFile(string path)
    {
        LoadFile(path, 0, true);
    }
    LoadFile(string path, uint gotoLine)
    {
        LoadFile(path, gotoLine, false);
    }
    LoadFile(string path, uint gotoLine, bool defaultLine)
    {
        path = GetFullPath(path); // correct case
#ifdef PROFILER    
        isProfiler = false;
        profileHits.Clear();
        profileTime.Clear();
        perfTotal = 0;
                
        string ext = (Path.GetExtension(path)).ToLower();
        if (ext == ".hasm")
        {
            string perfPath = (path.ToLower()).Replace(".hasm", ".perf");
            if (File.Exists(perfPath))
            {
                isProfiler = true;
                file perfFile = File.Open(perfPath);
                if (perfFile.IsValid())
                {
                    loop
                    {   
                        string ln = perfFile.ReadLine();
                        if (ln.Length == 0)
                        {
                            if (!perfFile.IsValid())
                            {
                                break;
                            }
                        }
                        <string> parts = ln.Split(' ');
                        long hits = 0;
                        long us = 0;
                        if (Token.TryParseLong(parts[1], ref us))
                        {
                            profileTime[parts[0]] = us;
                            perfTotal = perfTotal + us;
                        }
                        if (Token.TryParseLong(parts[2], ref hits))
                        {
                            profileHits[parts[0]] = hits;
                        }
                    }
                }
            }
        }
        
#endif        
    
        TextBuffer.Clear();
        
        file textFile = File.Open(path);
        if (textFile.IsValid())
        {
            TextBuffer.AddFile(textFile);
            if (defaultLine)
            {
                bool success = GotoLineNumber();
            }
            else
            {
                bool success = GotoLineNumber(gotoLine);
            }
        }
        currentPath = path;

        string extension = Path.GetExtension(currentPath);
        extension = extension.ToLower();
        isHopperSource = (extension == ".hs");

        if (projectPath.Length == 0) // first load
        {
            projectPath = currentPath;
        }
        CalculateLineNumberWidth();
        cursorX = 0;
        cursorY = 0;
        selectionActive = false;
        TextBufferUpdated(cursorX, cursorY, true);
        UpdateTitle();
    }
    UpdateTitle()
    {
        MenuBar.SetTitleText(menubar, currentPath, projectPath);
        StatusBar.SetText(statusbar, ""); // clears the "Saved" - not ideal
    }
    
    bool CheckLineNumber(string candidate)
    {
        bool ok = false;
        uint gotoLine = 0;
        if (TryParseUInt(candidate, ref gotoLine))
        {
            uint maxLines = TextBuffer.GetLineCount();
            if ((gotoLine >= 1) && (gotoLine <= maxLines))
            {
                ok = true;
            }
        }
        return ok;
    }
    
    Goto()
    {
        <string> buttons;
        buttons.Append("OK");
        buttons.Append("Cancel");
        
        <string, variant> mb = MessageBox.New("Go To Line", "Line:", "", buttons, 1, 10);
        DisplayCursor(false);
        
        <string, string> fields = mb["fields"];
        fields["0"] = ""; // (GetCurrentLineNumber()).ToString();
        mb["fields"] = fields;
        mb["allowed"] = "0123456789";
        
        MessageBox.ValidationDelegate validation = CheckLineNumber;
        
        string result = MessageBox.Execute(mb, validation);
        DisplayCursor(true);
        Draw();
        if (result == "OK")
        {
            <string, string> fieldsAfter = mb["fields"];
            string gotoString = fieldsAfter["0"];
            uint gotoLine = 0;
            if (TryParseUInt(gotoString, ref gotoLine))
            {
                selectionActive = false;
                if (Editor.GotoLineNumber(gotoLine))
                {
                }
            }
        }
    }
    bool CheckNotEmpty(string content)
    {
        content = content.Trim();
        return content.Length > 0;
    }
    
    bool HasFind()
    {
        return findString.Length > 0;
    }
    FindNext()
    {
        uint lineCount = TextBuffer.GetLineCount();
        uint currentX = cursorX;
        uint currentY = cursorY;
        
        loop
        {
            string currentLine = TextBuffer.GetLine(currentY);
            uint iFind;
            if (currentLine.IndexOf(findString, currentX, ref iFind))
            {
                selectionX = iFind;
                selectionY = currentY;
                selectionActive = true;
                cursorX = iFind + findString.Length;
                cursorY = currentY;
                bool success = Editor.GotoLineNumber(cursorY + 1, cursorX+1);
                cursorX = iFind + findString.Length; // GotoLineNumber munts cursorX
                DisplayCursor(true);
                TextBufferUpdated(cursorX, cursorY, true);
                break;
            }
            currentY++;
            currentX = 0;
            if (currentY == lineCount)
            {
                currentY = 0;
            }
            if (currentY == cursorY)
            {   
                break; 
            }
        }
    }
    
    Find()
    {
        <string> buttons;
        buttons.Append("OK");
        buttons.Append("Cancel");
        
        <string, variant> mb = MessageBox.New("Find", "Text:", "", buttons, 1, 30);
        DisplayCursor(false);
        
        <string, string> fields = mb["fields"];
        
        string initialText = ""; // findString; <- never useful
        if (HasSelection() && (selectionY == cursorY)) // one line selection?
        {
            initialText = GetSelectedText();
        }
        
        fields["0"] = initialText; 
        mb["fields"] = fields;
        mb["allowed"] = "";
        
        MessageBox.ValidationDelegate validation = CheckNotEmpty;
        
        string result = MessageBox.Execute(mb, validation);
        DisplayCursor(true);
        Draw();
        if (result == "OK")
        {
            <string, string> fieldsAfter = mb["fields"];
            findString = fieldsAfter["0"];
            FindNext();
        }
    }
    
    bool ValidateFilePathExists(string filePath)
    {
        return File.Exists(filePath);
    }
    
    Open()
    {
        OpenPath("");
    }
    
    OpenPath(string suggestedPath)
    {
        loop
        {
            if (Editor.CanUndo())
            {
                string result = Editor.OfferSave();
                if (result == "Cancel")
                {
                    break;
                }
            }
            
            <string> buttons;
            buttons.Append("OK");
            buttons.Append("Cancel");
            
            <string, variant> mb = MessageBox.New("Open", "Path:", "", buttons, 1, 50);
            DisplayCursor(false);
            
            string initialText = currentPath;
            if (suggestedPath != "")
            {
                initialText = suggestedPath;
            }
            else if (HasSelection() && (selectionY == cursorY)) // one line selection?
            {
                string candidate = GetSelectedText();
                if (candidate.Contains("/"))
                {
                    string extension = Path.GetExtension(candidate);
                    if (extension == ".")
                    {
                        candidate = candidate + ".hs";
                    }
                    if (File.Exists(candidate))
                    {
                        initialText = candidate;
                    }
                }
            }      
            <string, string> fields = mb["fields"];
            fields["0"] = initialText;
            mb["fields"] = fields;
            mb["allowed"] = "";
            
            MessageBox.ValidationDelegate validation = ValidateFilePathExists;
            
            string result = MessageBox.Execute(mb, validation);
            DisplayCursor(true);
            Draw();
            if (result == "OK")
            {
                <string, string> fieldsAfter = mb["fields"];
                LoadFile(fieldsAfter["0"]);
            }
           
            break;
        }
    }
    
    bool ValidateFilePathNew(string filePath)
    {
        bool ok = false;
        loop
        {
            string folder   = GetDirectoryName(filePath);
            string filename = GetFileName(filePath);
            if (filename.Length == 0)
            {
                break;
            }
            if (!Directory.Exists(folder))
            {
                break;
            }
            uint iDot;
            if (filename.IndexOf('.', ref iDot))
            {
                uint iDotLast;
                if (filename.LastIndexOf('.', ref iDotLast))
                {
                    if (iDot != iDotLast)
                    {
                        break;
                    }
                }
            }
            if (iDot == 0)
            {
                break;
            }
            if (iDot == filename.Length-1)
            {
                break;
            }
            ok = true;
            foreach (var c in filePath)
            {
                if (!IsValidPathCharacter(c))
                {
                    ok = false;
                    break;
                }
            }
            if (!ok)
            {
                break;
            }
            
            break;
        }
        return ok;
    }
    
    SaveAs()
    {
        loop
        {
            <string> buttons;
            buttons.Append("OK");
            buttons.Append("Cancel");
            
            <string, variant> mb = MessageBox.New("Save As", "Path:", "", buttons, 1, 50);
            DisplayCursor(false);
            
            <string, string> fields = mb["fields"];
            fields["0"] = currentPath;
            mb["fields"] = fields;
            mb["allowed"] = "";
            
            MessageBox.ValidationDelegate validation = ValidateFilePathNew;
            
            string result = MessageBox.Execute(mb, validation);
            DisplayCursor(true);
            Draw();
            if (result == "OK")
            {
                <string, string> fieldsAfter = mb["fields"];
                string savePath = fieldsAfter["0"];
                if (File.Exists(savePath))
                {
                    <string> buttons2;
                    buttons2.Append("Yes");
                    buttons2.Append("No");
                    buttons2.Append("Cancel");
                    
                    mb = MessageBox.New("Overwrite", "File exists! Overwrite?", "  '" + savePath + "'", buttons2);
                    DisplayCursor(false);
                    result = MessageBox.Execute(mb);
                    DisplayCursor(true);
                    Draw();
                    if (result == "No")
                    {
                        continue; // try again
                    }
                    else if (result == "Yes")
                    {
                        // go ahead and overwrite
                    }
                    else
                    {
                        break; // cancel on anything else
                    }
                }
                currentPath = savePath;
                Editor.Save();
            }
           
            break;
        }
    }
    
    FileNew()
    {
        loop
        {
            if (Editor.CanUndo())
            {
                string result = Editor.OfferSave();
                if (result == "Cancel")
                {
                    break;
                }
            }
            
            <string> buttons;
            buttons.Append("OK");
            buttons.Append("Cancel");
            
            <string, variant> mb = MessageBox.New("New", "Path:", "", buttons, 1, 50);
            DisplayCursor(false);
            
            <string, string> fields = mb["fields"];
            fields["0"] = currentPath;
            mb["fields"] = fields;
            mb["allowed"] = "";
            
            MessageBox.ValidationDelegate validation = ValidateFilePathNew;
            
            string result = MessageBox.Execute(mb, validation);
            DisplayCursor(true);
            Draw();
            if (result == "OK")
            {
                <string, string> fieldsAfter = mb["fields"];
                string newPath = fieldsAfter["0"];
                if (File.Exists(newPath))
                {
                    <string> buttons2;
                    buttons2.Append("OK");
                    buttons2.Append("Cancel");
                    
                    mb = MessageBox.New("Exists", "File exists! Cannot overwrite.", "  '" + newPath + "'", buttons2);
                    DisplayCursor(false);
                    result = MessageBox.Execute(mb);
                    DisplayCursor(true);
                    Draw();
                    if (result == "OK")
                    {
                        continue; // try again
                    }
                    else
                    {
                        break; // cancel on anything else
                    }
                }
                currentPath = newPath;
                TextBuffer.Clear();
                TextBuffer.Initialize();
                if (projectPath.Length == 0) // first load
                {
                    projectPath = currentPath;
                }
                CalculateLineNumberWidth();
                cursorX = 0;
                cursorY = 0;
                TextBufferUpdated(cursorX, cursorY, true);
                UpdateTitle();
                Editor.Save();
            }
            break;
        }
    }
    
    Replace()
    {
        <string> buttons;
        buttons.Append("OK");
        buttons.Append("Cancel");
        
        <string, variant> mb = MessageBox.New("Go To Line", "Line:", "Replace with:", buttons, 2, 10);
        DisplayCursor(false);
        
        <string, string> fields = mb["fields"];
        fields["0"] = (GetCurrentLineNumber()).ToString();
        fields["1"] = (GetCurrentLineNumber()).ToString();
        mb["fields"] = fields;
        mb["allowed"] = "0123456789";
        
        MessageBox.ValidationDelegate validation = CheckLineNumber;
        
        string result = MessageBox.Execute(mb, validation);
        DisplayCursor(true);
        Draw();
        if (result == "OK")
        {
            <string, string> fieldsAfter = mb["fields"];
            string gotoString = fieldsAfter["0"];
            uint gotoLine = 0;
            if (TryParseUInt(gotoString, ref gotoLine))
            {
                if (Editor.GotoLineNumber(gotoLine))
                {
                
                }
            }
        }
    }
    
    bool Always()
    {
        return true;
    }
    
    RegisterCommands()
    {   
    
        Commands.CommandExecuteDelegate saveCommand = Editor.Save;
        Commands.CommandEnabledDelegate saveEnabled = Editor.CanUndo;
        Key key = (Key.Control | Key.ModS);
        InstallCommand("Save", "&Save", saveCommand, saveEnabled, key);

        Commands.CommandExecuteDelegate openCommand = Editor.Open;
        Commands.CommandEnabledDelegate openEnabled = Editor.Always;
        key = (Key.Control | Key.ModO);
        InstallCommand("Open", "&Open..", openCommand, openEnabled, key);

        Commands.CommandExecuteDelegate saveAsCommand = Editor.SaveAs;
        Commands.CommandEnabledDelegate saveAsEnabled = Editor.HasText;
        key = Key.NoKey;
        InstallCommand("SaveAs", "Save &As..", saveAsCommand, saveAsEnabled, key);

        Commands.CommandExecuteDelegate newCommand = Editor.FileNew;
        Commands.CommandEnabledDelegate newEnabled = Editor.Always;
        key = (Key.Control | Key.ModN);
        InstallCommand("New", "&New..", newCommand, newEnabled, key);


        Commands.CommandExecuteDelegate selectAllCommand = Editor.SelectAll;
        Commands.CommandEnabledDelegate selectAllEnabled = Editor.HasText;
        key = (Key.Control | Key.ModA);
        InstallCommand("SelectAll", "Select &All", selectAllCommand, selectAllEnabled, key);
        
        Commands.CommandExecuteDelegate gotoCommand = Editor.Goto;
        Commands.CommandEnabledDelegate gotoEnabled = Editor.HasText;
        key = (Key.Control | Key.ModG);
        InstallCommand("Goto", "&Go To Line..", gotoCommand, gotoEnabled, key);
        
        Commands.CommandExecuteDelegate findCommand = Editor.Find;
        Commands.CommandEnabledDelegate findEnabled = Editor.HasText;
        key = (Key.Control | Key.ModF);
        InstallCommand("Find", "&Find..", findCommand, findEnabled, key);
        
        Commands.CommandExecuteDelegate findNext = Editor.FindNext;
        Commands.CommandEnabledDelegate findAgainEnabled = Editor.HasFind;
        key = Key.F3;
        InstallCommand("FindNext", "", findNext, findAgainEnabled, key);
                
        Commands.CommandExecuteDelegate undoCommand = Editor.Undo;
        Commands.CommandEnabledDelegate undoEnabled = Editor.CanUndo;
        key = (Key.Control | Key.ModZ);
        InstallCommand("Undo", "&Undo", undoCommand, undoEnabled, key);
        
        Commands.CommandExecuteDelegate redoCommand = Editor.Redo;
        Commands.CommandEnabledDelegate redoEnabled = Editor.CanRedo;
        key = (Key.Control | Key.ModY);
        InstallCommand("Redo", "&Redo", redoCommand, redoEnabled, key);

        Commands.CommandExecuteDelegate deleteCommand = Editor.DeleteSelection;
        Commands.CommandEnabledDelegate deleteEnabled = Editor.HasSelection;
        key = Key.Delete;
        InstallCommand("Delete", "&Delete", deleteCommand, deleteEnabled, key);
        
        Commands.CommandExecuteDelegate cutCommand = Editor.Cut;
        Commands.CommandEnabledDelegate cutEnabled = Editor.HasSelection;
        key = (Key.Control | Key.ModX);
        InstallCommand("Cut", "Cu&t", cutCommand, cutEnabled, key);
        
        Commands.CommandExecuteDelegate copyCommand = Editor.Copy;
        Commands.CommandEnabledDelegate copyEnabled = Editor.HasSelection;
        key = (Key.Control | Key.ModC);
        InstallCommand("Copy", "&Copy", copyCommand, copyEnabled, key);
        
        Commands.CommandExecuteDelegate pasteCommand = Editor.Paste;
        Commands.CommandEnabledDelegate pasteEnabled = Editor.HasClipboardText;
        key = (Key.Control | Key.ModV);
        InstallCommand("Paste", "&Paste", pasteCommand, pasteEnabled, key);
    }
}
