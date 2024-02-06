program Show
{

    uses "/Source/System/System"
    uses "/Source/System/Screen"
    uses "/Source/System/Keyboard"
    uses "/Source/Compiler/Tokens/Token"
    uses "/Source/Editor/Highlighter"

    uint backColor           = 0xAAA;
    uint plainTextColor      = 0x066;
    
    uint dataColor           = 0x900;
    uint addressColor        = 0x006;
    uint opcodeColor         = 0x33F;
    uint listingCommentColor = 0x360;
        
    bool more = false;
    bool wide = false;
    
    bool MorePrompt()
    {
        byte moreY = CursorY;
        string output = "More .. ";
        Print(output.Pad(' ', Screen.Columns-1), LightGray, DarkGray);
        SetCursor(8, moreY);
        
        Key character = ReadKey();
        
        SetCursor(0, moreY);
        bool exit = false;
        if (character == (Key.Control | Key.ModC ))
        {
            output = "Cancelled";
            Print(output.Pad(' ', Screen.Columns), LightGray, DarkGray);
            exit = true;
        }
        else
        {
            Print("       ", DarkGray, DarkGray);
            SetCursor(0, moreY);
        }
        return exit;
    }

    delegate LinePrinter(string ln);
    
    DefaultLinePrinter(string ln)
    {
        Print(ln.Pad(' ', Screen.Columns), plainTextColor, backColor);
    }
    
    ListingLinePrinter(string ln)
    {
        uint iComment;
        bool commentFound;
        bool allComment;
        string comment;
        if (ln.IndexOf("//", ref iComment))
        {
            commentFound = true;
            if (ln.Trim().StartsWith("//"))
            {
                comment = ln;
                ln = "";
            }
            else
            {
                comment = ln.Substring(iComment);
                ln = ln.Substring(0,iComment);
            }
        }
        if (ln.Length != 0)
        {
            if (!allComment && (ln.Length >= 6)) 
            {
                Print(ln.Substring(0,6), addressColor, backColor); // address
                ln = ln.Substring(6);
            }
            string trimmedLine = ln.Trim();
            if (trimmedLine.Length > 55)
            {
                // data line
                Print(ln.Substring(0,50), dataColor, backColor);
                Print(ln.Substring(50),   Constant,  backColor);
            }
            else
            {
                // instruction line
                Print(ln.Substring(0,17), Statement, backColor); // hex
                ln = ln.Substring(17);
                
                // leading space
                while ((ln.Length != 0) &&  (ln[0] == ' '))
                {
                    Print(' ', opcodeColor, backColor);
                    ln = ln.Substring(1);
                }
                
                uint iSpace;
                if (!ln.IndexOf(' ', ref iSpace))
                {
                    Print(ln, opcodeColor, backColor); // opcode, no operand
                }
                else
                {
                    Print(ln.Substring(0,iSpace), opcodeColor, backColor); // opcode
                    Print(ln.Substring(iSpace), dataColor,    backColor); // operand
                }
            }
        }
        if (comment.Length != 0)
        {
            Print(comment, listingCommentColor, backColor);
        }
        uint cx = Screen.CursorX;
        while  (cx < Screen.Columns)
        {
            Print(' ', backColor, backColor);
            cx++;
        }
    }
    
    HopperLinePrinter(string ln)
    {
        ln = ln.Pad(' ', Screen.Columns);
        uint blockCommentNesting;
        <uint> colours = Highlighter.HopperSource(ln, "", backColor, ref blockCommentNesting);
        uint length = ln.Length;
        for (uint i=0; i < length; i++)
        {
            uint colour = colours[i];
            char c = ln[i];
            Print(c, colour, backColor);
        }
    }
    
    Hopper()
    {
        <string> arguments = System.Arguments;
        
        string filePath;
        bool showHelp;
        
        foreach (var argument in arguments)
        {
            if (argument.ToLower() == "-m")
            {
                more = true;
            }
            else if (argument.ToLower() == "-w")
            {
                wide = true;
            }
            else if (filePath.Length != 0)
            {
                showHelp = true;
                break;
            }
            else
            {
                filePath = argument;
            }
        }
        
        if (filePath.Length == 0)
        {
            showHelp = true;
        }
        
        loop
        {
            if (!showHelp)
            {
                // check the file
                if (!File.Exists(filePath))
                {
                    string fullPath = Path.Combine(System.CurrentDirectory, filePath);
                    if (!File.Exists(fullPath))
                    {
                        PrintLn(fullPath);
                        PrintLn("File '" + filePath + "' not found.");
                        showHelp = true;
                    }
                    else
                    {
                        filePath = fullPath;
                    }
                }
            }
            
            if (showHelp)
            {
                PrintLn("SHOW [-m] [-w] <filepath>");
                PrintLn("      -m : more - prompt on each page");
                PrintLn("      -w : wide - wrap lines, don't truncate");
                break;
            }
            
            
            LinePrinter linePrinter = DefaultLinePrinter;
            string extension = Path.GetExtension(filePath);
            extension = extension.ToLower();
            if ((extension == ".hs") || (extension == ".json") || (extension == ".code") || (extension == ".options"))
            {
                linePrinter = HopperLinePrinter;
                Token.Initialize();// inialize the tokenizer
            }
            else if (extension == ".hasm")
            {
                linePrinter = ListingLinePrinter;
            }
            
            file textFile = File.Open(filePath);
            if (!textFile.IsValid())
            {
                PrintLn("Error attempting to open file '" + filePath +"'", Red, Black);
            }
            else
            {
                int moreLimit = Screen.Rows - 1;
                int linesPrinted = 0;
                Suspend();
                loop
                {
                    string ln = textFile.ReadLine();
                    if (ln.Length == 0)
                    {
                        if (!textFile.IsValid())
                        {
                            break;
                        }
                    }
                    if (!wide && (ln.Length > Screen.Columns))
                    {
                        ln = ln.Substring(0, Screen.Columns);
                    }
                    linePrinter(ln);
                    linesPrinted++;
                    if (more && (linesPrinted == moreLimit))
                    {
                        Resume(false);
                        if (MorePrompt())
                        {
                            Suspend();
                            break;
                        }
                        Suspend();
                        linesPrinted = 0;
                    }
                }
                Resume(false);
            }
            break;
        }
    }
}
