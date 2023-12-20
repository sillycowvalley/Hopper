program Show
{

    uses "/Source/System/System"
    uses "/Source/System/Screen"
    uses "/Source/System/Keyboard"
    uses "/Source/Compiler/Tokens/Token"
    uses "/Source/Editor/Highlighter"

    uint backColor = 0xAAA;
    uint plainTextColor = 0x066;
    
    uint dataColor = 0x900;
    uint addressColor = 0xF60;
    uint operandColor = 0x33F;
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
        ln = ln.Pad(' ', Screen.Columns);
        uint iComment;
        bool commentFound;
        if (ln.IndexOf("//", ref iComment))
        {
            commentFound = true;
        }
        if (ln.Length >= 6) 
        {
            if (commentFound && (iComment < 6))
            {
                // whole line is a comment
            }
            else
            {
                Print(ln.Substring(0,6), addressColor, backColor); // address
                ln = ln.Substring(6);
            }
        }
        string trimmedLine = ln.Trim();
        if (trimmedLine.Length > 55)
        {
            // data line
            Print(ln.Substring(0,50), dataColor, backColor);
            Print(ln.Substring(50),   Constant, backColor);
        }
        else
        {
            string comment;
            commentFound = false;
            if (ln.IndexOf("//", ref iComment))
            {
                commentFound = true;
            }
            bool allComment = false;
            if (commentFound)
            {
                comment = ln.Substring(iComment);
                ln = ln.Substring(0,iComment);
                allComment = (iComment < 20);
            }
            if (!allComment)
            {
                // instruction line
                Print(ln.Substring(0,13), Statement, backColor);
                Print(ln.Substring(13,7), Type, backColor);
                Print(ln.Substring(20), operandColor, backColor);
            }
            
            if (comment.Length > 0)
            {
                Print(comment, listingCommentColor, backColor);
            }
        }
    }
    
    HopperLinePrinter(string ln)
    {
        ln = ln.Pad(' ', Screen.Columns);
        uint blockCommentNesting;
        <uint> colours = Highlighter.Hopper(ln, backColor, ref blockCommentNesting);
        uint length = ln.Length;
        for (uint i=0; i < length; i++)
        {
            uint colour = colours[i];
            char c = ln[i];
            Print(c, colour, backColor);
        }
    }
    
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
            else if (filePath.Length > 0)
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
            if (extension == ".hs")
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
