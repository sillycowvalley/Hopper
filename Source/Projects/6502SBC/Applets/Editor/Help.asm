unit Help
{
    
    // Help screen lines (pre-formatted for 80 columns)
    
#if defined(TURBO)
    const string helpTitle    = "               TURBO PASCAL EDITOR - COMMAND REFERENCE                          ";
    const string helpLine1    = "";
    const string helpHeader1  = "CURSOR MOVEMENT          BLOCK OPERATIONS           FILE OPERATIONS";
    const string helpLine2    = "---------------          ----------------           ---------------";
    const string helpLine3    = "Ctrl+S  Left             Ctrl+K B  Mark begin       Ctrl+K O Open file";
    const string helpLine4    = "Ctrl+D  Right            Ctrl+K K  Mark end         Ctrl+K S Save";
    const string helpLine5    = "Ctrl+E  Up               Ctrl+K C  Copy block       Ctrl+K D Save & Done (exit)";
    const string helpLine6    = "Ctrl+X  Down             Ctrl+K V  Move block       Ctrl+K Q Quit (abandon)";
    const string helpLine7    = "Ctrl+A  Word left        Ctrl+K Y  Delete block";
    const string helpLine8    = "Ctrl+F  Word right       Ctrl+K W  Write block";
    const string helpLine9    = "Ctrl+R  Page up          Ctrl+K R  Read file";
    const string helpLine10   = "Ctrl+C  Page down                                   DELETE OPERATIONS";
    const string helpLine11   = "                                                    -----------------";
    const string helpLine12   = "QUICK MOVEMENT           FIND/REPLACE               Ctrl+H   Backspace";
    const string helpLine13   = "----------------         -------------              Ctrl+G   Delete char";
    const string helpLine14   = "Ctrl+Q S  Start of line  Ctrl+Q F  Find             Ctrl+T   Delete word";
    const string helpLine15   = "Ctrl+Q D  End of line    Ctrl+L    Find next        Ctrl+Y   Delete line";
    const string helpLine16   = "Ctrl+Q R  Top of file                               Ctrl+Q Y Delete to EOL";
    const string helpLine17   = "Ctrl+Q C  End of file";
    const string helpLine18   = "Ctrl+Q B  Block begin";
    const string helpLine19   = "Ctrl+Q K  Block end ";
    const string helpLine20   = "";
#else
    const string helpTitle    = "               Hopper BIOS EDITOR - COMMAND REFERENCE                           ";
    const string helpLine1    = "";
    const string helpHeader1  = "CURSOR MOVEMENT          BLOCK OPERATIONS           FILE OPERATIONS";
    const string helpLine2    = "---------------          ----------------           ---------------";
    const string helpLine3    = "Arrows  Left             Ctrl+K B  Mark begin       Ctrl+K O Open file";
    const string helpLine4    = "        Right            Ctrl+K K  Mark end         Ctrl+O   Open file (F3)";
    const string helpLine5    = "        Up               Ctrl+K C  Copy block       Ctrl+K S Save file";
    const string helpLine6    = "        Down             Ctrl+K V  Move block       Ctrl+S   Save file (F2)";
    const string helpLine7    = "Home    Start of line    Ctrl+K Y  Delete block     Ctrl+K D Save & Done (exit)";
    const string helpLine8    = "End     End of line      Ctrl+K W  Write block      Ctrl+K Q Quit (abandon)";
    const string helpLine9    = "PgUp    Page up          Ctrl+K R  Read file        Ctrl+N   New File";
    const string helpLine10   = "PgDn    Page down        Ctrl+A    Select all";
    const string helpLine11   = "                                                    DELETE OPERATIONS";
    const string helpLine12   = "CLASSIC MOVEMENT         FIND/REPLACE               -----------------";
    const string helpLine13   = "----------------         -------------              Ctrl+H   Backspace";
    const string helpLine14   = "Ctrl+Q R  Top of file    Ctrl+Q F  Find             Ctrl+G   Delete char";
    const string helpLine15   = "Ctrl+Q C  End of file    Ctrl+L    Find next        Ctrl+T   Delete word";
    const string helpLine16   = "Ctrl+Q B  Block begin                               Ctrl+Y   Delete line";
    const string helpLine17   = "Ctrl+Q K  Block end                                 Ctrl+Q Y Delete to EOL";
    const string helpLine18   = "";
    const string helpLine19   = "                         MODERN ALIASES:  Ctrl+C=Copy, Ctrl+X=Cut, Ctrl+V=Paste";
    const string helpLine20   = "                         ---------------  Ctrl+Z=Undo/Redo";
#endif
    const string memoryLine   = "Available Memory: ";
    const string bytesLabel   = " bytes";

    const string helpBottom   = "                        Press any key to return to editor                       ";
    
    // Display help screen
    Show()
    {
        // Clear screen and home cursor
        Screen.Clear();
        Screen.Reset();
        
        // Title in inverse video
        Screen.Inverse();
        LDA #(helpTitle % 256)
        STA ZP.STRL
        LDA #(helpTitle / 256)
        STA ZP.STRH
        LDY #0
        printLine();
        Screen.Reset();
        
         // Help content - Y is preserved so just increment
        LDY #2  // Skip line 1 (blank)
        
        LDA #(helpHeader1 % 256)
        STA ZP.STRL
        LDA #(helpHeader1 / 256)
        STA ZP.STRH
        printLine();
        INY
        
        LDA #(helpLine2 % 256)
        STA ZP.STRL
        LDA #(helpLine2 / 256)
        STA ZP.STRH
        printLine();
        INY
        
        LDA #(helpLine3 % 256)
        STA ZP.STRL
        LDA #(helpLine3 / 256)
        STA ZP.STRH
        printLine();
        INY       
        
         
        LDA #(helpLine4 % 256)
        STA ZP.STRL
        LDA #(helpLine4 / 256)
        STA ZP.STRH
        printLine();
        INY
        
        LDA #(helpLine5 % 256)
        STA ZP.STRL
        LDA #(helpLine5 / 256)
        STA ZP.STRH
        printLine();
        INY
        
        LDA #(helpLine6 % 256)
        STA ZP.STRL
        LDA #(helpLine6 / 256)
        STA ZP.STRH
        printLine();
        INY
        
        LDA #(helpLine7 % 256)
        STA ZP.STRL
        LDA #(helpLine7 / 256)
        STA ZP.STRH
        printLine();
        INY
        
        LDA #(helpLine8 % 256)
        STA ZP.STRL
        LDA #(helpLine8 / 256)
        STA ZP.STRH
        printLine();
        INY
        
        LDA #(helpLine9 % 256)
        STA ZP.STRL
        LDA #(helpLine9 / 256)
        STA ZP.STRH
        printLine();
        INY
        
        LDA #(helpLine10 % 256)
        STA ZP.STRL
        LDA #(helpLine10 / 256)
        STA ZP.STRH
        printLine();
        INY
        
        LDA #(helpLine11 % 256)
        STA ZP.STRL
        LDA #(helpLine11 / 256)
        STA ZP.STRH
        printLine();
        INY
        
        LDA #(helpLine12 % 256)
        STA ZP.STRL
        LDA #(helpLine12 / 256)
        STA ZP.STRH
        printLine();
        INY
        
        LDA #(helpLine13 % 256)
        STA ZP.STRL
        LDA #(helpLine13 / 256)
        STA ZP.STRH
        printLine();
        INY
        
        LDA #(helpLine14 % 256)
        STA ZP.STRL
        LDA #(helpLine14 / 256)
        STA ZP.STRH
        printLine();
        INY
        
        LDA #(helpLine15 % 256)
        STA ZP.STRL
        LDA #(helpLine15 / 256)
        STA ZP.STRH
        printLine();
        INY
        
        LDA #(helpLine16 % 256)
        STA ZP.STRL
        LDA #(helpLine16 / 256)
        STA ZP.STRH
        printLine();
        INY
        
        LDA #(helpLine17 % 256)
        STA ZP.STRL
        LDA #(helpLine17 / 256)
        STA ZP.STRH
        printLine();
        INY
        
        LDA #(helpLine18 % 256)
        STA ZP.STRL
        LDA #(helpLine18 / 256)
        STA ZP.STRH
        printLine();
        INY
        
        LDA #(helpLine19 % 256)
        STA ZP.STRL
        LDA #(helpLine19 / 256)
        STA ZP.STRH
        printLine();
        INY  
        
        LDA #(helpLine20 % 256)
        STA ZP.STRL
        LDA #(helpLine20 / 256)
        STA ZP.STRH
        printLine();
        INY  
        
        
        INY

        LDA #(memoryLine % 256)
        STA ZP.STRL
        LDA #(memoryLine / 256)
        STA ZP.STRH
        LDY #22  // Line 22
        printLine();
        
        // Position after "Memory: " text
        LDA #18  // Column 8 (after "Available Memory: ")
        LDY #22
        Screen.GotoXY();
        
        Memory.Available();  // Returns bytes available in ZP.ACC (16-bit)
        Shared.MoveAccToTop();
        Long.Print();
        
        // Add " bytes" suffix
        LDA #(bytesLabel % 256)
        STA ZP.STRL
        LDA #(bytesLabel / 256)
        STA ZP.STRH
        
        Print.String();
                
        // Bottom prompt in inverse at line 24
        Screen.Reset();
        Screen.Inverse();
        LDA #(helpBottom % 256)
        STA ZP.STRL
        LDA #(helpBottom / 256)
        STA ZP.STRH
        LDY #24
        printLine();
        Screen.Normal();
        
        // Hide cursor while displaying help
        Screen.HideCursor();
        
        // Wait for any key
        Keyboard.GetKey();
        
        // Show cursor again
        Screen.ShowCursor();
        
        // Main program will call View.Render() to restore display
    }
    
    // Helper to print a line at specific row
    printLine()  // Input: Y = row, ZP.STR = string pointer
    {
        PHY
        
        // Position cursor at column 0, row in Y
        LDA #0      // Column 0
        Screen.GotoXY();  // A = col (0), Y = row
        
        Print.String();
               
        PLY  // Y restored to row number
    }}
