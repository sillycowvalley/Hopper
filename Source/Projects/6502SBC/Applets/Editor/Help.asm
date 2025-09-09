unit Help
{
    uses "System/Screen"
    uses "System/Serial"
    uses "Editor/Keyboard"
    
    // Help screen lines (pre-formatted for 80 columns)
    const string helpTitle    = "               Hopper BIOS EDITOR - COMMAND REFERENCE                           ";
    const string helpLine1    = "";
    const string helpHeader1  = "CURSOR MOVEMENT          BLOCK OPERATIONS           FILE OPERATIONS";
    const string helpLine2    = "---------------          ----------------           ---------------";
    const string helpLine3    = "Arrows  Left             Ctrl+K B  Mark begin       F2       Save";
    const string helpLine4    = "        Right            Ctrl+K K  Mark end         Ctrl+K S Save";
    const string helpLine5    = "        Up               Ctrl+K C  Copy block       Ctrl+K D Save & exit";
    const string helpLine6    = "        Down             Ctrl+K V  Move block       Ctrl+K Q Quit (no save)";
    const string helpLine7    = "Home    Start of line    Ctrl+K Y  Delete block     F3       Open file";
    const string helpLine8    = "End     End of line      Ctrl+K H  Hide block       ";
    const string helpLine9    = "PgUp    Page up          Ctrl+K W  Write block";
    const string helpLine10   = "PgDn    Page down        Ctrl+K R  Read file        DELETE OPERATIONS";
    const string helpLine11   = "                                                    -----------------";
    const string helpLine12   = "CLASSIC MOVEMENT         FIND/REPLACE               Ctrl+H   Backspace";
    const string helpLine13   = "----------------         -------------              Ctrl+G   Delete char";
    const string helpLine14   = "Ctrl+Q R  Top of file    Ctrl+Q F  Find             Ctrl+T   Delete word";
    const string helpLine15   = "Ctrl+Q C  End of file    Ctrl+Q A  Replace          Ctrl+Y   Delete line";
    const string helpLine16   = "Ctrl+Q B  Block begin    Ctrl+L    Find next        Ctrl+Q Y Delete to EOL";
    const string helpLine17   = "Ctrl+Q K  Block end";
    const string helpLine18   = "                         MODERN ALIASES:  Ctrl+C=Copy, Ctrl+X=Cut, Ctrl+V=Paste";
    const string helpLine19   = "                         ---------------  Ctrl+Z=Undo/Redo, Ctrl+S=Save,";
    const string helpLine20   = "                                          Ctrl+O=Open, Ctrl+N=New";
    
    const string helpBottom   = "                        Press any key to return to editor                       ";
    
    // Display help screen
    Show()
    {
        // Clear screen and home cursor
        Screen.Clear();
        
        // Title in inverse video
        Screen.Inverse();
        LDA #(helpTitle % 256)
        STA ZP.STRL
        LDA #(helpTitle / 256)
        STA ZP.STRH
        LDY #0
        printLine();
        Screen.Normal();
        
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
        
        
        // Bottom prompt in inverse at line 24
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
        
        // Print string using Serial.WriteChar
        LDY #0
        loop
        {
            LDA [ZP.STR], Y
            if (Z) { break; }
            Serial.WriteChar();
            INY
        }
        
        PLY  // Y restored to row number
    }}
