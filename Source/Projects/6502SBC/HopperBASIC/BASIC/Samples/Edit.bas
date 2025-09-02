NEW

! Simple Text Editor for HopperBASIC
! Uses VT100 codes for display, CHAR arrays for text storage

CONST MAXLINES = 100      ! Maximum number of lines
CONST MAXCOLS = 80        ! Maximum line length
CONST SCREENHEIGHT = 24   ! Terminal height
CONST STATUSLINE = 24     ! Status line at bottom

! Key codes
CONST KEYUP = 128
CONST KEYDOWN = 129
CONST KEYRIGHT = 130
CONST KEYLEFT = 131
CONST ESC = 27
CONST ENTER = 13
CONST BACKSPACE = 8
CONST DELETE = 127
CONST CTRLX = 24    ! Exit
CONST CTRLS = 19    ! Save
CONST CTRLL = 12    ! Load

! Text buffer - simple array of characters with newlines
CHAR TextBuffer[8000]  ! ~100 lines of 80 chars
VAR BufferSize = 0
VAR CursorX = 1
VAR CursorY = 1
VAR TopLine = 1      ! First visible line
VAR Modified = FALSE
VAR FileName[14]     ! Current filename

! VT100 Functions
FUNC ClearScreen()
    PRINT CHR(27); "[2J";
    PRINT CHR(27); "[H";
ENDFUNC

FUNC GotoXY(x, y)
    PRINT CHR(27); "["; y; ";"; x; "H";
ENDFUNC

FUNC ClearLine()
    PRINT CHR(27); "[K";
ENDFUNC

FUNC ShowCursor()
    PRINT CHR(27); "[?25h";
ENDFUNC

FUNC HideCursor()
    PRINT CHR(27); "[?25l";
ENDFUNC

! Find start of line N in buffer
FUNC FindLine(n)
    VAR pos = 0
    VAR line = 1
    
    IF n = 1 THEN RETURN 0 ENDIF
    
    WHILE (pos < BufferSize) AND (line < n)
        IF TextBuffer[pos] = CHR(10) THEN
            line = line + 1
        ENDIF
        pos = pos + 1
    WEND
    
    RETURN pos
ENDFUNC

! Count total lines
FUNC CountLines()
    VAR lines = 1
    FOR i = 0 TO BufferSize - 1
        IF TextBuffer[i] = CHR(10) THEN
            lines = lines + 1
        ENDIF
    NEXT i
    RETURN lines
ENDFUNC

! Display text from TopLine
FUNC Redraw()
    VAR pos
    VAR y = 1
    VAR x = 1
    
    HideCursor()
    GotoXY(1, 1)
    
    pos = FindLine(TopLine)
    
    WHILE (pos < BufferSize) AND (y < STATUSLINE)
        IF TextBuffer[pos] = CHR(10) THEN
            ClearLine()
            y = y + 1
            IF y < STATUSLINE THEN
                GotoXY(1, y)
            ENDIF
            x = 1
        ELSE
            IF x <= MAXCOLS THEN
                PRINT TextBuffer[pos];
                x = x + 1
            ENDIF
        ENDIF
        pos = pos + 1
    WEND
    
    ! Clear remaining lines
    WHILE y < STATUSLINE
        GotoXY(1, y)
        ClearLine()
        y = y + 1
    WEND
    
    ShowStatus()
    GotoXY(CursorX, CursorY)
    ShowCursor()
ENDFUNC

FUNC ShowStatus()
    GotoXY(1, STATUSLINE)
    ClearLine()
    ! Inverse video
    PRINT CHR(27); "[7m";
    
    IF LEN(FileName) > 0 THEN
        PRINT " "; FileName;
    ELSE
        PRINT " [No Name]";
    ENDIF
    
    IF Modified THEN
        PRINT " *";
    ENDIF
    
    PRINT "  Line "; CursorY + TopLine - 1; "  Col "; CursorX;
    PRINT "  ^X=Exit ^S=Save ^L=Load";
    
    ! Normal video
    PRINT CHR(27); "[0m";
ENDFUNC

! Insert character at current position
FUNC InsertChar(c)
    VAR pos = FindPosition()
    
    ! Shift everything right
    IF BufferSize < LEN(TextBuffer) - 1 THEN
        FOR i = BufferSize TO pos + 1 STEP -1
            TextBuffer[i] = TextBuffer[i - 1]
        NEXT i
        
        TextBuffer[pos] = c
        BufferSize = BufferSize + 1
        Modified = TRUE
        
        IF c = CHR(10) THEN
            CursorY = CursorY + 1
            CursorX = 1
        ELSE
            CursorX = CursorX + 1
        ENDIF
        
        Redraw()
    ENDIF
ENDFUNC

! Calculate buffer position from cursor
FUNC FindPosition()
    VAR pos = FindLine(TopLine + CursorY - 1)
    VAR col = 1
    
    WHILE (col < CursorX) AND (pos < BufferSize)
        IF TextBuffer[pos] = CHR(10) THEN
            RETURN pos
        ENDIF
        pos = pos + 1
        col = col + 1
    WEND
    
    RETURN pos
ENDFUNC

! Save file
FUNC SaveFile()
    IF LEN(FileName) = 0 THEN
        GotoXY(1, STATUSLINE)
        ClearLine()
        PRINT "Filename: ";
        ! Simple filename input (would need better implementation)
        RETURN FALSE
    ENDIF
    
    EXPORT(TextBuffer, FileName)
    Modified = FALSE
    RETURN TRUE
ENDFUNC

! Load file
FUNC LoadFile(name)
    VAR count = IMPORT(TextBuffer, name)
    IF count > 0 THEN
        BufferSize = count
        FOR i = 0 TO 13
            IF i < LEN(name) THEN
                FileName[i] = name[i]
            ELSE
                FileName[i] = CHR(0)
            ENDIF
        NEXT i
        Modified = FALSE
        RETURN TRUE
    ENDIF
    RETURN FALSE
ENDFUNC

! Key handler functions
FUNC HandleUp()
    IF CursorY > 1 THEN
        CursorY = CursorY - 1
    ELSE
        IF TopLine > 1 THEN
            TopLine = TopLine - 1
            Redraw()
        ENDIF
    ENDIF
    GotoXY(CursorX, CursorY)
ENDFUNC

FUNC HandleDown()
    IF CursorY < STATUSLINE - 1 THEN
        CursorY = CursorY + 1
    ELSE
        TopLine = TopLine + 1
        Redraw()
    ENDIF
    GotoXY(CursorX, CursorY)
ENDFUNC

FUNC HandleLeft()
    IF CursorX > 1 THEN
        CursorX = CursorX - 1
        GotoXY(CursorX, CursorY)
    ENDIF
ENDFUNC

FUNC HandleRight()
    IF CursorX < MAXCOLS THEN
        CursorX = CursorX + 1
        GotoXY(CursorX, CursorY)
    ENDIF
ENDFUNC

FUNC HandleBackspace()
    VAR pos
    IF CursorX > 1 THEN
        CursorX = CursorX - 1
        pos = FindPosition()
        ! Delete character at position
        IF pos < BufferSize THEN
            FOR i = pos TO BufferSize - 2
                TextBuffer[i] = TextBuffer[i + 1]
            NEXT i
            BufferSize = BufferSize - 1
            Modified = TRUE
            Redraw()
        ENDIF
    ENDIF
ENDFUNC

FUNC HandlePrintable(key)
    IF key >= 32 THEN
        IF key < 127 THEN
            InsertChar(CHR(key))
        ENDIF
    ENDIF
ENDFUNC

BEGIN
    ClearScreen()
    ShowStatus()
    ShowCursor()
    
    ! Main edit loop
    VAR key
    VAR done = FALSE
    
    WHILE NOT done
        key = GetKey()
        
        ! Handle each key type separately
        IF key = CTRLX THEN
            done = TRUE
        ENDIF
        
        IF key = CTRLS THEN
            SaveFile()
        ENDIF
        
        IF key = KEYUP THEN
            HandleUp()
        ENDIF
        
        IF key = KEYDOWN THEN
            HandleDown()
        ENDIF
        
        IF key = KEYLEFT THEN
            HandleLeft()
        ENDIF
        
        IF key = KEYRIGHT THEN
            HandleRight()
        ENDIF
        
        IF key = ENTER THEN
            InsertChar(CHR(10))
        ENDIF
        
        IF key = BACKSPACE THEN
            HandleBackspace()
        ENDIF
        
        ! Handle printable characters
        HandlePrintable(key)
    WEND
    
    ClearScreen()
    PRINT "Editor exited"
END
