NEW
CLS

CONST InWritePtr = 0x0A   ! Serial buffer write position
CONST InReadPtr  = 0x0B   ! Serial buffer read position
CONST InBuffer   = 0x0200 ! actual Serial input buffer

! Special key codes
CONST KEYUP    = 128
CONST KEYDOWN  = 129
CONST KEYRIGHT = 130
CONST KEYLEFT  = 131

! Screen dimensions (typical terminal)
CONST SCREENWIDTH = 80
CONST SCREENHEIGHT = 24

! State for escape sequence processing
VAR EscState = 0   ! 0=normal, 1=got ESC, 2=got ESC[

! Current position
VAR CurX = 40  ! Start in center
VAR CurY = 12

! Check if key available
FUNC KeyReady()
    RETURN PEEK(InReadPtr) <> PEEK(InWritePtr)
ENDFUNC

! Get raw character from buffer
FUNC GetRawKey()
    VAR IPTR
    VAR KEY
    WHILE NOT KeyReady()
        ! Wait for key
    WEND
    IPTR = PEEK(InReadPtr)
    KEY  = PEEK(InBuffer + IPTR)
    POKE(InReadPtr, (IPTR + 1) & 0xFF)
    RETURN KEY
ENDFUNC

! Get key with VT100 escape sequence interpretation
FUNC GetKey()
    VAR k
    
    k = GetRawKey()
    
    IF EscState = 0 THEN
        IF k = 0x1B THEN
            EscState = 1
            RETURN GetKey()
        ELSE
            RETURN k
        ENDIF
    ELSE
        IF EscState = 1 THEN
            IF k = 0x5B THEN
                EscState = 2
                RETURN GetKey()
            ELSE
                EscState = 0
                RETURN 0x1B
            ENDIF
        ELSE
            IF EscState = 2 THEN
                EscState = 0
                
                IF k = 0x41 THEN RETURN KEYUP ENDIF
                IF k = 0x42 THEN RETURN KEYDOWN ENDIF
                IF k = 0x43 THEN RETURN KEYRIGHT ENDIF
                IF k = 0x44 THEN RETURN KEYLEFT ENDIF
                
                RETURN k
            ENDIF
        ENDIF
    ENDIF
    
    RETURN k
ENDFUNC

! Send ESC sequence
FUNC SendEsc()
    PRINT CHR(0x1B);
ENDFUNC

! Clear screen
FUNC ClearScreen()
    SendEsc()
    PRINT "[2J";
    SendEsc()
    PRINT "[H";  ! Also home cursor
ENDFUNC

! Hide cursor
FUNC HideCursor()
    SendEsc()
    PRINT "[?25l";
ENDFUNC

! Show cursor
FUNC ShowCursor()
    SendEsc()
    PRINT "[?25h";
ENDFUNC

! Move cursor to position (1-based coordinates for VT100)
FUNC GotoXY(x, y)
    SendEsc()
    PRINT "["; y; ";"; x; "H";
ENDFUNC

! Move the star to a new position
FUNC MoveStar(newX, newY)
    ! Clear old position
    GotoXY(CurX, CurY)
    PRINT " ";
    
    ! Update position
    CurX = newX
    CurY = newY
    
    ! Draw at new position
    GotoXY(CurX, CurY)
    PRINT "*";
ENDFUNC

BEGIN
    VAR k
    
    ! Initialize screen
    ClearScreen()
    ! Hide the blinking cursor
    HideCursor()  
    
    ! Draw initial star at center
    GotoXY(CurX, CurY)
    PRINT "*";
    
    ! Main loop
    DO
        k = GetKey()
        
        IF k = KEYUP THEN
            IF CurY > 1 THEN
                MoveStar(CurX, CurY - 1)
            ENDIF
        ELSE
            IF k = KEYDOWN THEN
                IF CurY < SCREENHEIGHT THEN
                    MoveStar(CurX, CurY + 1)
                ENDIF
            ELSE
                IF k = KEYLEFT THEN
                    IF CurX > 1 THEN
                        MoveStar(CurX - 1, CurY)
                    ENDIF
                ELSE
                    IF k = KEYRIGHT THEN
                        IF CurX < SCREENWIDTH THEN
                            MoveStar(CurX + 1, CurY)
                        ENDIF
                    ENDIF
                ENDIF
            ENDIF
        ENDIF
        
    UNTIL k = 0x1B  ! ESC to exit
    
    ! Clean up
    ShowCursor() 
    ClearScreen()
    PRINT "Goodbye!"
END

