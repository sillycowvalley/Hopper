CONST InWritePtr = 0x0A   ! Serial buffer write position
CONST InReadPtr  = 0x0B   ! Serial buffer read position
CONST InBuffer   = 0x0200 ! actual Serial input buffer

! Special key codes we'll return
CONST KEYUP    = 128
CONST KEYDOWN  = 129
CONST KEYRIGHT = 130
CONST KEYLEFT  = 131

! State for escape sequence processing
VAR EscState = 0   ! 0=normal, 1=got ESC, 2=got ESC[
VAR EscBuffer = 0  ! Store the pending character

! Check if key available
FUNC KeyReady()
    RETURN PEEK(InReadPtr) <> PEEK(InWritePtr)
ENDFUNC

! Get raw character from buffer (no interpretation)
FUNC GetRawKey()
    VAR IPTR
    VAR KEY
    WHILE NOT KeyReady()
        ! Could yield to other processing here
    WEND
    IPTR = PEEK(InReadPtr)
    KEY  = PEEK(InBuffer + IPTR)
    POKE(InReadPtr, (IPTR + 1) & 0xFF)
    RETURN KEY
ENDFUNC

! Get key with VT100 escape sequence interpretation using state machine
FUNC GetKey()
    VAR k
    
    k = GetRawKey()
    
    ! State machine for escape sequences
    IF EscState = 0 THEN
        ! Normal state
        IF k = 0x1B THEN
            EscState = 1  ! Got ESC, wait for next char
            RETURN GetKey()  ! Recurse to get next char immediately
        ELSE
            RETURN k  ! Normal character
        ENDIF
    ELSE
        IF EscState = 1 THEN
            ! We previously got ESC
            IF k = 0x5B THEN  ! '[' character  
                EscState = 2  ! Got ESC[, wait for final char
                RETURN GetKey()  ! Recurse to get next char
            ELSE
                ! Not an arrow sequence, return the ESC we saved
                EscState = 0
                EscBuffer = k  ! Save this char for next call
                RETURN 0x1B  ! Return the ESC
            ENDIF
        ELSE
            IF EscState = 2 THEN
                ! We previously got ESC[
                EscState = 0  ! Reset state
                
                ! Decode arrow keys
                IF k = 0x41 THEN RETURN KEYUP ENDIF     ! 'A'
                IF k = 0x42 THEN RETURN KEYDOWN ENDIF   ! 'B'  
                IF k = 0x43 THEN RETURN KEYRIGHT ENDIF  ! 'C'
                IF k = 0x44 THEN RETURN KEYLEFT ENDIF   ! 'D'
                
                ! Unknown escape sequence, just return the char
                RETURN k
            ENDIF
        ENDIF
    ENDIF
    
    RETURN k
ENDFUNC

FUNC InKey()
    IF KeyReady() THEN
        RETURN GetKey()
    ELSE
        RETURN 0
    ENDIF
ENDFUNC

BEGIN
    VAR k
    PRINT "Arrow key test - Press 'q' to quit"
    PRINT "Try arrow keys and other keys:"
    PRINT
    
    DO
        k = GetKey()
        IF k <> 0 THEN
            IF k = KEYUP THEN
                PRINT "UP ARROW"
            ELSE 
                IF k = KEYDOWN THEN
                    PRINT "DOWN ARROW"
                ELSE
                    IF k = KEYRIGHT THEN
                        PRINT "RIGHT ARROW"
                    ELSE
                        IF k = KEYLEFT THEN
                            PRINT "LEFT ARROW"
                        ELSE
                            IF k = 0x1B THEN
                                PRINT "ESC key"
                            ELSE
                                IF k < 32 THEN
                                    PRINT "Control char: ", k
                                ELSE
                                    PRINT "You pressed: '"; CHR(k); "' (", k, ")"
                                ENDIF
                            ENDIF
                        ENDIF
                    ENDIF
                ENDIF
            ENDIF
        ENDIF
    UNTIL (CHR(k) = 'q') OR (CHR(k) = 'Q')
    
    PRINT "Goodbye!"
END

