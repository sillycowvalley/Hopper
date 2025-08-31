CONST InWritePtr = 0x0A   ! Serial buffer write position
CONST InReadPtr  = 0x0B   ! Serial buffer read position
CONST InBuffer   = 0x0200 ! actual Serial input buffer

! Check if key available
FUNC KeyReady()
    ! If read != write, we have characters
    RETURN PEEK(InReadPtr) <> PEEK(InWritePtr)
ENDFUNC

FUNC GetKey()
    VAR RPTR
    VAR IPTR
    VAR KEY
    WHILE NOT KeyReady()
        DELAY(5) ! 5ms
    WEND
    IPTR = PEEK(InReadPtr)
    KEY  = PEEK(InBuffer + IPTR)
    POKE(InReadPtr, (IPTR + 1) & 0xFF)
    RETURN KEY
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
    DO
        k = GETKEY()
        IF k <> 0 THEN
            PRINT "You pressed: "; CHR(k), k
        ENDIF
    UNTIL (CHR(k) = 'q') OR (CHR(k) = 'Q')
END
