FUNC HexNibble(n)
    IF n < 10 THEN
        RETURN CHR(n + 48)  ! '0'-'9'
    ELSE
        RETURN CHR(n + 55)  ! 'A'-'F'
    ENDIF
ENDFUNC

FUNC HexByte(b)
    PRINT HexNibble(b / 16);
    PRINT HexNibble(b MOD 16);
ENDFUNC

FUNC HexDump(page)
    VAR baseAddr = page * 256
    VAR addr
    VAR b
    VAR row
    VAR col
    
    FOR row = 0 TO 15
        addr = baseAddr + (row * 16)
        
        ! Print address
        HexByte(addr / 256)
        HexByte(addr MOD 256)
        PRINT ": ";
        
        ! Print hex bytes
        FOR col = 0 TO 15
            b = PEEK(addr + col)
            HexByte(b)
            PRINT " ";
            IF col = 7 THEN
                PRINT " ";
            ENDIF
        NEXT col
        
        PRINT "  ";
        
        ! Print ASCII
        FOR col = 0 TO 15
            b = PEEK(addr + col)
            IF (b >= 32) AND (b <= 126) THEN
                PRINT CHR(b);
            ELSE
                PRINT ".";
            ENDIF
        NEXT col
        
        PRINT ""
    NEXT row
ENDFUNC

! Example usage:
! HexDump(0)    ! Dumps page 0 (addresses 0x0000-0x00FF)
! HexDump(1)    ! Dumps page 1 (addresses 0x0100-0x01FF)
! HexDump(2)    ! Dumps page 2 (addresses 0x0200-0x02FF)

