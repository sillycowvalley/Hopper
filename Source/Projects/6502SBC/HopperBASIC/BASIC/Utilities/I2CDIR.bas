CONST EEPROM = 80       ! I2C address
CONST DIRADDR = 256     ! Directory at sector 1
CONST ENTRIES = 16      ! 16 directory entries
VAR files
VAR bytes
VAR bufpos
BYTE buffer[256]

FUNC Hex(val)
    VAR h = val / 16
    VAR l = val & 15
    VAR c
    
    IF h < 10 THEN
        c = h + ASC('0')
    ELSE
        c = h - 10 + ASC('A')
    ENDIF
    PRINT CHR(c);
    
    IF l < 10 THEN
        c = l + ASC('0')
    ELSE
        c = l - 10 + ASC('A')
    ENDIF
    PRINT CHR(c);
ENDFUNC

FUNC DumpBuffer()
    VAR idx
    PRINT "Buffer contents (256 bytes):"
    FOR row = 0 TO 15
        ! Print address
        Hex(row * 16)
        PRINT ": ";
        
        ! Print 16 hex bytes
        FOR col = 0 TO 15
            idx = (row * 16) + col
            Hex(buffer[idx])
            PRINT " ";
        NEXT col
        PRINT
    NEXT row
ENDFUNC

FUNC ReadSector(sector)
    VAR bytes
    VAR result
    
    I2CBEGIN(EEPROM)
    I2CPUT(sector)
    I2CPUT(0)
    result = I2CEND()
    
    DELAY(5) ! wait 5ms for EEPROM..
    bytes = I2CGET(EEPROM, 128)
    FOR i = 0 TO 127
        buffer[i] = I2CNEXT()
    NEXT I
    
    I2CBEGIN(EEPROM)
    I2CPUT(sector)
    I2CPUT(128)
    result = I2CEND()
    
    DELAY(5) ! wait 5ms for EEPROM..
    bytes = I2CGET(EEPROM, 128)
    FOR i = 128 TO 255
        buffer[i] = I2CNEXT()
    NEXT I
ENDFUNC

FUNC GetByte()
    VAR b = buffer[bufpos]
    bufpos = bufpos + 1
    RETURN b
ENDFUNC

FUNC GetSize()
    VAR lo = GetByte()
    VAR hi = GetByte() & 0x7F
    VAR size = lo + (hi * 256)
    
    IF size > 0 THEN
        files = files + 1
        bytes = bytes + size
    ENDIF
    
    RETURN size
ENDFUNC

FUNC GetSector()
    RETURN GetByte()
ENDFUNC

FUNC Name()
    VAR length
    VAR c
    FOR i = 1 TO 13
        c = GetByte()
        IF c = 0 THEN
            RETURN length
        ENDIF
        IF c > 127 THEN
            PRINT CHR(c & 127);
            RETURN length + 1
        ELSE
            PRINT CHR(c);
            length = length + 1
        ENDIF
    NEXT i
    RETURN length
ENDFUNC

FUNC Skip()
    FOR i = 1 TO 13
        bufpos = bufpos + 1  ! Just advance the pointer
    NEXT i
ENDFUNC

FUNC PadSpaces(size)
    PRINT "  ";
    IF size < 10000 THEN
        PRINT " ";
    ENDIF
    IF size < 1000 THEN
        PRINT " ";
    ENDIF
    IF size < 100 THEN
        PRINT " ";
    ENDIF
    IF size < 10 THEN
        PRINT " ";
    ENDIF
    PRINT size; " bytes";
ENDFUNC

FUNC Entry(number)
    VAR size
    VAR sector
    VAR length
    
    ! Set buffer position to start of this entry
    bufpos = number * 16
    
    size = GetSize()
    sector = GetSector()
    
    IF size = 0 THEN
        Skip()
        RETURN
    ENDIF
    
    IF number < 10 THEN
        PRINT " ";
    ENDIF
    PRINT number; ": ";
    
    length = Name()
    FOR i = length TO 12
        PRINT " ";
    NEXT i
    
    PadSpaces(size)
    
    PRINT "  (0x";
    Hex(sector)
    PRINT ")"
ENDFUNC

FUNC Directory()
    FOR i = 0 TO ENTRIES - 1
        Entry(i)
    NEXT i
ENDFUNC

FUNC Summary()
    PRINT "------------------------"
    PRINT files; " file(s), "; bytes; " bytes used"
    
    VAR free = (254 * 256) - bytes
    PRINT free; " bytes free"
ENDFUNC

BEGIN
    PRINT "EEPROM Directory"
    PRINT "================"
    ReadSector(1) ! sector 1 is directory
    ! DumpBuffer()
    Directory()
    Summary()
END
