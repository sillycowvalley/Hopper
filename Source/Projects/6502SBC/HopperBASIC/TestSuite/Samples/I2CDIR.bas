CONST EEPROM = 80       ! I2C address
CONST DIRADDR = 256     ! Directory at sector 1
CONST ENTRIES = 16      ! 16 directory entries

VAR files = 0
VAR bytes = 0

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

FUNC ReadEEPROM(addr)
    VAR bytes
    VAR result
    I2CBEGIN(EEPROM)
    I2CPUT(addr / 256)
    I2CPUT(addr & 255)
    result = I2CEND()
    IF NOT result THEN
print "a";
        ! RETURN FALSE
    ENDIF
    DELAY(5) ! wait 5ms for EEPROM..
    bytes = I2CGET(EEPROM, 16)

    IF bytes <> 16 THEN
print "c";
print bytes
        !RETURN FALSE
    ENDIF
    RETURN TRUE
ENDFUNC

FUNC GetSize()
    VAR lo = I2CNEXT()
    VAR hi = I2CNEXT()
    VAR size = lo + (hi * 256)
    
    IF size > 0 THEN
        files = files + 1
        bytes = bytes + size
    ENDIF
    
    RETURN size
ENDFUNC

FUNC GetSector()
    RETURN I2CNEXT()
ENDFUNC

FUNC Name()
    VAR length
    VAR c
    FOR i = 1 TO 13
        c = I2CNEXT()
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
        I2CNEXT()
    NEXT i
ENDFUNC

FUNC PadSpaces(size)
    PRINT "  ";
    IF size < 10000 THEN PRINT " " ENDIF
    IF size < 1000 THEN PRINT " " ENDIF
    IF size < 100 THEN PRINT " " ENDIF
    IF size < 10 THEN PRINT " " ENDIF
    PRINT size; " bytes";
ENDFUNC

FUNC Entry(number)
    VAR addr = DIRADDR + (number * 16)
    VAR size
    VAR sector
    VAR length
    IF NOT ReadEEPROM(addr) THEN
        PRINT "Read error"
        RETURN
    ENDIF
    
    size = GetSize()
    sector = GetSector()
    
    IF size = 0 THEN
        Skip()
        RETURN
    ENDIF
    
    IF number < 10 THEN PRINT " " ENDIF
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
    PRINT
    PRINT "------------------------"
    PRINT files; " file(s), "; bytes; " bytes used"
    
    VAR free = (254 * 256) - bytes
    PRINT free; " bytes free"
ENDFUNC

BEGIN
    PRINT "EEPROM Directory"
    PRINT "================"
    PRINT
    
    Directory()
    Summary()
END

