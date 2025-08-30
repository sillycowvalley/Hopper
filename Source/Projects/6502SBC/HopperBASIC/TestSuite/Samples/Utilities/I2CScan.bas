NEW

FUNC PRINTHEX(value)
    VAR high = value / 16
    VAR low = value & 15
    VAR ch
    
    IF high < 10 THEN
        ch = high + ASC('0')
    ELSE
        ch = high - 10 + ASC('A')
    ENDIF
    PRINT CHR(ch);
    
    IF low < 10 THEN
        ch = low + ASC('0')
    ELSE
        ch = low - 10 + ASC('A')
    ENDIF
    PRINT CHR(ch);
ENDFUNC

BEGIN
    PRINT "Scanning I2C bus..."
    VAR found = 0
    FOR addr = 8 TO 119
        IF I2CFIND(addr) THEN
            PRINT "Device at address 0x"; 
            PRINTHEX(addr)
            PRINT
            found = found + 1
        ENDIF
    NEXT addr
    PRINT found, "devices found"
END
RUN
