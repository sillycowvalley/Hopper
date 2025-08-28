NEW

FUNC PRINTHEX(B)
    
ENDFUNC

BEGIN
    PRINT "Scanning I2C bus..."
    VAR found = 0
    FOR addr = 8 TO 119
        IF I2CFIND(addr) THEN
            PRINT "Device at address", PRINTHEX(addr)
            found = found + 1
        ENDIF
    NEXT addr
    PRINT found, "devices found"
END
RUN
