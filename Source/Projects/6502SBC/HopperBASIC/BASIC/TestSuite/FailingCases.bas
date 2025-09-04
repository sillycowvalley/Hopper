NEW
BEGIN
    PRINT 100 / A
END
RUN


NEW
VAR A
BEGIN
    PRINT 100 / A
END
RUN





NEW
func fizzbuzz()
    INT number
    for number = 1 to 100
        PRINT number,
    next number
endfunc

begin
    fizzbuzz()
end
RUN


BEGIN
    IF 0 = 0 THEN ! comment
    ENDIF
END

VAR bufferAddress = PEEK(0xAA) * 256
VAR bufferSize    = PEEK(0xAB) * 256
VARS
