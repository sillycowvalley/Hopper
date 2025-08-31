NEW
BEGIN
    PINMODE(0, 1)
    VAR P
    WHILE TRUE
        P = NOT READ(1)
        PRINT P
        IF P THEN
            WRITE(0, TRUE)
        ELSE
            WRITE(0, FALSE)
        ENDIF
        DELAY(300)
    WEND
END
RUN
