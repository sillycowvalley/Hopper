NEW

BEGIN
    WORD I
    LONG MS
    DO
        MS = MILLIS()
        PRINT MS
        DELAY(1000)
        I = I + 1
    UNTIL I > 100
END
RUN
DASM

