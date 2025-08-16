CLS
NEW
CONST WORD sizepl = 5
BIT flags[sizepl*2]

WORD I
BEGIN
    FOR i = 0 TO (sizepl*2)-1
        flags[i] = TRUE
    NEXT i
END

RUN
DASM

