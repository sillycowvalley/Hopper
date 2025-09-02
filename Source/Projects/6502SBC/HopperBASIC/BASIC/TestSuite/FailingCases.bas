CLS
NEW
MEM

CHAR globCharArray[10] 
INT globIntArray[10]

BEGIN
    FOR i = 0 TO 9
        globCharArray[i] = 'A'
        globIntArray[i] = i - 5
    NEXT i
END
RUN
