!CLS
!NEW

CONST WORD size = 5

BIT B[size*2]
WORD W[size]
!INT I[30]
!CHAR C[20]
!BYTE BB[40]

!CONST WORD size = 10

!TRON
BEGIN
    FOR I = 0 to LEN(W)-1
        W[I] = I
    NEXT I
    FOR I = 0 to 9
        PRINT W[I],
    NEXT I
END
VARS

RUN

