! Minimal Failing Comparison Cases
! Type promotion fails outside common ranges

! Test 1: INT-WORD at max positive boundary
NEW
BEGIN
    INT i = 32767
    WORD w = 32767
    PRINT i = w ! Fails with TYPE MISMATCH
END
RUN

! Test 2: BYTE-WORD outside BYTE range  
NEW
BEGIN
    BYTE b = 255 ! Comment
    WORD w = 256
    PRINT "BYTE(255) <> WORD(256): "
    PRINT b <> w ! Fails with TYPE MISMATCH
END
RUN

! Test 3: BYTE-INT with negative INT
NEW
BEGIN
    BYTE b = 100
    INT i = -1
    PRINT "BYTE(100) <> INT(-1): "
    PRINT b <> i  ! Fails with TYPE MISMATCH
END
RUN

! Test 4: INT-WORD with negative INT
NEW
BEGIN
    INT i = -1
    WORD w = 0
    PRINT "INT(-1) <> WORD(0): "
    PRINT i <> w  ! Fails with TYPE MISMATCH
END
RUN


BEGIN
    WORD A = 20 ! comment works fine
END
RUN

NEW
CLS
BEGIN
    WORD A = 20
    PRINT 20 ! comment ok
    PRINT A
    PRINT ABS(-10)
    PRINT A ! comment munted
END
RUN

BEGIN
    WORD A = 20
    A = A * A : ! comment munted
END
RUN
BEGIN
    WORD A = 20
    A = A * 20 ! comment works fine
END
RUN
BEGIN
    WORD A = 20
    A = (A * A) ! comment works fine
END
RUN
BEGIN
    BIT B = FALSE OR TRUE ! comment works fine
END
RUN
BEGIN
    PRINT "HELLO" ! comment works fine
END
RUN


TRON
bit b[10]
begin
    b[0] = true
    print b[0]
end
