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
    PRINT A ! comment munted
    A = A * A ! comment works fine
    PRINT A ! comment munted
END
RUN
