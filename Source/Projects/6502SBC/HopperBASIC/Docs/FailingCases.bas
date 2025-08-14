! Failing Comparison Tests - Isolated for Debugging
! These tests either crash or fail unexpectedly

! ===== FAILURE 1: INT-WORD Comparison at 32767 =====
! Unexpected TYPE MISMATCH when both values are 32767
NEW
FUNC TestIntWord32767()
    INT i = 32767
    WORD w = 32767
    PRINT "INT-WORD at 32767:"
    PRINT "i="; i; " w="; w
    PRINT "Comparing i = w..."
    PRINT i = w; " ! expect TRUE but got TYPE MISMATCH"
ENDFUNC
BEGIN
    TestIntWord32767()
END
RUN

! ===== FAILURE 2: BYTE-WORD Outside Safe Range =====
! TYPE MISMATCH when WORD exceeds BYTE range
NEW
FUNC TestByteWord256()
    BYTE b = 255
    WORD w = 256
    PRINT "BYTE-WORD at boundary:"
    PRINT "b="; b; " w="; w
    PRINT "Comparing b <> w..."
    PRINT b <> w; " ! expect TRUE but got TYPE MISMATCH"
ENDFUNC
BEGIN
    TestByteWord256()
END
RUN

! ===== FAILURE 3: BYTE-INT with Negative INT =====
! TYPE MISMATCH when INT is negative
NEW
FUNC TestByteIntNegative()
    BYTE b = 255
    INT i = -1
    PRINT "BYTE-INT with negative:"
    PRINT "b="; b; " i="; i
    PRINT "Comparing b <> i..."
    PRINT b <> i; " ! expect TRUE but got TYPE MISMATCH"
ENDFUNC
BEGIN
    TestByteIntNegative()
END
RUN

! ===== FAILURE 4: INT-WORD with Negative INT =====
! Unexpected behavior - some comparisons work, others don't
NEW
FUNC TestIntWordNegative()
    INT i = -32768
    WORD w = 32768
    PRINT "Test 1: i=-32768, w=32768"
    PRINT i <> w; " ! This actually worked!"
    
    i = -1
    w = 0
    PRINT "Test 2: i=-1, w=0"
    PRINT i <> w; " ! expect TRUE but got TYPE MISMATCH"
ENDFUNC
BEGIN
    TestIntWordNegative()
END
RUN

! ===== CRASH 1: STRING Comparisons =====
! This entire function crashes on RUN
NEW
FUNC TestStringCrash()
    STRING s1 = "HELLO"
    STRING s2 = "WORLD"
    STRING s3 = "HELLO"
    STRING s4 = ""
    PRINT "STRING Tests:"
    PRINT s1 = s3; " ! expect TRUE"
    PRINT s1 = "HELLO"; " ! expect TRUE"
    PRINT s1 <> s2; " ! expect TRUE"
    PRINT s4 = ""; " ! expect TRUE"
    PRINT s4 <> "X"; " ! expect TRUE"
ENDFUNC
BEGIN
    TestStringCrash()
END
! RUN causes crash - commented out
! RUN

! ===== CRASH 2: STRING = INT Comparison =====
! Crashes instead of giving TYPE MISMATCH
NEW
FUNC TestStringIntCrash()
    STRING s = "HELLO"
    INT i = 5
    PRINT "STRING = INT: "; s = i
ENDFUNC
BEGIN
    PRINT "Testing STRING = INT:"
    TestStringIntCrash()
END
! RUN causes crash - commented out
! RUN

! ===== CRASH 3: CHAR = STRING Comparison =====
! Crashes instead of giving TYPE MISMATCH
NEW
FUNC TestCharStringCrash()
    CHAR c = 'A'
    STRING s = "A"
    PRINT "CHAR = STRING: "; c = s
ENDFUNC
BEGIN
    PRINT "Testing CHAR = STRING:"
    TestCharStringCrash()
END
! RUN causes crash - commented out
! RUN

! ===== Minimal Reproduction Cases =====

! Minimal INT-WORD failure
NEW
BEGIN
    INT i = 32767
    WORD w = 32767
    PRINT i = w
END
RUN

! Minimal STRING crash
NEW
BEGIN
    STRING s1 = "A"
    STRING s2 = "A"
    PRINT s1 = s2
END
RUN

! Minimal STRING-INT crash
NEW
BEGIN
    STRING s = "X"
    INT i = 1
    PRINT s = i
END
! RUN causes crash - commented out
! RUN

