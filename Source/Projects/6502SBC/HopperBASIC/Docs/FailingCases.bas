! Remaining CHAR Comparison Test Failures
! Only Tests 10 and 12 which still have SYNTAX ERRORs

! ===== TEST 10: Complex Expression Comparisons =====
! Fails at: PRINT "Complex Expression Tests:"
NEW
FUNC TestComplexExpr()
    CHAR c = 'G'
    BYTE b = 70  ! 'F'
    PRINT "Complex Expression Tests:"
    PRINT c > CHR(b); " ! expect TRUE (G > F)"
    PRINT ASC(c) > b; " ! expect TRUE (71 > 70)"
    ! Chained comparison
    CHAR c1 = 'B'
    CHAR c2 = 'D'
    PRINT c1 < c AND c < c2; " ! expect FALSE (B<G AND G<D)"
    CHAR c3 = 'A'
    CHAR c4 = 'Z'
    PRINT c3 < c AND c < c4; " ! expect TRUE (A<G AND G<Z)"
ENDFUNC
BEGIN
    TestComplexExpr()
END
RUN

! ===== TEST 12: String Indexing with Comparisons =====
! Fails at: STRING s = "HELLO"
NEW
FUNC TestStringIndex()
    STRING s = "HELLO"
    CHAR c1 = s[0]  ! 'H'
    CHAR c2 = s[1]  ! 'E'
    CHAR c3 = s[4]  ! 'O'
    PRINT "String Index Comparison:"
    PRINT c2 < c1; " ! expect TRUE (E < H)"
    PRINT c3 > c1; " ! expect TRUE (O > H)"
    PRINT c1 <= s[0]; " ! expect TRUE (H <= H)"
    PRINT s[2] = s[3]; " ! expect TRUE (L = L)"
ENDFUNC
BEGIN
    TestStringIndex()
END
RUN