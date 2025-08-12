! BIT Type Test Suite - Modular Version

FUNC TestDeclaration()
    PRINT "Test 1: Declaration"
    BIT flag1
    BIT flag2 = TRUE
    BIT flag3 = FALSE
    PRINT "flag2="; flag2; " (expect TRUE)"
    PRINT "flag3="; flag3; " (expect FALSE)"
    flag1 = TRUE
    PRINT "flag1="; flag1; " (expect TRUE)"
ENDFUNC

FUNC TestLogical()
    PRINT "Test 2: Logical ops"
    BIT a = TRUE
    BIT b = FALSE
    PRINT "T AND F="; a AND b; " (expect FALSE)"
    PRINT "T OR F="; a OR b; " (expect TRUE)"
    PRINT "NOT T="; NOT a; " (expect FALSE)"
    PRINT "NOT F="; NOT b; " (expect TRUE)"
ENDFUNC

FUNC TestComparison()
    PRINT "Test 3: Comparisons"
    INT x = 5
    INT y = 10
    BIT cmp
    cmp = x < y
    PRINT "5<10="; cmp; " (expect TRUE)"
    cmp = x > y
    PRINT "5>10="; cmp; " (expect FALSE)"
    cmp = x = 5
    PRINT "5=5="; cmp; " (expect TRUE)"
ENDFUNC

FUNC TestConstants()
    PRINT "Test 4: Constants"
    CONST BIT yes = TRUE
    CONST BIT no = FALSE
    PRINT "yes="; yes; " (expect TRUE)"
    PRINT "no="; no; " (expect FALSE)"
    BIT test = yes AND no
    PRINT "yes AND no="; test; " (expect FALSE)"
ENDFUNC

FUNC TestComplex()
    PRINT "Test 5: Complex"
    BIT p = TRUE
    BIT q = FALSE
    BIT r = TRUE
    BIT result
    result = (p AND q) OR (NOT q AND r)
    PRINT "(T&F)|(~F&T)="; result; " (expect TRUE)"
    result = p AND (q OR r)
    PRINT "T&(F|T)="; result; " (expect TRUE)"
ENDFUNC

FUNC TestControlFlow()
    PRINT "Test 6: Control flow"
    BIT flag = TRUE
    IF flag THEN
        PRINT "flag was TRUE"
    ELSE
        PRINT "flag was FALSE"
    ENDIF
    INT x = 5
    IF x > 3 THEN
        PRINT "5>3 is TRUE"
    ENDIF
ENDFUNC

FUNC TestLoop()
    PRINT "Test 7: Loop with BIT"
    BIT go = TRUE
    INT n = 0
    WHILE go
        n = n + 1
        PRINT "Loop "; n
        IF n >= 3 THEN go = FALSE ENDIF
    WEND
ENDFUNC

FUNC TestEdgeCases()
    PRINT "Test 8: Edge cases"
    BIT toggle = FALSE
    toggle = NOT toggle
    PRINT "NOT F="; toggle; " (expect TRUE)"
    toggle = NOT toggle
    PRINT "NOT T="; toggle; " (expect FALSE)"
    BIT x = NOT NOT TRUE
    PRINT "NOT NOT T="; x; " (expect TRUE)"
ENDFUNC

BEGIN
    PRINT "=== BIT Test Suite ==="
    TestDeclaration()
    PRINT
    TestLogical()
    PRINT
    TestComparison()
    PRINT
    TestConstants()
    PRINT
    TestComplex()
    PRINT
    TestControlFlow()
    PRINT
    TestLoop()
    PRINT
    TestEdgeCases()
    PRINT "=== Tests Complete ==="
END

RUN

