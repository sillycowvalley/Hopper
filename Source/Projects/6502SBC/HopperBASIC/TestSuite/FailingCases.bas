FUNC TestPrint()
    PRINT
    PRINT "string literal"
    PRINT 42
    PRINT -100
    PRINT TRUE
    PRINT FALSE
    PRINT 'X'
    ! Comma separator
    PRINT 1, 2, 3
    PRINT "a", "b", "c"
    ! Semicolon separator
    PRINT 1; 2; 3
    PRINT "a"; "b"; "c"
    ! Mixed
    PRINT "Value:", 42, "OK"
    PRINT "X="; 10; " Y="; 20
    ! Trailing comma
    PRINT "No newline",
    PRINT "continues"
    ! Trailing semicolon
    PRINT "No space";
    PRINT "tight"
    ! Variables
    VAR pv = 999
    VAR ps = "test"
    PRINT pv
    PRINT ps
    PRINT pv, ps
    PRINT "pv="; pv; " ps="; ps
ENDFUNC
TestPrint()
LIST

