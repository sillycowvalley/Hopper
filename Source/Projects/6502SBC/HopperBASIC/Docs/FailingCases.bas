! Test: Exclamation marks in string literals

NEW
BEGIN
    PRINT "Simple test ! with exclamation"
END
RUN

NEW
BEGIN
    PRINT "Test 1 ! first"
    PRINT "Test 2 ! second"
    PRINT "Test 3 ! third"
    PRINT "Test 4 ! fourth"
END  
RUN

NEW
BEGIN
    INT i = 100
    PRINT "i="; i; " ! expect 100"
END
RUN

NEW
BEGIN
    INT i = 100
    WORD w = 200
    PRINT "i="; i; " ! expect 100"
    PRINT "w="; w; " ! expect 200"
END
RUN

! Try without the exclamation mark
NEW
BEGIN
    INT i = 100
    WORD w = 200
    PRINT "i="; i; " expect 100"
    PRINT "w="; w; " expect 200"
END
RUN