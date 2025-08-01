REM Failing STRING Tests for HopperBASIC - Only cases that need fixes

REM Define test variables
int pos1 = 1
word w1 = 1

REM Test variables for string operations  
string hello = "hello"
string world = "world"
string empty = ""

REM ISSUE 1: Reserved keywords can't be variable names (EXPECTED BEHAVIOR)
REM bit true = (1 = 1)     REM FAILS: ILLEGAL IDENTIFIER - 'true' is reserved
REM bit false = (1 = 0)    REM FAILS: ILLEGAL IDENTIFIER - 'false' is reserved

REM ISSUE 2: Wrong string literal comparisons
print "hello" = hello      REM Returns FALSE - should return TRUE (same content)
print "hello" = hello2     REM Returns FALSE - should return TRUE (same content)  
print "" = empty           REM Returns FALSE - should return TRUE (both empty)
print "123" = number       REM Returns TRUE - should return TRUE (WORKS)
print hello = "hello"      REM Returns TRUE - should return TRUE (WORKS)
print empty = ""           REM Returns TRUE - should return TRUE (WORKS)

REM ISSUE 3: Cross-type comparisons with wrong error messages
REM These correctly show TYPE MISMATCH for = and <> (WORKS)

REM ISSUE 4: Ordering operations show wrong error - should be INVALID OPERATOR
print hello < world        REM Shows UNDEFINED IDENTIFIER - should show INVALID OPERATOR
print hello > world        REM Shows UNDEFINED IDENTIFIER - should show INVALID OPERATOR
print hello <= world       REM Shows UNDEFINED IDENTIFIER - should show INVALID OPERATOR
print hello >= world       REM Shows UNDEFINED IDENTIFIER - should show INVALID OPERATOR

print hello < pos1         REM Shows UNDEFINED IDENTIFIER - should show INVALID OPERATOR
print hello > pos1         REM Shows UNDEFINED IDENTIFIER - should show INVALID OPERATOR
print hello <= pos1        REM Shows UNDEFINED IDENTIFIER - should show INVALID OPERATOR
print hello >= pos1        REM Shows UNDEFINED IDENTIFIER - should show INVALID OPERATOR

print pos1 < hello         REM Shows UNDEFINED IDENTIFIER - should show INVALID OPERATOR
print pos1 > hello         REM Shows UNDEFINED IDENTIFIER - should show INVALID OPERATOR
print pos1 <= hello        REM Shows UNDEFINED IDENTIFIER - should show INVALID OPERATOR
print pos1 >= hello        REM Shows UNDEFINED IDENTIFIER - should show INVALID OPERATOR

print hello < w1           REM Shows UNDEFINED IDENTIFIER - should show INVALID OPERATOR
print hello > w1           REM Shows UNDEFINED IDENTIFIER - should show INVALID OPERATOR
print hello <= w1          REM Shows UNDEFINED IDENTIFIER - should show INVALID OPERATOR
print hello >= w1          REM Shows UNDEFINED IDENTIFIER - should show INVALID OPERATOR

print w1 < hello           REM Shows UNDEFINED IDENTIFIER - should show INVALID OPERATOR
print w1 > hello           REM Shows UNDEFINED IDENTIFIER - should show INVALID OPERATOR
print w1 <= hello          REM Shows UNDEFINED IDENTIFIER - should show INVALID OPERATOR
print w1 >= hello          REM Shows UNDEFINED IDENTIFIER - should show INVALID OPERATOR

REM BIT ordering operations should also show INVALID OPERATOR
bit flag1 = TRUE
bit flag2 = FALSE
print flag1 < flag2        REM Should show INVALID OPERATOR  
print flag1 >= flag2       REM Should show INVALID OPERATOR

REM ISSUE 5: String ordering operations work on literals (unexpected behavior)
REM These actually work but should probably show TYPE MISMATCH:
print "hello" < "world"    REM Returns TRUE - should this error?
print "hello" > "world"    REM Returns FALSE - should this error?
print "hello" <= "world"   REM Returns TRUE - should this error?
print "hello" >= "world"   REM Returns FALSE - should this error?
