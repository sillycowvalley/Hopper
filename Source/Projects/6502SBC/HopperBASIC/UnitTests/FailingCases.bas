REM Actual Failing Tests - Based on emulator results

REM Define test variables first (these work)
string hello = "hello"
VARS
string world = "world" 
VARS
string empty = ""
VARS

REM ISSUE 2: String comparison asymmetry and wrong results
print hello                : REM Works: outputs "HELLO" 
print hello = "world"      : REM Works but wrong result: returns TRUE - should return FALSE
print "world" = hello      : REM Fails: UNDEFINED IDENTIFIER - should return FALSE
print "hello" = hello      : REM Fails: UNDEFINED IDENTIFIER - should return TRUE
print "" = empty           : REM Fails: UNDEFINED IDENTIFIER - should return TRUE

REM ISSUE 4: All variable ordering operations show UNDEFINED IDENTIFIER instead of INVALID OPERATOR
print hello < world        : REM Currently: UNDEFINED IDENTIFIER - should show INVALID OPERATOR
print hello > world        : REM Currently: UNDEFINED IDENTIFIER - should show INVALID OPERATOR
print hello <= world       : REM Currently: UNDEFINED IDENTIFIER - should show INVALID OPERATOR
print hello >= world       : REM Currently: UNDEFINED IDENTIFIER - should show INVALID OPERATOR

int pos1 = 10

print hello < pos1         : REM Currently: UNDEFINED IDENTIFIER - should show INVALID OPERATOR
print hello <= pos1        : REM Currently: UNDEFINED IDENTIFIER - should show INVALID OPERATOR

REM BIT ordering operations also show UNDEFINED IDENTIFIER instead of INVALID OPERATOR
bit flag1 = TRUE
bit flag2 = FALSE
print flag1 < flag2        : REM Currently: UNDEFINED IDENTIFIER - should show INVALID OPERATOR
print flag1 >= flag2       : REM Currently: UNDEFINED IDENTIFIER - should show INVALID OPERATOR

REM WORKING CORRECTLY: String literal ordering shows proper INVALID OPERATOR
print "hello" < "world"    : REM Currently: INVALID OPERATOR (CORRECT behavior)

