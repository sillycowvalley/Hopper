FUNC GetValue()
    RETURN 42
ENDFUNC

FUNC DoWork()
    PRINT "working"
    RETURN
ENDFUNC

INT result = GetValue()
PRINT result

WORD wresult = GetValue()
PRINT wresult

BIT bresult = GetValue()
PRINT bresult

INT voidresult = DoWork()
PRINT voidresult

GetValue()

DoWork()

PRINT GetValue()

PRINT DoWork()

PRINT GetValue() + 10

PRINT DoWork() + 10

IF GetValue() THEN PRINT "true"

IF DoWork() THEN PRINT "true"

PRINT GetValue() = 42

PRINT DoWork() = 0
