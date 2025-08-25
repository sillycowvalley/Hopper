NEW
! Only the 2 remaining failures

! Failure 1: Parameter modification
FUNC TestParam(value)
    PRINT "param="; value
    value = 777
    PRINT "modified="; value
ENDFUNC

VAR original = 123

BEGIN
    PRINT "Parameter modification test:"
    TestParam(original)
END
RUN




NEW
! Failure 2: Array parameters
BIT flags[3]
CHAR letters[3]
INT numbers[3]

FUNC ModifyArrays(f, l, n)
    f[0] = TRUE
    PRINT "Modified"
ENDFUNC

BEGIN
    PRINT "Array parameter test:"
    ModifyArrays(flags, letters, numbers)
END
RUN




NEW
! Issue 1: Uninitialized defaults wrong
VAR uninit

BEGIN
    PRINT "uninit="; uninit; " expect 0"
END
RUN



NEW
! Issue 2: CONST string problem
CONST message = "HELLO"

BEGIN
    PRINT message
END
RUN



NEW
! Issue 3: Array type compatibility
INT numbers[3]

BEGIN
    numbers[0] = 999
    PRINT numbers[0]
END
RUN
