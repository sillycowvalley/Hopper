NEW
! Test INT re-initialization
INT i = -100
INT i2

! Modify before RUN
i = 999
i2 = -5555

BEGIN
    PRINT "=== INT Test ==="
    PRINT "i="; i; " ! expect -100"
    PRINT "i2="; i2; " ! expect 0"
    ! Now modify inside
    i = 777
    i2 = -999
    PRINT "Modified to 777 and -999"
END

RUN

NEW
! Test WORD re-initialization  
WORD w = 5000
WORD w2

! Modify before RUN
w = 12345
w2 = 65535

BEGIN
    PRINT "=== WORD Test ==="
    PRINT "w="; w; " ! expect 5000"
    PRINT "w2="; w2; " ! expect 0"
    ! Now modify inside
    w = 30000
    w2 = 40000
    PRINT "Modified to 30000 and 40000"
END

RUN

NEW
! Test BIT re-initialization
BIT b = TRUE
BIT b2

! Modify before RUN
b = FALSE
b2 = TRUE

BEGIN
    PRINT "=== BIT Test ==="
    PRINT "b="; b; " ! expect TRUE"
    PRINT "b2="; b2; " ! expect FALSE"
    ! Now modify inside
    b = FALSE
    b2 = TRUE
    PRINT "Modified to FALSE and TRUE"
END

RUN

NEW
! Test BYTE re-initialization
BYTE by = 255
BYTE by2

! Modify before RUN
by = 128
by2 = 200

BEGIN
    PRINT "=== BYTE Test ==="
    PRINT "by="; by; " ! expect 255"
    PRINT "by2="; by2; " ! expect 0"
    ! Now modify inside
    by = 100
    by2 = 150
    PRINT "Modified to 100 and 150"
END

RUN

NEW
! Test CHAR re-initialization
CHAR c = 'A'
CHAR c2

! Modify before RUN
c = 'Z'
c2 = 'Q'

BEGIN
    PRINT "=== CHAR Test ==="
    PRINT "c="; c; " ! expect A"
    PRINT "c2="; c2; " ! expect NUL/space"
    ! Now modify inside
    c = 'X'
    c2 = 'Y'
    PRINT "Modified to X and Y"
END

RUN

NEW
! Test STRING re-initialization
STRING s = "HELLO"
STRING s2

! Modify before RUN
s = "CHANGED"
s2 = "MODIFIED"

BEGIN
    PRINT "=== STRING Test ==="
    PRINT "s="; s; " ! expect HELLO"
    PRINT "s2="; s2; " ! expect empty"
    ! Now modify inside
    s = "FINAL"
    s2 = "TEST"
    PRINT "Modified to FINAL and TEST"
END

RUN

NEW
! Test VAR re-initialization
VAR v = 42
VAR v2

! Modify before RUN
v = "VARSTRING"
v2 = FALSE

BEGIN
    PRINT "=== VAR Test ==="
    PRINT "v="; v; " ! expect 42"
    PRINT "v2="; v2; " ! expect 0"
    ! Now modify inside
    v = TRUE
    v2 = 999
    PRINT "Modified to TRUE and 999"
END

RUN

NEW

! We got this far, now try the kitchen sink


! Test global re-initialization for all types
! Declare initialized globals (will reset to these values on RUN)
INT i = -100
WORD w = 5000
BIT b = TRUE
BYTE by = 255
CHAR c = 'A'
STRING s = "HELLO"
VAR v = 42

! Declare uninitialized globals (will reset to type defaults on RUN)
INT i2
WORD w2
BIT b2
BYTE by2
CHAR c2
STRING s2
VAR v2

! Modify all globals before RUN
i = 999
w = 12345
b = FALSE
by = 128
c = 'Z'
s = "CHANGED"
v = "VARSTRING"
i2 = -5555
w2 = 65535
b2 = TRUE
by2 = 200
c2 = 'Q'
s2 = "MODIFIED"
v2 = FALSE

BEGIN
    PRINT "=== After RUN (globals re-initialized) ==="
    PRINT "Initialized globals (expect original values):"
    PRINT "i="; i; " ! expect -100"
    PRINT "w="; w; " ! expect 5000"
    PRINT "b="; b; " ! expect TRUE"
    PRINT "by="; by; " ! expect 255"
    PRINT "c="; c; " ! expect 'A'"
    PRINT "s="; s; " ! expect HELLO"
    PRINT "v="; v; " ! expect 42"
    PRINT
    PRINT "Uninitialized globals (expect type defaults):"
    PRINT "i2="; i2; " ! expect 0"
    PRINT "w2="; w2; " ! expect 0"
    PRINT "b2="; b2; " ! expect FALSE"
    PRINT "by2="; by2; " ! expect 0"
    PRINT "c2="; c2; " ! expect NUL char (may show as space)"
    PRINT "s2="; s2; " ! expect empty string"
    PRINT "v2="; v2; " ! expect 0 (default VAR type)"
    PRINT
    PRINT "Now modifying inside BEGIN/END..."
    i = 777
    b = FALSE
    i2 = -999
    b2 = TRUE
END

RUN

! verify final values:

VARS

NEW
MEM

