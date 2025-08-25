NEW
! Test 10: Type compatibility verification
VAR longNum = 1000000
VAR character = 'Z'
VAR boolean = FALSE
VAR text = "TEST"

CONST maxValue = 2147483647
CONST letter = 'A'
CONST flag = TRUE
CONST message = "HELLO"

! Modify before RUN
longNum = -999999
character = 'Q'
boolean = TRUE
text = "MODIFIED"

BEGIN
    PRINT "10. Type compatibility:"
    PRINT "longNum="; longNum; " expect 1000000"
    PRINT "character="; character; " expect Z"
    PRINT "boolean="; boolean; " expect FALSE"
    PRINT "text="; text; " expect TEST"
    
    ! Test assignments work
    longNum = maxValue
    character = letter
    boolean = flag
    text = message
    PRINT "Assignments successful"
END
RUN


