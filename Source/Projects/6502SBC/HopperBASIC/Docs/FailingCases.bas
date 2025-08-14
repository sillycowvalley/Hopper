VAR v = 42
v = "VARSTRING"

BEGIN
    PRINT "v="; v; " ! expect 42"
    v = TRUE
    PRINT "Modified to TRUE"
END

RUN

! Expect v to be TRUE:
VARS 
