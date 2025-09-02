
! Compile time
NEW
BEGIN
   PRINT "HELLO"
   BLAH
END
RUN

! Run time
NEW
VAR A = 0
BEGIN
   PRINT "HELLO"
   PRINT 100 / A
END
RUN

! Peephole
NEW
BEGIN
    VAR A
    FOR I = 1 TO 10000
        A = A + 1
    NEXT I
END
RUN

