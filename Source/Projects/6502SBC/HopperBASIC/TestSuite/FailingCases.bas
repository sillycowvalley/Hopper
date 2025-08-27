BEGIN
    PRINT "Testing RND function"
    PRINT ""
    
    PRINT "Rolling dice (1-6):"
    PRINT RND(6), RND(6), RND(6), RND(6), RND(6)
    PRINT ""
    
    PRINT "Random 1-100:"
    PRINT RND(100), RND(100), RND(100)
    PRINT ""
    
    PRINT "Always 1 (RND(1)):"
    PRINT RND(1), RND(1), RND(1)
    PRINT ""
    
    PRINT "Random 1-10 stored in variable:"
    VAR num = RND(10)
    PRINT "Random number: "; num
    PRINT ""
    
    PRINT "Random numbers in a loop:"
    FOR i = 1 TO 5
        PRINT "Roll "; i; ": "; RND(20)
    NEXT i
END

RUN
