FUNC TEST1()
    VAR a = 10
    VAR b = 20  
    VAR f = 50
    VAR c = 5
    
    ! Test broken down step by step
    VAR aa = a * a
    PRINT "aa = "; aa; " ! should be 100"
    
    VAR bb = b * b
    PRINT "bb = "; bb; " ! should be 400"
    
    VAR diff = aa - bb  
    PRINT "diff = "; diff; " ! should be -300"
    
    VAR temp = diff / f
    PRINT "temp = "; temp; " ! should be -6"
    
    VAR result2 = temp + c
    PRINT "result2 = "; result2; " ! should be -1"
ENDFUNC


FUNC TEST2()
    VAR a = 10
    VAR b = 20  
    VAR f = 50
    VAR c = 5
    VAR extra1 = 1
    
    PRINT "a = "; a; " ! should be 10"
    PRINT "b = "; b; " ! should be 20"
    
    VAR aa = a * a
    PRINT "a * a = "; aa; " ! should be 100"
    
    VAR bb = b * b  
    PRINT "b * b = "; bb; " ! should be 400"
ENDFUNC

BEGIN
    TEST1()
    TEST2()
END

