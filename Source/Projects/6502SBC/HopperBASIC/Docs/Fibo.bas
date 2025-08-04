FUNC Fibo(n)
    IF n <= 1 THEN RETURN n ENDIF
    RETURN Fibo(n-1) + Fibo(n-2)
ENDFUNC

FUNC Benchmark(name, arg, loops)
    WORD start
    WORD result
    WORD count
    WORD elapsed
    WORD avgS
    
    start = SECONDS()
    
    FOR count = 0 TO loops-1
        result = Fibo(arg)
    NEXT count
    
    elapsed = MILLIS() - start
    avgS = elapsed / loops
    
    PRINT name, "(", arg, ") = ", result, " in ", avgS, " seconds average"
ENDFUNC

BEGIN
    Benchmark(10, 1)
END

' test from REPL
Benchmark(10, 1)
' or just
RUN

