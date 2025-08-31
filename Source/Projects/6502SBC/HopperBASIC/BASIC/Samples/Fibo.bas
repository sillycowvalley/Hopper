NEW
CLS
MEM
FUNC Fibo(n)
    IF n <= 1 THEN
        RETURN n 
    ENDIF
    RETURN Fibo(n-1) + Fibo(n-2)
ENDFUNC

FUNC Benchmark(name, arg, loops)
    VAR start
    VAR result
    VAR count
    VAR elapsed
    VAR avgS
    
    start = SECONDS()
    
    FOR count = 0 TO loops-1
        result = Fibo(arg)
    NEXT count
    
    elapsed = SECONDS() - start
    avgS = elapsed / loops
    
    PRINT name; "(", arg, ") = "; result; " in "; avgS; " seconds average"
ENDFUNC

BEGIN
    Benchmark("Fibonacci", 24, 1)
END

Benchmark("Fibonacci", 20, 1)
