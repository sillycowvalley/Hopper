NEW
CLS
MEM

! Sieve of Eratosthenes - Byte Magazine benchmark
CONST sizepl  = 8191
BIT flags[sizepl]

VAR i
VAR prime
VAR k
VAR count
VAR iter
VAR start
VAR elapsed

BEGIN
    PRINT "10 iterations"
    start = SECONDS()
    FOR iter = 1 TO 1
        count = 0
        
        ! Initialize flags array to true
        FOR i = 0 TO sizepl-1
            flags[i] = TRUE
        NEXT i
        
        ! Sieve algorithm
        FOR i = 0 TO sizepl-1
            IF flags[i] THEN
                prime = i + i + 3
                k = i + prime
                WHILE k < sizepl
                    flags[k] = FALSE
                    k = k + prime
                WEND
                count = count + 1
            ENDIF
        NEXT i
        PRINT ".",
    NEXT iter
    elapsed = SECONDS() - start
    
    PRINT "Done."
    PRINT count, " primes", elapsed, " seconds"
END
RUN

! DASM

