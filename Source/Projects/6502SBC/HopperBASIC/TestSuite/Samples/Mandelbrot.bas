NEW
CLS
MEM

FUNC INTRO()
    PRINT
    PRINT "Mandelbrot - ported from Gordon's TinyBasic"
    PRINT "                Updated for Hopper BASIC v3"
    PRINT
    PRINT
ENDFUNC    

FUNC DRAW()
    PRINT ' ';
    VAR palette = ".,'~=+:;*%&$OXB#@ "
    
    VAR a, b, c, d
    VAR q, p, t, s, i
    VAR f = 50
    
    FOR y = -12 TO 12
        FOR x = -49 TO 29
            c = x * 229 / 100
            d = y * 416 / 100
            a = c
            b = d
            i = 0
            
            VAR done = FALSE
            WHILE NOT done
                q = b / f
                s = b - (q * f)
                t = ((a * a) - (b * b)) / f + c
                b = 2 * ((a * q) + (a * s / f)) + d
                a = t
                p = a / f
                q = b / f
                
                IF ((p * p) + (q * q)) >= 5 THEN
                    PRINT palette[i];
                    done = TRUE
                ELSE
                    i = i + 1
                    IF i >= 16 THEN
                        PRINT ' ';
                        done = TRUE
                    ENDIF
                ENDIF
            WEND
        NEXT x
        PRINT
        PRINT ' ';
    NEXT y
ENDFUNC

BEGIN
    INTRO()
    
    VAR start = SECONDS()
    DRAW()
    VAR elapsed = SECONDS() - start
    
    PRINT
    PRINT elapsed; " seconds"
END