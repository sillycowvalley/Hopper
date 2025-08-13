NEW
CLS
MEM



BEGIN
    PRINT
    PRINT "Mandelbrot - ported from Gordon's TinyBasic - Integers"
    PRINT "    Ported to Tigger BASIC"
    PRINT
    PRINT
    PRINT ' ';
    
    WORD START = SECONDS()
    
    string palette = ".,'~=+:;*%&$OXB#@ "
    
    int a : int b : int c : int d
    int q : int p : int t : int s : byte i
    int y : int x
    int f = 50
    
    FOR y = -12 TO 12
        FOR x = -49 TO 29
            c = x * 229 / 100
            d = y * 416 / 100
            a = c : b = d : i = 0
            while TRUE
                q = b / f : s = b - (q * f)
                t = ((a * a) - (b * b)) / f + c
                b = 2 * ((a * q) + (a * s / f)) + d
                a = t : p = a / f : q = b / f
                IF ((p * p) + (q * q)) >= 5 THEN
                    PRINT palette[i];
                    BREAK
                ELSE
                    I = I + 1
                    if i < 16 THEN
                        CONTINUE
                    ENDIF
                    PRINT ' ';
                    BREAK
                ENDIF
            WEND
        NEXT X
        PRINT
        PRINT ' ';
    NEXT Y
    
    WORD elapsed = SECONDS() - START
    
    PRINT
    PRINT sec; " seconds"
END
  
RUN
NEW
MEM

  