10 DIM flags(201) AS BOOL
15 iterations = 1
20 PRINT iterations, "Iterations"
25 start = SECONDS
30 FOR m = 1 TO iterations
40     count = 0
50     FOR i = 0 TO 200
60         flags(i) = TRUE
70     NEXT  i  
80     FOR i = 0 TO 200
90         IF flags(i) = FALSE THEN 170
100        prime = i + i + 3
105        REM PRINT prime
110        k = i + prime
120        WHILE k <= 200
130            flags(k) = FALSE
140            k = k + prime
150        WEND
160        count = count + 1
170    NEXT i   
175    PRINT m, ":", count,"primes"
180 NEXT m 
185 elapsed = SECONDS - start
190 PRINT (elapsed/iterations),"sec per iteration"
197 PRINT elapsed,"sec total"
200 END
RUN

5 PRINT SECONDS
10 DIM A(100) AS BOOL
20 FOR I = 0 TO 99
30 A(I) = TRUE
35
50 NEXT I
60 FOR I = 0 TO 99
80 NEXT I
85 PRINT SECONDS
RUN

10 DIM A(20)
20 for I = 0 TO 19
30   A(I) = I
40 NEXT I
50 for I = 0 TO 19
60   PRINT A(I)
70 NEXT I
RUN

10 FOR I = 1 TO  10
20 NEXT I
30 END
TRON
RUN

10 LET A = 0
20 WHILE A < 3
30   A = A + 1
40   LET B = 0
50   WHILE B < 2
60     B = B + 1
70   WEND
80 WEND
90 END
TRON
RUN

10 IF TRUE = FALSE THEN 20
20 REM
30 END
TRON
RUN

10 WHILE true
20   LED = true
30   DELAY 500
40   LED = false
50   DELAY 500
60 WEND

