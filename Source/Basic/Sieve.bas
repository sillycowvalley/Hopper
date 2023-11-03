 5 CLS
 7 PRINT " BYTE Sieve Benchmark - Tigger BASIC - Integers"
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

 5  start = SECONDS
10  SIZE = 8190
20  DIM FLAGS(8191) AS BOOL
30  PRINT "Only 1 iteration"
50  COUNT = 0
60  FOR I = 0 TO SIZE
70  FLAGS(I) = TRUE
80  NEXT I
90  FOR I = 0 TO SIZE
100 IF FLAGS(I) = FALSE THEN 180
110 PRIME = I + I + 3
115 REM PRINT PRIME
120 K = I + PRIME
130 IF K > SIZE THEN 170
140 FLAGS(K) = FALSE
150 K = K + PRIME
160 GOTO 130
170 COUNT = COUNT + 1
180 NEXT I
190 PRINT COUNT, "Primes"
200 elapsed = SECONDS - start
210 PRINT elapsed,"seconds"

 5  !160=0
10  S = 8190
20  F=TOP
30  PRINT "Only 1 iteration"
50  C = 0
60  FOR I = 0 TO S
70  @(F,I) = 1
80  NEXT I
90  FOR I = 0 TO S
100 IF @(F,I) = 0 GOTO 180
110 P = I + I + 3
115 REM PRINT P
120 K = I + P
130 IF K > S GOTO 170
140 @(F,K) = 0
150 K = K + P
160 GOTO 130
170 C = C + 1
180 NEXT I
190 PRINT C, "Primes"
200 E = !(160):PRINT "Time: ",E/10;
210VDU(46):VDU(E%10+48):PRINT" seconds"
220END








10 VDU12:PRINT " BYTE Sieve Benchmark - Gordon's Tiny BASIC - Integers"
15 S = 1:Print S, "Iterations"
20 !160=0:L=8191:REM Initialise TIME
30 F=TOP: REM DIM F(8192)
40 FOR M = 1 TO S
50     C = 0
60     FOR I = 0 TO L
70         ?(F+I) = 1
80     NEXT I
90     FOR I = 0 TO L
100        IF ?(F+I) = 0 GOTO 200
110        P = I + I + 3
120        REM PRINT P
130        K = I + P
140        IF K > L GOTO 190:REM Should be a WHILE .. WEND
150        DO
160            ?(F+K) = 0
170            K = K + P
180        UNTIL K > L
190        C = C + 1
200    NEXT I
210    PRINT M, ":", C, "primes"
220 NEXT M
230 E = !(160):PRINT " ", E/10, " secs."
240 END
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
60   PRINT A(I), "";
70 NEXT I
80 PRINT
90 PRINT "DONE"
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

1000 REM Mandelbrot Set Project
1010 REM Quite BASIC Math Project
1020 REM ------------------------ 
1030 CLS
1040 PRINT "This program plots a graphical representation of the famous Mandelbrot set.  It takes a while to finish so have patience and don't have too high expectations;  the graphics resolution is not very high on our canvas."
2000 REM Initialize the color palette
2010 GOSUB 3000
2020 REM L is the maximum iterations to try
2030 LET L = 100
2040 FOR I = 0 TO 100
2050 FOR J = 0 TO 100
2060 REM Map from pixel coordinates (I,J) to math (U,V)
2060 LET U = I / 50 - 1.5
2070 LET V = J / 50 - 1
2080 LET X = U
2090 LET Y = V
2100 LET N = 0
2110 REM Inner iteration loop starts here 
2120 LET R = X * X
2130 LET Q = Y * Y
2140 IF R + Q > 4 OR N >= L THEN GOTO 2190
2150 LET Y = 2 * X * Y + V
2160 LET X = R - Q + U
2170 LET N = N + 1
2180 GOTO 2120
2190 REM Compute the color to plot
2200 IF N < 10 THEN LET C = "black" ELSE LET C = P[ROUND(8 * (N-10) / (L-10))]
2210 PLOT I, J, C 
2220 NEXT J
2230 NEXT I
2240 END
3000 REM Subroutine -- Set up Palette
3010 ARRAY P
3020 LET P[0] = "black"
3030 LET P[1] = "magenta"
3040 LET P[2] = "blue"
3050 LET P[3] = "green"
3060 LET P[4] = "cyan"
3070 LET P[5] = "red"
3080 LET P[6] = "orange"
3090 LET P[7] = "yellow"
3090 LET P[8] = "white"
3100 RETURN

// https://www.dos4ever.com/SCMP/NIBL.html

100 VDU12:PRINT "Mandelbrot – Gordons TinyBasic – Integers"
110 PRINT "Start"
120 !160=0:REM Initialise TIME
130 Z=TOP:$Z=".,’~=+:;*%&$OXB#@ "
140 F=50
150 FOR Y = -12 TO 12
160 FOR X = -49 TO 29
170 C=X*229/100
180 D=Y*416/100
190 A=C:B=D:I=0
200 Q=B/F:S=B-(Q*F)
210 T=((A*A)-(B*B))/F+C
220 B=2*((A*Q)+(A*S/F))+D
230 A=T: P=A/F:Q=B/F
240 IF ((P*P)+(Q*Q))>=5 GOTO 280
250 I=I+1:IF I<16 GOTO 200
260 PRINT" ";
270 GOTO 290
280 VDU ?(Z+I)
290 NEXT X
300 PRINT ""
310 NEXT Y
320 Q=!160
330 PRINT"Finished"
340 PRINT"Time: ", Q/100, " secs."



10 Str = "Hello World"
20 for i = 0 to 10
30   ch = MID (str, i, 1)
40   PRINT CH
50 NEXT I
TRON
RUN

5  START = SECONDS
10 FOR i=1 TO 10
20 s=0
30 FOR j=1 TO 1000
40 s=s+j
50 NEXT j
60 PRINT ".";
70 NEXT i
80 PRINT s
90 ELAPSED = SECONDS - START
100 PRINT "Time: ", ELAPSED, " secs."

5  S = 2
10 DO
20 S=S*2:PRINT I,S
30 PRINT "MORE" :UNTIL S > 1024: PRINT "Done"
RUN
LIST

10 IF 10 < 20 GOTO 20: PRINT "!"
20 PRINT "?"
COMPILE
LIST

10 FOR I=0 TO 10
20 PRINT I
30 NEXT I
COMPILE
LIST
RUN


10 GOTO 20
20 GOTO 30


40 GOTO 50
50 PRINT "!";
60 GOTO 40
COMPILE
LIST

10 !160=0
20 DO
30 A = A + 1
40 UNTIL A > 1000
50 PRINT "Done:", !160



10 Z=TOP:$Z="ABC"
20 VDU ?(Z)
40 VDU ?(Z+1)
60 VDU ?(Z+2)
COMPILE
LIST

 5 IF A < 100 GOTO 30
10 IF A > 45 IF B < 56 AND C > 72 GOTO 30
20 PRINT "Not Here"
30 PRINT "HERE"
COMPILE
