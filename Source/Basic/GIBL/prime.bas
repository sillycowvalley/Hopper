NEW

100 VDU12:PRINT "Find Prime numbers.."
101 PRINT "Upper limit ";
102 INPUT L

105 Z = 0 : C = 0
110 FOR N = 0 TO L
120  GOSUB 800
130  IF P <> 0 GOSUB 900
140 NEXT N
150 IF C <> 0 THEN PRINT ""
160 PRINT "Found ", Z, " primes."
170 END

800 REM Is it Prime?
810 REM Input N, output P - true or false (1 or 0)
811 REM
820 IF N < 1 P = 0 : RETURN : REM 0 and 1 are not Prime
830 IF N < 4 P = 1 : RETURN : REM 2 and 3 are Prime
834 IF N%2 = 0 P = 0 : RETURN : REM Trivial case for even numbers
840 S = N : GOSUB 1000 : U = Q : REM Test up to the sqrt of S
845 T = 3
850 REM
860   IF N % T = 0 THEN P = 0 : RETURN
870   T = T + 2
880 IF T <= Q GOTO 850
890 P = 1
899 RETURN

900 REM Print N in a field of 6 in 10 columns
930 IF N < 10000 PR " ";
940 IF N <  1000 PR " ";
950 IF N <   100 PR " ";
960 IF N <    10 PR " ";
970 PR N;
980 Z = Z + 1 : C = C + 1 : IF C = 10 PRINT "" : C = 0
990 RETURN


1000 REM Integer Square Root - Herons method.
1010 REM     Input: S, Output Q, uses X,Y
1100 IF S < 2 Q=S : RETURN
1110 X=S/2
1120 Y=(S/X+X)/2
1130 DO
1140  X=Y
1150  Y=(S/X+X)/2
1160 UNTIL X <= Y
1170 Q=X
1180 RETURN
