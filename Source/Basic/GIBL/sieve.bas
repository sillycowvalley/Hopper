NEW
 90VDU12
100REM Sieve of Eratosthenes
110REM Limit will be 78*29 so we fit a screen
120L=78*29-1
130C=TOP : $C=".*"
135Z=C+4 : REM The array
140REM Initialise it - Everything is true until proven otherwise...
150FOR I = 0 TO L : ?(Z+I) = 1 : NEXT I
160?(Z+0)=0:?(Z+1)=0 : REM We know 0 and 1 are not prime...

180REM Get upper limit - square root of the number we're looking at
190S=L : GOSUB 1000 : U = Q
200PRINT "Upper limit is: ",U
210PRINT "Finding..."
220GOSUB 600
230GOSUB 2000
240END

600REM Find Primes
610F=2
620DO
630 P=?(Z+F)
640 IF P=1 GOSUB 800
650 F=F+1
660UNTIL F > U
670RETURN

800REM Strikeout
810REM F is the found Prime. Uses J.
820J=F+F
830DO
840 ?(Z+J)=0
850 J=J+F
860UNTIL J >= L
870RETURN

1000REM Integer Square Root - Herons method.
1010REM Input: S, Output Q, uses X,Y
1100IF S < 2 Q=S : RETURN
1110X=S/2
1120Y=(S/X+X)/2
1130DO
1140 X=Y
1150 Y=(S/X+X)/2
1160UNTIL X <= Y
1170Q=X
1180RETURN

2000REM Print
2010REM Uses I,J
2020J=0:FOR I = 0 TO L
2030 VDU ?(C+?(Z+I))
2040 J=J+1:IF J=78 PR "" : J=0
2050NEXT I
2060RETURN
