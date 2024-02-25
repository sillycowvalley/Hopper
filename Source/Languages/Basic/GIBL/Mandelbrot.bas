NEW
  100VDU12:PRINT "Mandelbrot - Gordon's TinyBasic - Integers"
  110PRINT "Start"
  120!160=0:REM Initialise TIME
  130Z=TOP:$Z=".,'~=+:;*%&$OXB#@ "
  140F=50
  150FOR Y = -12 TO 12
  160FOR X = -49 TO 29
  170C=X*229/100
  180D=Y*416/100
  190A=C:B=D:I=0
  200Q=B/F:S=B-(Q*F)
  210T=((A*A)-(B*B))/F+C
  220B=2*((A*Q)+(A*S/F))+D
  230A=T: P=A/F:Q=B/F
  240IF ((P*P)+(Q*Q))>=5 GOTO 280
  250I=I+1:IF I<16 GOTO 200
  260PRINT" ";
  270GOTO 290
  280VDU ?(Z+I)
  290NEXT X
  300PRINT ""
  310NEXT Y
  320Q=!160
  330PRINT"Finished"
  340PRINT "Time: ",Q/10;
  350VDU(46):VDU(Q%10+48):PRINT" seconds"
  360END
  