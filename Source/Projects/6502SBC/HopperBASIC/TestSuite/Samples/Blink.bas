BEGIN
   WORD COUNT
   PINMODE(0, 1) ! Pin 0  for Writing
   
   WHILE TRUE
       WRITE(0, 1)
       DELAY(500)
       PRINT "On"
       WRITE(0, 0)
       DELAY(500)
       PRINT "Off"
       COUNT = COUNT + 1
       PRINT COUNT, SECONDS()
   WEND
END

