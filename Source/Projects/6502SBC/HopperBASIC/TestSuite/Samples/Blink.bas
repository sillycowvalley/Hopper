BEGIN
   VAR COUNT
   PINMODE(0, 1) ! Pin 0  for Writing
   
   WHILE TRUE
       WRITE(0, TRUE)
       DELAY(1000)
       PRINT "On"
       WRITE(0, FALSE)
       DELAY(1000)
       PRINT "Off"
       COUNT = COUNT + 1
       PRINT "Laps="; COUNT, "Seconds="; SECONDS()
   WEND
END

