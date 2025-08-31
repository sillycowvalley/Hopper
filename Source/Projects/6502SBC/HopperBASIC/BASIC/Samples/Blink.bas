CONST PIN = 0

BEGIN
   VAR COUNT
   PINMODE(PIN, 1) ! set to "write"
   VAR STATE = FALSE
   WHILE TRUE
       STATE = NOT STATE
       WRITE(PIN, STATE)
       DELAY(1000)
       COUNT = COUNT + 1
       PRINT "Laps="; COUNT, "Seconds="; SECONDS(), "Milliseconds="; MILLIS()
   WEND
END

