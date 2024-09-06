program Scale
{
    uses "/Source/Library/Boards/ChallengerNB2040WiFi"
    uses "/Source/Library/Devices/Adafruit128x64OLEDFeatherwing"
    uses "/Source/Library/Devices/HX711"
        
/*

void loop() {

  if (scale.is_ready()) {
    long reading = scale.read();
    Serial.print("HX711 reading: ");
    Serial.println(reading);
  } else {
    Serial.println("HX711 not found.");
  }

  delay(1000);
  
}
*/

    Hopper()
    {
        // Overclocking to meet the clock criteria for the HX711 (see HX711.shiftIn())
        ClockSpeed = RPClockSpeed.Overclock250; // Challenger RP2040 boards
        
        LoadCell cell = HX711.Create(GP17, GP16);
        int wtf = 0;
        
        loop
        {
            if (HX711.IsReady(cell))
            {
                long reading = HX711.Read(cell);
                WriteLn("HX711 reading: " + reading.ToString());
            }
            else
            {
                WriteLn("HX711 not found.");
            }
            Delay(500);
        }
    }
}
