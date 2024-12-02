program Proximity
{
    uses "/Source/Library/Boards/PiPicoW"
    
    const byte sensorPin = GP16;
    const byte ledPin    = GP17;
    
    Hopper()
    {
        MCU.PinMode(sensorPin, PinModeOption.Input);
        MCU.PinMode(ledPin,    PinModeOption.Output);
        
        long lightUntil = Time.Millis;
        loop
        {
            bool triggered = MCU.DigitalRead(sensorPin);
            if (triggered)
            {
                // keep lit for 3 seconds after sensor goes low
                lightUntil = Time.Millis + 3000;
            }
            MCU.DigitalWrite(ledPin, Time.Millis < lightUntil);
        }
    }
}
