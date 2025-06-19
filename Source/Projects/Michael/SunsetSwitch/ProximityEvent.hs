program ProximityEvent
{
    uses "/Source/Library/Boards/PiPicoW"
    
    const byte sensorPin = GP16;
    const byte ledPin    = GP17;
    
    long lightUntil = Time.Millis;
    
    sensorEvent(byte pin, PinStatus status)
    {
        // keep lit for 3 seconds after sensor goes low
        lightUntil = Time.Millis + 3000;
    }
    
    Hopper()
    {
        MCU.PinMode(ledPin,    PinModeOption.Output);
        if (!MCU.AttachToPin(sensorPin, sensorEvent, PinStatus.Falling))
        {
            IO.WriteLn("Failed to attach to pin " + (sensorPin).ToString());
        }
        
        loop
        {
            MCU.DigitalWrite(ledPin, Time.Millis < lightUntil);
        }
    }
}
