unit Ultrasonic
{
    long PulseIn(byte pin, byte state)  
    {
        // Maximum 1 second timeout 
        const long TIMEOUT = 1000;
        long start = Time.Millis;
        
        // Wait for pulse to start (opposite of state we want)
        while (MCU.DigitalRead(pin) == state)
        {
            if ((Time.Millis - start) > TIMEOUT)
            {
                return 0; // Timeout waiting for start
            }
        }
        
        // Get start time once pulse begins
        start = Time.Millis;
        
        // Wait for pulse to end
        long width = 0;
        while (MCU.DigitalRead(pin) == state)
        {
            width = Time.Millis - start;
            if (width > TIMEOUT)
            {
                return 0; // Timeout waiting for end
            }
        }
        
        // Convert to microseconds - multiply by sample rate
        return width * Time.SampleMicros;
    }
    
    uint ReadDistance(byte trigPin, byte echoPin)
    {
        // Send trigger pulse
        MCU.DigitalWrite(trigPin, false);
        Time.Delay(1);
        MCU.DigitalWrite(trigPin, true);
        Time.Delay(1);
        MCU.DigitalWrite(trigPin, false);
        
        // Read echo pulse duration in microseconds
        long duration = PulseIn(echoPin, true);
        
        // Convert to centimeters (duration / 58)
        return uint(duration / 58);
    }
    
    uint ReadDistanceAverage(byte trigPin, byte echoPin, byte samples)
    {
        uint total = 0;
        
        for (uint i = 0; i < samples; i++)
        {
            total = total + ReadDistance(trigPin, echoPin);
            Time.Delay(10);
        }
        
        return total / samples;
    }
}