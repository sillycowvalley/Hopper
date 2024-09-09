unit HX711
{
    /*
    
        This driver is based on the work by Bogdan Necula from this
        Arduino library: https://github.com/bogde/HX711
    
    */
    
    record LoadCell
    {
        byte clockPin;  // serial clock input pin
        byte dataPin;   // serial data output pin
        byte gain;      // amplification factor
        long offset;    // used for tare weight
        float scale;    // used to return weight in grams, kg, ounces, whatever
    }
    
    // Initialize LoadCell with data output pin, clock input pin and gain factor.
  		// Channel selection is made by passing the appropriate gain:
  		// - With a gain factor of 64 or 128, channel A is selected
  		// - With a gain factor of 32, channel B is selected
  		// The library default is "128" (Channel A).
    LoadCell Create(byte data, byte clock)
    {
        return Create(data, clock, 128);
    }
    LoadCell Create(byte data, byte clock, byte setGain)
    {
        LoadCell cell;
        cell.dataPin  = data;
        cell.clockPin = clock;
        switch (setGain)
        {
            case 32:  { cell.gain     = 2; }
            case 62:  { cell.gain     = 3; }
            
            default:  { cell.gain     = 1; }
        }
        cell.scale    = 1.0;
        
        PinMode(clock, PinModeOption.Output);
        PinMode(data,  PinModeOption.InputPullup);
        DigitalWrite(clock, false);
        
        return cell;
    }
    
    // Check if HX711 is ready
		  // from the datasheet: When output data is not ready for retrieval, digital output pin dataPin is high. Serial clock
  		// input clockPin should be low. When dataPin goes to low, it indicates data is ready for retrieval.
    bool IsReady(LoadCell cell)
    {
        return !DigitalRead(cell.dataPin);
    }
    
    SetTare(LoadCell cell, long tare)
    {
        cell.offset = tare;
    }
    
    waitReady(LoadCell cell) 
    {
        loop
        {
            if (IsReady(cell))
            {
                break;
            }
        }
    }
    
    byte shiftIn(byte dataPin, byte clockPin)
    {
        bool data;
        byte value;
        byte i = 7;
        loop
        {
            // Maximum high time for the clock pin of the HX711 is 50 microseconds.
            // The following 3 lines appear to be fast enough at 133MHz (even in debugger) but
            // we should probably overclock the RP2040 to be on the safe side.
            DigitalWrite(clockPin, true);
            data = DigitalRead(dataPin);
            DigitalWrite(clockPin, false);
            
            if (data)
            {
                value = value | (1 << i);
            }
            
            if (i == 0) { break; }
            i--;
        }
        return value;
    }
    
    long Read(LoadCell cell, bool useTare)
    {
        // Wait for the chip to become ready.
        waitReady(cell);
        
        // Define structures for reading data into.
        long value = 0;
        byte[4] data;
        byte filler = 0x00;
        
        byte dataPin  = cell.dataPin;
        byte clockPin = cell.clockPin;
        byte gain     = cell.gain;
        
        InterruptsEnabled = false;
        
        // Pulse the clock pin 24 times to read the data.
        data[2] = shiftIn(dataPin, clockPin);
        data[1] = shiftIn(dataPin, clockPin);
        data[0] = shiftIn(dataPin, clockPin);
        
        // Set the channel and the gain factor for the next reading using the clock pin.
        for (uint i = 0; i < gain; i++) 
        {
            DigitalWrite(clockPin, true);
           	DigitalWrite(clockPin, false);
        }
        
        InterruptsEnabled = true;
        
        if ((data[2] & 0x80) != 0) 
        {
            data[3] = 0xFF;
        } else 
        {
            data[3] = 0x00;
        }
        
        value = Long.FromBytes(data[0], data[1], data[2], data[3]);
        if (useTare)
        {
            value = value - cell.offset;
        }
        return value;
    }
}
