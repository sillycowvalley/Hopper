unit GPS
{
    // initially written for the AdaFruit Mini GPS
    // https://www.adafruit.com/product/4415
    
    bool gpsConfigured;
    byte i2cAddress    = 0x10;
    byte i2cController = Wire.DefaultI2CController;
    byte sdaPin        = Wire.DefaultI2CSDAPin;
    byte sclPin        = Wire.DefaultI2CSCLPin;
    
    byte I2CController { get { return i2cController; } set { i2cController = value; } }
    byte I2CAddress    { get { return i2cAddress; }    set { i2cAddress = value; } }
    byte I2CSDAPin     { get { return sdaPin; }        set { sdaPin = value; } }
    byte I2CSCLPin     { get { return sclPin; }        set { sclPin = value; } }
    
    bool Begin()
    {
        gpsConfigured = false;
        loop
        {
            if (!Wire.Initialize(GPS.I2CController, GPS.I2CSDAPin, GPS.I2CSCLPin, 400))
            {
                break;
            }
            Wire.BeginTx(GPS.I2CController, GPS.I2CAddress);
            gpsConfigured = Wire.EndTx(GPS.I2CController) == 0; // 0 from EndTx means 'success'
            Time.Delay(10);
            break;
        }
        return gpsConfigured;
    }
    bool NextSentence(ref string sentence)
    {
        sentence = "";
        loop
        {
            byte available = Wire.RequestFrom(GPS.I2CController, GPS.I2CAddress, 1);
            if (available == 0)
            {
                return false;
            }
            byte c = Wire.Read(GPS.I2CController);
            if ((c >= 32) && (c < 127))
            {
                sentence += char(c);
            }
            else if (c == 0x0D)
            {
                break;
            }
        }
        return (sentence.Length != 0) && verifyChecksum(sentence);
    }
    bool SendSentence(string content)
    {
         Wire.BeginTx(GPS.I2CController, GPS.I2CAddress);
         foreach (var ch in content)
         {
             Wire.Write(byte(ch));
         }   
         Wire.Write(GPS.I2CController, 0x0D); // cr
         Wire.Write(GPS.I2CController, 0x0A); // lf
         return 0 == Wire.EndTx(GPS.I2CController);
    }
    
    
    // Function to calculate the checksum for a given NMEA sentence
    byte calculateChecksum(string sentence)
    {
        byte checksum = 0;

        // Start XOR calculation after the '$' and stop before '*'
        for (uint i = 1; i < sentence.Length; i++)
        {
            char ch = sentence[i];
            if (ch == '*')
            {
                break;
            }
            checksum = checksum ^ byte(ch);
        }
        return checksum;
    }
    
    // Function to verify the checksum of an NMEA sentence
    bool verifyChecksum(string sentence)
    {
        // Find the asterisk position
        uint asteriskIndex;
        if (!sentence.IndexOf('*', ref asteriskIndex) || (asteriskIndex + 2 >= sentence.Length))
        {
            return false; // invalid format
        }

        // Extract the checksum from the sentence
        string checksumStr = sentence.Substring(asteriskIndex + 1, 2);
        uint providedChecksum;
        if (!UInt.TryParse("0x" + checksumStr, ref providedChecksum))
        {
            return false; // Invalid checksum format
        }

        // Calculate the checksum
        byte calculatedChecksum = calculateChecksum(sentence);

        // Compare the provided checksum with the calculated checksum
        return calculatedChecksum == byte(providedChecksum);
    }
    
    string utc;
    string latitude;
    string longitude;
    string ns;
    string ew;
    string elevation;
    
    string UTC
    {
        get
        {
            uint dot;
            _ = utc.IndexOf('.', ref dot);
            string content = utc.Substring(0, dot-4) + ":" + utc.Substring(dot-4, 2) + ":" + utc.Substring(dot-2, 2);
            return content;
                    
        }
    }
    
    string Elevation
    {
        get
        {
            return elevation + "m";
        }
    }
    
    string convertToDMS(string coordinate)
    {
         // Determine degrees dynamically
        uint separatorIndex;
        _ = coordinate.IndexOf('.', ref separatorIndex);
        
        uint degreesLength = separatorIndex > 4 ? 3 : 2;             // Longitude uses 3 digits for degrees, latitude 2
        string degreesPart = coordinate.Substring(0, degreesLength); // Extract degrees
        string minutesPart = coordinate.Substring(degreesLength);    // Extract minutes (MM.MMMM)

        // Parse degrees and minutes
        uint degrees;
        _ = UInt.TryParse(degreesPart, ref degrees);
        float minutes;
        _ = Float.TryParse(minutesPart, ref minutes);

        // Convert minutes to seconds
        float totalSeconds = minutes * 60.0;
        uint  minutesInDMS = uint(minutes);                      // Minutes for DMS
        float secondsInDMS = totalSeconds - (minutesInDMS*60);   // Seconds for DMS
        uint  seconds = uint(secondsInDMS);

        // Build the DMS string
        string dms = degrees.ToString() + "^ " + minutesInDMS.ToString() + "' " + (seconds.ToString()).LeftPad('0', 2) + '"';

        return dms;
    }
    
    string Latitude
    {
        get
        {
            string content;
            if (latitude.Length != 0)
            {
                content = convertToDMS(latitude) + ew;
            }
            return content;
        }
    }
    string Longitude
    {
        get
        {
            string content;
            if (longitude.Length != 0)
            {
                content = convertToDMS(longitude) + ew;
            }
            return content;
        }
    }
    
    bool Consume(string sentence)
    {
        bool newFix;
        <string> parts = sentence.Replace(",,", ", ,").Split(',');
        switch (parts[0])
        {
            case "$GNGGA":
            case "$GPGGA": // Global Positioning System Fix Data
            {
                newFix = true;
                utc       = parts[1];
                ns = parts[3];
                latitude  = ((ns == "N") || (ns == "S")) ? parts[2] : "";
                ew = parts[5];
                longitude = ((ew == "W") || (ew == "E")) ? parts[4] : "";
                elevation = parts[9];
            }
        }
        return newFix;
    }
}
