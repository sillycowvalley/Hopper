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
    string utcDate;
    bool   valid;
    string latitude;
    string longitude;
    string ns;
    string ew;
    float  elevation;
    
    string UTC
    {
        get
        {
            string content;
            if (!utc.IsEmpty)
            {
                uint dot;
                _ = utc.IndexOf('.', ref dot);
                content = utc.Substring(0, dot-4) + ":" + utc.Substring(dot-4, 2) + ":" + utc.Substring(dot-2, 2);
            }
            return content;
                    
        }
    }
    string Date
    {
        get
        {
            return utcDate;
        }
    }
    
    string Elevation
    {
        get
        {
            return valid ? elevation.ToString() : "";
        }
    }
    
    string convertToDMS(string coordinate)
    {
        <string> parts = coordinate.Split('.');
        uint whole;
        uint fraction;
        _ = UInt.TryParse(parts[0], ref whole);
        _ = UInt.TryParse(parts[1], ref fraction);
        
        uint  degrees = whole / 100;
        float minutes = float(whole % 100) + float(fraction) / 10000.0;
        
         // Convert minutes to seconds
        float totalSeconds = minutes * 60.0;
        uint  minutesInDMS = uint(minutes);                      // Minutes for DMS
        float secondsInDMS = totalSeconds - (minutesInDMS*60);   // Seconds for DMS
        uint  seconds = uint(secondsInDMS);

        // Build the DMS string
        string dms = degrees.ToString() + "^ " + minutesInDMS.ToString() + "' " + (seconds.ToString()).LeftPad('0', 2) + '"';

        return dms;
    }
    string convertToDecimalDegrees(string coordinate)
    {
        <string> parts = coordinate.Split('.');
        
        uint whole;
        uint fraction;
        _ = UInt.TryParse(parts[0], ref whole);
        _ = UInt.TryParse(parts[1], ref fraction);
        
        // round DDMM.MMMM to DDMM.MMM
        fraction += 5;
        fraction /= 10;
        
        uint  degrees = whole / 100;
        float minutes = float(whole % 100) + float(fraction) / 1000.0;        
        
        // 4 decimal places = 11m precision at equator
        // 5 decimal places =  2m precision at equator
        // round to 5 decimal places:
        float decimal = minutes / 60.0;
        decimal += 0.00005;
        if (decimal >= 1.0)
        {
            degrees++;
            decimal -= 1.0;
        }
        string result = degrees.ToString() + (decimal.ToString()).Replace("0.", ".");
        uint index;
        _ = result.IndexOf('.', ref index);
        index += 5;
        result = result.Substring(0, index);
        return result;
    }
    
    string convertLatitudeToDecimal(string latitude, string hemisphere)
    {
        string degrees = convertToDecimalDegrees(latitude);
        // Apply the direction (N/S)
        if (hemisphere == "S")
        {
            degrees = "-" + degrees;
        }
        return degrees;
    }
    
    string convertLongitudeToDecimal(string longitude, string hemisphere)
    {
        string degrees = convertToDecimalDegrees(longitude);
        // Apply the direction (E/W)
        if (hemisphere == "W")
        {
            degrees = "-" + degrees;
        }
        return degrees;
    }
    string DecimalLatitude
    {
        get
        {
            return convertLatitudeToDecimal(latitude, ns);
        }
    }
    string DecimalLongitude
    {
        get
        {
            return convertLongitudeToDecimal(longitude, ew);
        }
    }
    
    string Latitude
    {
        get
        {
            string content;
            if (valid)
            {
                content = convertToDMS(latitude) + ns;
            }
            return content;
        }
    }
    string Longitude
    {
        get
        {
            string content;
            if (valid)
            {
                content = convertToDMS(longitude) + ew;
            }
            return content;
        }
    }
    
    bool Consume(string sentence)
    {
        bool newFix;
        <string> parts = sentence.Split(',');
        switch (parts[0])
        {
            case "$GNGGA":
            case "$GPGGA": // Global Positioning System Fix Data
            {
                utc       = parts[1];
                ns = parts[3];
                latitude = "";
                longitude = "";
                elevation = 0;
                valid = true;
                valid = valid && ((ns == "N") || (ns == "S"));
                if (valid)
                {
                    latitude = parts[2];
                    valid = !latitude.IsEmpty;
                }
                ew = parts[5];
                valid = valid && ((ew == "W") || (ew == "E"));
                if (valid)
                {
                    longitude = parts[4];
                    valid = !longitude.IsEmpty;
                }
                valid = valid && Float.TryParse(parts[9], ref elevation);
                if (elevation >= 0)
                {
                    elevation = uint(elevation); // precision loss
                }
                else
                {
                    elevation = -(uint(-elevation)); // precision loss
                }
                newFix = valid;
            }
            case "$GNRMC":
            case "$GPRMC":
            {
                string date = parts[9];
                //IO.WriteLn(date + " " + sentence);
                // '2024-02-28'
                utcDate = "20" + date.Substring(4,2) + "-" + date.Substring(2,2) + "-" + date.Substring(0,2);
                
            }
            default:
            {
                //IO.WriteLn(parts[0]);
            }
        }
        return newFix;
    }
}
