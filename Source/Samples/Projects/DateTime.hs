unit DateTime
{
    // good defaults for NZ in case the time server doesn't provide 
    const uint dstStartDay = 267;
    const uint dstEndDay   = 97;
    
    bool TryDSTFromDay(uint dayOfYear, ref bool dst)
    {
        if (dstEndDay < dstStartDay) 
        {
            dst = !((dayOfYear > dstEndDay) && (dayOfYear < dstStartDay)); // Southern Hemisphere
        }
        else
        {
            dst = ((dayOfYear >= dstStartDay) && (dayOfYear <= dstEndDay)); // Northern Hemisphere
        }
        return true;
    }
    
    string MinutesToTime(long dayMinutes)
    {
        long hours   = dayMinutes / 60;
        long minutes = dayMinutes % 60;
        return (hours.ToString()).LeftPad('0', 2) + ":" + (minutes.ToString()).LeftPad('0', 2);
    }
    bool TryTimeToMinutes(string time, ref uint totalMinutes)
    {
        uint hours;
        uint minutes;
        uint iT;
        if (time.IndexOf('T', ref iT)) { time = time.Substring(iT+1); }
        if (!UInt.TryParse(time.Substring(0, 2), ref hours))
        {
            return false;
        }
        if (!UInt.TryParse(time.Substring(3, 2), ref minutes))
        {
            return false;
        }
        if (time.Contains("PM"))
        {
            hours += 12;
        }
        totalMinutes = hours * 60 + minutes;
        return true;
    }
    bool TryDateToDays(string date, ref uint totalDays)
    {
        uint month;
        uint day;
        uint year;
        
        totalDays = 0;
        
        if (date.Contains(',') && date.Contains(' '))
        {
            // 'Tue, 15 Nov 1994' : from HTTP headers
            <string> parts = date.Split(' ');
            if (parts.Count != 4)
            {
                return false;
            }
            switch (parts[2])
            {
                case "Jan": { month = 1; }
                case "Feb": { month = 2; }
                case "Mar": { month = 3; }
                case "Apr": { month = 4; }
                case "May": { month = 5; }
                case "Jun": { month = 6; }
                case "Jul": { month = 7; }
                case "Aug": { month = 8; }
                case "Sep": { month = 9; }
                case "Oct": { month = 10; }
                case "Nov": { month = 11; }
                case "Dec": { month = 12; }
                default:    { return false; }
            }
            if (!UInt.TryParse(parts[1], ref day))
            {
                return false;
            }
            if (!UInt.TryParse(parts[3], ref year))
            {
                return false;
            }   
        }
        else
        {
            // '02-28-2024' : MM-DD-YYYY from http://time.jsontest.com/
            if (!UInt.TryParse(date.Substring(0, 2), ref month))
            {
                return false;
            }
            if (!UInt.TryParse(date.Substring(3, 2), ref day))
            {
                return false;
            }
            if (!UInt.TryParse(date.Substring(6, 4), ref year))
            {
                return false;
            }
        }
        uint currentMonth = 1;
        while (currentMonth < month)
        {
            switch (currentMonth)
            {
                case 2:  // February
                { 
                    totalDays += 28; 
                    if (IsLeapYear(year)) { totalDays++; }
                }
                case 4:  // April
                case 6:  // June
                case 9:  // September
                case 11: // November
                { 
                    totalDays += 30; 
                }
                default:
                { 
                    totalDays += 31;
                }
            }
            currentMonth++;
        }
        totalDays += day;
        return true;
    }
    
    long NormalizeMinutes(long minutes)
    {
        while (minutes > 24*60) { minutes -= 24*60; }
        while (minutes < 0)     { minutes += 24*60; }
        return minutes;
    }
    bool IsLeapYear(uint year)
    {
        bool leap;
        if ((year % 4) == 0)
        {
            leap = true;
            if ((year % 100) == 0)
            {
                leap = ((year % 400) == 0);
            }
        }
        return leap;
    }
}
