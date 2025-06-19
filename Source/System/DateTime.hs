unit DateTime
{
    enum DayOfWeek // from C#
    {
        Sunday,
        Monday,
        Tuesday,
        Wednesday,
        Thursday,
        Friday,
        Saturday,
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
        if ((date.Length == 10) && (date[4] == '-'))
        {
            // '2024-02-28'
            if (!UInt.TryParse(date.Substring(0, 4), ref year))
            {
                return false;
            }
            if (!UInt.TryParse(date.Substring(5, 2), ref month))
            {
                return false;
            }
            if (!UInt.TryParse(date.Substring(8, 2), ref day))
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
    
    bool TryDateToDaysSince2000(string date, ref uint totalDays)
    {
        if (!TryDateToDays(date, ref totalDays))
        {
            return false;
        }
        uint year;
        if (!UInt.TryParse(date.Substring(0, 4), ref year))
        {
            return false;
        }
        if (year <= 2000)
        {
            return false;
        }
        loop
        {
            year--;
            totalDays += 365;
            if (IsLeapYear(year))
            {
                totalDays++;
            }
            if (year == 2000)
            {
                break;
            }
        }
        return true;
    }
    bool TryDateToDayOfWeek(string date, ref DayOfWeek dayOfWeek)
    {
        uint totalDays;
        if (!TryDateToDaysSince2000(date, ref totalDays))
        {
            return false;
        }
        switch((totalDays + 6) % 7) // 2000-01-01 was a Saturday (day 6)
        {
            case 0: { dayOfWeek = DayOfWeek.Sunday; }
            case 1: { dayOfWeek = DayOfWeek.Monday; }
            case 2: { dayOfWeek = DayOfWeek.Tuesday; }
            case 3: { dayOfWeek = DayOfWeek.Wednesday; }
            case 4: { dayOfWeek = DayOfWeek.Thursday; }
            case 5: { dayOfWeek = DayOfWeek.Friday; }
            case 6: { dayOfWeek = DayOfWeek.Saturday; }
        }
        return true;
    }
    
}
