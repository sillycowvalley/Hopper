unit Pitches
{
    <string,uint> tones;
    <string,uint> GetTones()
    {
        if (tones.Count == 0)
        {
            tones["A4"] = 440;
            tones["F4"] = 349;
            tones["C5"] = 523;
            tones["E5"] = 659;
            tones["F5"] = 698;
            tones["G5"] = 784;
            tones["A5"] = 880;
            tones["D5"] = 587;
            tones["G4"] = 392;
            tones["C4"] = 261;
            tones["D4"] = 294;
            tones["E4"] = 330;
            tones["B3"] = 247;
            tones["B4"] = 494;
            tones["B5"] = 988;
            tones["C6"] = 1046;
            tones["D6"] = 1174;
            tones["E6"] = 1318;
            tones["F6"] = 1396;
            tones["G6"] = 1567;
            tones["G3"] = 196;
            
            tones["C4"] = 261;
            tones["D4"] = 294;
            tones["E4"] = 330;
            tones["F4"] = 349;
            tones["G4"] = 392;
            tones["A4"] = 440;
            tones["B4"] = 494;
            tones["C5"] = 523;
            tones["D5"] = 587;
            tones["E5"] = 659;
            tones["F5"] = 698;
            tones["G5"] = 784;
            tones["A5"] = 880;
            tones["B5"] = 988;
            tones["C6"] = 1046;
            tones["D6"] = 1174;
            tones["E6"] = 1318;
            tones["F6"] = 1396;
            tones["G6"] = 1567;
            tones["G3"] = 196;
        }
        return tones;
    }   
}
