program Melody
{
    uses "/Source/Library/Boards/CytronMakerPiRP2040"
    uses "Pitches"
    
    const byte GPIO22 = 22; // Buzzer pin on the Maker Nano RP2040
    
    const float tempo = 120.0;
    const string melody = "A4,A4,A4,F4,C5,A4,F4,C5,A4,E5,E5,E5,F5,C5,A4,F4,C5,A4";
    const byte[] rhythm = {
        4, 4, 4, 5, 16, 4, 5, 16, 2,
        4, 4, 4, 5, 16, 4, 5, 16, 2,
    };
    /*
    const float tempo = 240.0;    
    const string melody = "G4,D5,G5,B5,D6,G6,D5,G5,B5,D6,G6,D5,G5,B5,D6,G6,C6,G6,E6,G6,B5,D6,F6,G6,G4,D5,G5,B5,D6,G6,D5,G5,B5,D6,G6,D5,G5,B5,D6,G6,C6,G6,E6,G6,B5,D6,F6,G6,G5,D5,G5,B5,D6,G6,D5,G5,B5,D6,G6,D5,G5,B5,D6,G6,C6,G6,E6,G6,B5,D6,F6,G6,G4,D5,G5,B5,D6,G6,D5,G5,B5,D6,G6,D5,G5,B5,D6,G6,C6,G6,E6,G6,B5,D6,F6,G6";

    const byte[] rhythm = {
        8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
        8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
        8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
        8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
        8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
        8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
        8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
        8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8
    };
    */
    /*
    const float tempo = 120.0;  
    const string melody = "E4,E4,F4,G4,G4,F4,E4,D4,C4,C4,D4,E4,E4,D4,D4,E4,E4,F4,G4,G4,F4,E4,D4,C4,C4,D4,E4,D4,C4,C4,D4,D4,E4,C4,D4,E4,F4,E4,C4,D4,E4,F4,E4,D4,C4,D4,G3,E4,E4,F4,G4,G4,F4,E4,D4,C4,C4,D4,E4,D4,C4,C4";
    const byte[] rhythm = {
        8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
        8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
        8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
        8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8
    };
    */
    

    PlayTune()
    {
        <string> notes = (melody).Split(',');
        uint tuneSize = notes.Count;
        
        <string,uint> toneFrequencies = Pitches.GetTones();
        
        float fduration = (60.0 / tempo) * 4.0 * 1000.0;
        
        for (uint thisNote = 0; thisNote < tuneSize; thisNote++)
        {
            uint frequency = toneFrequencies[notes[thisNote]];
            uint duration = uint(fduration / rhythm[thisNote]);
            MCU.Tone(GPIO22, frequency);
            Time.Delay(duration);
            MCU.NoTone(GPIO22);
            Time.Delay(duration / 4); // pause between notes
        }
    }

    Hopper()
    {
        PlayTune();
    }
}

