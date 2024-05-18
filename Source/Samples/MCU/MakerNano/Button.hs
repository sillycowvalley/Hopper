program MakerNanoMelody
{
    uses "/Source/Library/Boards/CytronMakerPiRP2040"
    uses "Pitches"

    const string powerUpNotes     = "E5,E5,,E5,,C5,E5,,G5,,G4";
    const byte[] powerUpDurations = { 150, 150, 150, 150, 150, 150, 150, 150, 150, 150 }; // ms

    const byte PIEZO_PIN = 22;
    const byte BUTTON_PIN = 20;

    InitializeButton()
    {
        MCU.PinMode(BUTTON_PIN, MCU.PinModeOption.Input);
        MCU.DigitalWrite(BUTTON_PIN, true); // Enable pull-up
    }

    PlayMelody(<string> notes, byte[] durations)
    {
        uint tuneSize = notes.Count;
        <string,uint> toneFrequencies = GetTones();

        for (uint thisNote = 0; thisNote < tuneSize; thisNote++)
        {
            string note = notes[thisNote];
            byte duration = durations[thisNote];
           if (toneFrequencies.Contains(note))
            {
                uint frequency = toneFrequencies[note];
                MCU.Tone(PIEZO_PIN, frequency);
                Time.Delay(duration);
                MCU.NoTone(PIEZO_PIN);
            }
            else
            {
                Time.Delay(duration);
            }
       }
    }

    CheckButton()
    {
        if (!MCU.DigitalRead(BUTTON_PIN))
        {
            IO.WriteLn("Button 1 pressed");
            MCU.Tone(PIEZO_PIN, 262);
            Time.Delay(100);
            MCU.NoTone(PIEZO_PIN);
            MCU.Tone(PIEZO_PIN, 659);
            Time.Delay(150);
            MCU.NoTone(PIEZO_PIN);
            MCU.Tone(PIEZO_PIN, 784);
            Time.Delay(200);
            MCU.NoTone(PIEZO_PIN);
        }
    }

    Hopper()
    {
        InitializeButton();
        PlayMelody((powerUpNotes).Split(','), powerUpDurations);

        loop
        {
            CheckButton();
            Time.Delay(100); // Sleep for debounce
        }
    }
}

