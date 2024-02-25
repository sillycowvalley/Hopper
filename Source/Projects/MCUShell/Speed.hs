program Speed
{
    uses "/Source/Library/Boards/PiPicoW"
    
    Hopper()
    {
        <string> args = System.Arguments;
        char cli = char(0);
        if (args.Count == 1)
        {
            string arg = args[0];
            if (arg.Length == 1)
            {
                cli = arg[0];
            }
        }
        
        WriteLn("Select clock speed:");
        
        <RP2040ClockSpeed> speeds;
        speeds.Append(RP2040ClockSpeed.Slow48);
        speeds.Append(RP2040ClockSpeed.Default133);
        speeds.Append(RP2040ClockSpeed.Overclock250);
        speeds.Append(RP2040ClockSpeed.Overclock270);
        
        bool exit = false;
        loop
        {
            RP2040ClockSpeed currentSpeed = MCU.ClockSpeed;
            if (cli == char(0))
            {
                for (uint i = 0; i < speeds.Count; i++)
                {
                    RP2040ClockSpeed speed = speeds[i];
                    string mhz = (uint(speed)).ToString();
                    string prompt = "    [" + i.ToString() + "] - " + mhz.LeftPad(' ', 3) + " MHz";
                    if (speed == currentSpeed)
                    {
                        prompt += " (current)";
                    }
                    if (!exit || (speed == currentSpeed))
                    {
                        WriteLn(prompt);
                    }
                }
            }
            if (exit) { break; }
        
            Write("Select speed or press (<esc> to exit):");
            char choice = cli;
            cli = char(0);
            if (choice == char(0))
            {
                choice = Serial.ReadChar();
            }
            if (choice == char(0x1B))
            {
                WriteLn();
                break;
            }
            if (Char.IsDigit(choice))
            {
                WriteLn(" " + choice);
                switch (choice)
                {
                    case '0': { MCU.ClockSpeed = RP2040ClockSpeed.Slow48;       exit = true; }
                    case '1': { MCU.ClockSpeed = RP2040ClockSpeed.Default133;   exit = true; }
                    case '2': { MCU.ClockSpeed = RP2040ClockSpeed.Overclock250; exit = true; }
                    case '3': { MCU.ClockSpeed = RP2040ClockSpeed.Overclock270; exit = true; }
                }
            }
            else
            {
                WriteLn();
            }
        }
    }
}
