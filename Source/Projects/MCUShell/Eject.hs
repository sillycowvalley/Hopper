program Eject
{
    uses "/Source/Library/Boards/PiPicoW"
    
    {
        SD.Eject();
        WriteLn("SD card can now be safely removed.");
        string current = (System.CurrentDirectory).ToLower();
        if (current.StartsWith("/sd/") || (current == "/sd"))
        {
            System.CurrentDirectory = "/";
        }
    }
}

