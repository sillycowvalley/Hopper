program HighLow
{
    uses "/Source/Library/Boards/Hopper6502"
    //uses "/Source/Library/Boards/BenEater6502"
    //uses "/Source/Library/Boards/PD6502"
    
    Hopper() {
        IO.WriteLn();
        IO.WriteLn("Welcome to the game of high / low.");
        IO.WriteLn("I will guess a number between 1 and 100.  You must guess what it is.");
        IO.WriteLn("Press [ENTER] to start ...");
        
        // Random Seed
        long ms = Millis;
        uint s = UInt.FromBytes(ms.GetByte(0), ms.GetByte(1));       
        
        uint n = 0;       // Computer secret number.
        byte c = 0;       // Number of guesses.
        string gs = "";   // User input string
        uint g = 0;       // UInt of user input.
        
        
        /*
        While waiting for the user to hit the enter key, go ahead and count up.
        When they hit enter, the value of the counter becomes the seed.
        */
        loop {
            if(IO.IsAvailable) { break; }
            s ++;
        }
        UInt.Seed(s);
        n = UInt.Random() % 100 + 1;
        //IO.Write(" n=" + n.ToString()); // for testing
        while (g != n)
        {
            if (IO.ReadLn(ref gs)) {
                if (UInt.TryParse(gs, ref g)) {
                    c++;
                    if (g > n) {
                        IO.WriteLn("HIGH");
                    } else if ( g < n) {
                        IO.WriteLn("LOW");
                    }
                }
            }
        }
        IO.WriteLn("Well done, you got it in " + c.ToString() + " tries.");
    }
    
}
