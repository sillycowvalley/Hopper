program Timing
{
    //#define NO_PACKED_INSTRUCTIONS
    //#define NO_JIX_INSTRUCTIONS
    uses "/Source/Library/Boards/Hopper6502"
    //uses "/Source/Library/Boards/BenEater6502"
    //uses "/Source/Library/Boards/PD6502"
    //uses "/Source/Library/Boards/MECB6502"
    //uses "/Source/Library/Boards/PiPico"
    
    const uint iterations = 1000;
    Hopper()
    {
        //ClockSpeed = RP2040ClockSpeed.Overclock270;
        
        uint j;
        long start = Millis;

        uint i = iterations;        
        loop
        {
            if (i == 0) { break; }
            i--;
            
            // code to time:
            
            // 15us on 6502 at 8MHz 
            // 0.8us on Pi Pico at 133MHz (default)
            // 0.4us on Pi Pico at 270MHz (overclocked)
            j++; // compiles to: INCLOCALB <byte operand>
        }
        
        long elapsed = Millis - start;
        float perIteration = 1.0 * elapsed / iterations;
        
        // 6502 at 8MHz
        perIteration = perIteration - 0.083999; // per 'empty' iteration
        
        // Pi Pico (133MHz)
        //perIteration = perIteration - 0.0029; // per 'empty' iteration
        
        // Pi Pico (270MHz)
        //perIteration = perIteration - 0.0014; // per 'empty' iteration
        
        IO.WriteLn();
        IO.WriteLn("Elapsed: " + perIteration.ToString() + " ms");
    }
}
