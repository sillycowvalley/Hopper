program PiSpigot
{
    #define SERIAL_CONSOLE
    uses "/Source/System/System"
    uses "/Source/System/IO"

    const uint n = 20; //1000;
    const uint len = 10 * n / 3;

    WriteUInt(uint value)
    {
        IO.Write(value.ToString());
    }

    Hopper()
    {
        uint i;
        uint j;
        uint k;
        uint q;
        uint x;
        uint nines;
        uint predigit;
        uint[len] a;
        bool firstDigit = true; // Flag to handle the first digit specially

        IO.WriteLn();
        IO.Write("Pi to ");
        WriteUInt(n);
        IO.WriteLn(" places:");
        IO.WriteLn();
        uint start = Seconds;

        // Initialize array
        for (j = 0; j < len; j++) 
        {
            a[j] = 2; // Start with 2s
        }

        nines = 0;
        predigit = 0; // First predigit is 0

        for (j = 1; j <= n; j++)
        {
            q = 0;
            for (i = len; i > 0; i--)
            {
                x = 10 * a[i - 1] + q * i;
                a[i - 1] = x % (2 * i - 1);
                q = x / (2 * i - 1);
            }

            a[0] = q % 10;
            q = q / 10;

            if (q == 9)
            {
                nines++;
            }
            else if (q == 10)
            {
                if (firstDigit)
                {
                    IO.Write("3.");
                    firstDigit = false;
                }
                else
                {
                    WriteUInt(predigit + 1);
                }
                for (k = 1; k <= nines; k++) 
                {
                    WriteUInt(0); // Zeros
                }
                predigit = 0;
                nines = 0;
            }
            else
            {
                if (firstDigit)
                {
                    IO.Write("3.");
                    WriteUInt(predigit);
                    firstDigit = false;
                }
                else
                {
                    WriteUInt(predigit);
                }
                predigit = q;
                if (nines != 0)
                {
                    for (k = 1; k <= nines; k++) 
                    {
                        WriteUInt(9);
                    }
                    nines = 0;
                }
            }
        }

        WriteUInt(predigit);
        uint elapsed = (Seconds - start); 
        
        IO.WriteLn();
        IO.WriteLn("Done.");
        
        IO.WriteLn(elapsed.ToString() + " seconds");
    }
}

