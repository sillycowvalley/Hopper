program PiSpigot
{
    #define SERIAL_CONSOLE
    uses "/Source/System/System"
    uses "/Source/System/IO"

    const uint n = 1000;
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

        IO.WriteLn();
        IO.Write("Pi to ");
        WriteUInt(n);
        IO.WriteLn(" places:");
        IO.WriteLn();
        long start = Millis;

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
                WriteUInt(predigit + 1);
                for (k = 1; k <= nines; k++) 
                {
                    WriteUInt(0); // Zeros
                }
                predigit = 0;
                nines = 0;
            }
            else
            {
                WriteUInt(predigit);
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
        IO.WriteLn();
        IO.WriteLn("Done.");

        float ms = (Millis - start) / 10.0; 
        IO.WriteLn(ms.ToString() + " ms");
    }
}

