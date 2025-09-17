program NRLBenchmark
{
    #define CPU_65C02S
    
    uses "System/Definitions"
    uses "System/Print"
    uses "System/Time"
    uses "System/Long"
    uses "System/Serial"
    uses "System/Shared"
    
    // Zero Page Variable Allocation (using free slots 0x59-0x6F)
    // Document our zero page usage clearly
    const byte iCounter        = 0x59;  // Outer loop counter (1-10)
    const byte jCounterL       = 0x5A;  // Inner loop counter low byte
    const byte jCounterH       = 0x5B;  // Inner loop counter high byte (1-1000)
    
    const byte sumL            = 0x5C;  // Running sum low byte
    const byte sumH            = 0x5D;  // Running sum high byte
    const byte sumU            = 0x5E;  // Running sum upper byte
    const byte sumT            = 0x5F;  // Running sum top byte (32-bit)
    
    const byte startMillis0    = 0x60;  // Start time in milliseconds
    const byte startMillis1    = 0x61;
    const byte startMillis2    = 0x62;
    const byte startMillis3    = 0x63;
    
    const byte startSeconds0   = 0x64;  // Start time in seconds
    const byte startSeconds1   = 0x65;
    const byte startSeconds2   = 0x66;
    const byte startSeconds3   = 0x67;
    
    // String constants
    const string titleString = "Noel's RetroLab Benchmark\n";
    const string msString = " ms\n";
    const string secondsString = " seconds\n";
    
    saveStartTimes()
    {
        // Get start time in seconds
        Time.Seconds();
        LDA ZP.TOP0
        STA startSeconds0
        LDA ZP.TOP1
        STA startSeconds1
        LDA ZP.TOP2
        STA startSeconds2
        LDA ZP.TOP3
        STA startSeconds3
        
        // Get start time in milliseconds
        Time.Millis();
        LDA ZP.TOP0
        STA startMillis0
        LDA ZP.TOP1
        STA startMillis1
        LDA ZP.TOP2
        STA startMillis2
        LDA ZP.TOP3
        STA startMillis3
    }
    
    loadSumToNext()
    {
        LDA sumL
        STA ZP.NEXT0
        LDA sumH
        STA ZP.NEXT1
        LDA sumU
        STA ZP.NEXT2
        LDA sumT
        STA ZP.NEXT3
    }
    
    saveSumFromNext()
    {
        LDA ZP.NEXT0
        STA sumL
        LDA ZP.NEXT1
        STA sumH
        LDA ZP.NEXT2
        STA sumU
        LDA ZP.NEXT3
        STA sumT
    }
    
    loadSumToTop()
    {
        LDA sumL
        STA ZP.TOP0
        LDA sumH
        STA ZP.TOP1
        LDA sumU
        STA ZP.TOP2
        LDA sumT
        STA ZP.TOP3
    }
    
    loadJCounterToTop()
    {
        // Use ACC as intermediate for 16-bit j counter
        LDA jCounterL
        STA ZP.ACCL
        LDA jCounterH
        STA ZP.ACCH
        Shared.MoveAccToTop();  // This zeros TOP2 and TOP3 automatically
    }
    
    loadStartMillisToTop()
    {
        LDA startMillis0
        STA ZP.TOP0
        LDA startMillis1
        STA ZP.TOP1
        LDA startMillis2
        STA ZP.TOP2
        LDA startMillis3
        STA ZP.TOP3
    }
    
    loadStartSecondsToTop()
    {
        LDA startSeconds0
        STA ZP.TOP0
        LDA startSeconds1
        STA ZP.TOP1
        LDA startSeconds2
        STA ZP.TOP2
        LDA startSeconds3
        STA ZP.TOP3
    }
    
    Hopper()
    {
        // Print title
        LDA #(titleString % 256)
        STA ZP.STRL
        LDA #(titleString / 256)
        STA ZP.STRH
        Print.String();
        
        saveStartTimes();
        
        // Initialize outer loop counter (i = 1)
        LDA #1
        STA iCounter
        
        // Outer loop: FOR i=1 TO 10
        loop
        {
            // Initialize sum to 0 (s = 0)
            STZ sumL
            STZ sumH
            STZ sumU
            STZ sumT
            
            // Initialize inner loop counter (j = 1)
            LDA #1
            STA jCounterL
            STZ jCounterH
            
            // Inner loop: FOR j=1 TO 1000
            loop
            {
                // s = s + j using BIOS Long.Add
                loadSumToNext();
                loadJCounterToTop();
                
                // NEXT = NEXT + TOP
                Long.Add();
                
                saveSumFromNext();
                
                // j++ (increment 16-bit counter)
                INC jCounterL
                if (Z)
                {
                    INC jCounterH
                }
                
                // Check if j > 1000
                LDA jCounterH
                CMP #(1000 / 256)
                if (Z)
                {
                    LDA jCounterL
                    CMP #((1000 % 256) + 1)
                    if (Z)
                    {
                        // j reached 1001, exit inner loop
                        break;
                    }
                }
                else
                {
                    if (C)
                    {
                        // High byte > expected, exit
                        break;
                    }
                }
            } // inner loop
            
            // Print progress dot
            LDA #'.'
            Print.Char();
            
            // i++
            INC iCounter
            
            // Check if i > 10
            LDA iCounter
            CMP #11
            if (Z)
            {
                // i reached 11, exit outer loop
                break;
            }
        } // outer loop
        
        // Print newline after dots
        Print.NewLine();
        
        // Print final sum value
        loadSumToTop();
        Long.Print();
        Print.NewLine();
        
        // Calculate and print elapsed milliseconds
        Time.Millis();
        Shared.MoveTopToNext();  // Current millis now in NEXT
        loadStartMillisToTop();  // Start millis in TOP
        
        // NEXT = NEXT - TOP (elapsed time)
        Long.Sub();
        
        // Print elapsed milliseconds (result is in NEXT, move to TOP for printing)
        Shared.MoveNextToTop();
        Long.Print();
        
        // Print " ms"
        LDA #(msString % 256)
        STA ZP.STRL
        LDA #(msString / 256)
        STA ZP.STRH
        Print.String();
        
        // Calculate and print elapsed seconds
        Time.Seconds();
        Shared.MoveTopToNext();  // Current seconds now in NEXT
        loadStartSecondsToTop(); // Start seconds in TOP
        
        // NEXT = NEXT - TOP (elapsed time)
        Long.Sub();
        
        // Print elapsed seconds (result is in NEXT, move to TOP for printing)
        Shared.MoveNextToTop();
        Long.Print();
        
        // Print " seconds"
        LDA #(secondsString % 256)
        STA ZP.STRL
        LDA #(secondsString / 256)
        STA ZP.STRH
        Print.String();
    }
}
