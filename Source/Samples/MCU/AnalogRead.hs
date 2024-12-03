program AnalogRead
{
    uses "/Source/Library/Boards/ChallengerNB2040WiFi"
    
    {
        loop
        {
            uint a0 = A0;
            uint a1 = A1;
            uint a2 = A2;
            uint a3 = A3;
            
            WriteLn(a0.ToString() + ", " + a1.ToString() + ", " + a2.ToString() + ", " + a3.ToString());
            Delay(50);
        }
    }
}
