program AnalogWrite
{
    //uses "/Source/Library/Boards/AdaFruitItsyBitsy"
    //uses "/Source/Library/Boards/AdaFruitQTPy"
    uses "/Source/Library/Boards/AdaFruitFeather"
    
    const byte ledA = A0;
    const byte ledB = A1;
    {
        AnalogWriteResolution(10);
        uint fade;
        bool flip;
        loop
        {
            fade = (flip ? fade - 1 : fade + 1);
            if ((fade & 0x03FF) == 0x0000)
            {
                flip = !flip;
                fade = (flip ? fade - 1 : fade + 1);   
            }
            AnalogWrite(ledA, fade);
            AnalogWrite(ledB, 0x03FF-fade);
            Delay(5);
        }
    }
}
