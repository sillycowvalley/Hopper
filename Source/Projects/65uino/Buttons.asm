unit Buttons
{
    
    Initialize()
    {
        // buttons as inputs
        LDA RIOT.DDRA
        AND # 0b11110011
        STA RIOT.DDRA
    }
    ButtonOneDown()
    {
        LDA RIOT.DRA
        AND # 0b00000100
    }
    ButtonTwoDown()
    {
        LDA RIOT.DRA
        AND # 0b00001000
    }
}
