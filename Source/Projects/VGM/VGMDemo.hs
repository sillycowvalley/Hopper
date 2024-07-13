program SNVGM
{
    uses "/Source/Library/Boards/Hopper6502"
    uses "/Source/Library/Audio/SNVGMPlayer"

    //uses "Samples/Ghost"
    //uses "Samples/Christmas"
    //uses "Samples/Sonata"
    uses "Samples/AlleyCat"
        
    Hopper()
    {
        Initialize();
        Play(VGMDATA);
        Silence();
    }
}
