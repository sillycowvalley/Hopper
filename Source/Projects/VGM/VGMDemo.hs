program SNVGM
{
    //uses "/Source/Library/Boards/PiPico"
    uses "/Source/Library/Boards/Hopper6502"
    
    
    uses "/Source/Library/Audio/SNVGMPlayer"

    //uses "Samples/Ghost"
    uses "Samples/GB"
    //uses "Samples/Christmas"
    //uses "Samples/Sonata"
    //uses "Samples/AlleyCat"
    //uses "Samples/Kings"
    //uses "Samples/BruceLee"
    //uses "Samples/allaturca"
        
    Hopper()
    {
        Initialize();
        Play(VGMDATA);
        Silence();
    }
}
