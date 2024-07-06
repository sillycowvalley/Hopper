program LCDDemo
{
    uses "/Source/Minimal/System"
    uses "/Source/Library/Boards/Hopper6502"
    uses "/Source/Library/Displays/HD44780"
    
    Hopper()
    {
        LCD.Initialize();
        foreach (var c in "Hello Hopper!")
        {
            LCD.Character(c);
        }
    }
}
