program Hello
{
    uses "/Source/System/Screen"
    uses "/Source/System/Keyboard"
    {
        Screen.Clear();
        for (uint i=0; i < 20; i++)
        {
            PrintLn("Hello Witch!");
        }
        Key k = ReadKey();
    }
}
