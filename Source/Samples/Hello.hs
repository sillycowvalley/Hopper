program Hello
{
    uses "/Source/System/String"
    uses "/Source/System/Screen"
    uses "/Source/System/Keyboard"
    {
        Screen.Clear();
        string src = "Hello Witch!";
        string build;
        foreach (var c in src)
        {
            String.Build(ref build, c);
        }
        PrintLn("Before:" + build);
        build = build.ToUpper();
        PrintLn("After:" + build);
        for (uint i=0; i < 20; i++)
        {
            PrintLn(build);
        }
        Key k = ReadKey();
    }
}
