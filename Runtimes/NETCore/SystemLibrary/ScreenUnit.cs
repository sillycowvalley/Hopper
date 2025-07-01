public class ScreenUnit
{
    public uint ForeColour { get; set; } = 0x07F7; // Default from bytecode
    public uint BackColour { get; set; } = 0;

    public void Print(string text, uint foreColor, uint backColor)
    {
        // For now, ignore colors and just print to console
        Console.Write(text);
    }

    public void PrintLn()
    {
        Console.WriteLine();
    }
}