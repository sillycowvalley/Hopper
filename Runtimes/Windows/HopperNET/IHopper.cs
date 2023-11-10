namespace HopperNET
{
    public interface IHopper
    {
        bool Exiting { get; set; }
        void HopperInvalidate();
        bool HasClipboardText();
        void SetClipboardText(string text);
        string GetClipboardText();
    }
}