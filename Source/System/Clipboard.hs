unit Clipboard
{
    bool HasText { get  system; }
    char   GetChar() system;
#ifndef RUNTIME    
    string GetText() system;
    SetText(string text) system;
#endif
    
}
