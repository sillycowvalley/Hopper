unit ClickStack
{
    uses "/Source/System/System"
    
    <string> clickStack;
    
    Push(string path, uint ln)
    {
        string location = path + ":" + ln.ToString();
        clickStack.Append(location);
    }
    Pop()
    {
        uint length = clickStack.Length;
        if (0 != length)
        {
            string location = clickStack[length-1];
            clickStack.Remove(length-1);
            ClickStack.Load(location);
            
            // don't keep this Load(..) in the stack:
            length = clickStack.Length;
            if (0 != length)
            {
                clickStack.Remove(length-1);
            }
        }
    }
    Load(string location)
    {
        <string> parts = location.Split(':');
        if (parts.Length == 2)
        {
            string sourcePath = parts[0];
            string currentPath = Editor.GetCurrentPath();
            uint ln;
            bool gotoLine = false;
            if (UInt.TryParse(parts[1], ref ln))
            {
                gotoLine = true;
            }
            if (sourcePath.ToLower() != currentPath.ToLower())
            {
                if (Editor.CanUndo())
                {
                    Editor.OpenPath(sourcePath); // offer undo
                }
                else
                {
                    Editor.LoadFile(sourcePath);
                }
                currentPath = Editor.GetCurrentPath();
                gotoLine = (sourcePath.ToLower() == currentPath.ToLower());
            }
            if (gotoLine)
            {
                if (Editor.GotoLineNumber(ln))
                {
                }
            }
        }
    }
}
