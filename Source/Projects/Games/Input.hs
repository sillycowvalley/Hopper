unit Input
{
    uses "/Source/System/Keyboard"
    
    <Key> keyQueue;
    
    Initialize()
    {
    }
    
    Update()
    {
        if (Keyboard.IsAvailable)
        {
            Key key = Keyboard.ReadKey();
            switch (key)
            {
                case Key.Left:
                case Key.Right:
                case Key.Up:
                case Key.Down:
                case Key.Space:
                case Key.Escape:
                {
                    keyQueue.Append(key);
                }
            }
        }
    }
    
    bool Left { get {
        if ((keyQueue.Count != 0) && (keyQueue[0] == Key.Left))
        {
            keyQueue.Remove(0);
            return true;
        }
        return false;
    } }
    bool Right { get {
        if ((keyQueue.Count != 0) && (keyQueue[0] == Key.Right))
        {
            keyQueue.Remove(0);
            return true;
        }
        return false;
    } }
    bool Up { get {
        if ((keyQueue.Count != 0) && (keyQueue[0] == Key.Up))
        {
            keyQueue.Remove(0);
            return true;
        }
        return false;
    } }
    bool Down { get {
        if ((keyQueue.Count != 0) && (keyQueue[0] == Key.Down))
        {
            keyQueue.Remove(0);
            return true;
        }
        return false;
    } }
    bool Space { get {
        if ((keyQueue.Count != 0) && (keyQueue[0] == Key.Space))
        {
            keyQueue.Remove(0);
            return true;
        }
        return false;
    } }
    bool Exit { get {
        if ((keyQueue.Count != 0) && (keyQueue[0] == Key.Escape))
        {
            keyQueue.Remove(0);
            return true;
        }
        return false;
    } }
    
    Clear()
    {
        keyQueue.Clear();
    }
    
}
