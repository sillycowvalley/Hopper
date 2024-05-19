unit Input
{
    uses "/Source/System/Keyboard"
    
    <Key> keyQueue;
    
    Initialize()
    {
    }
    
    Update()
    {
#ifndef MCU
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
#endif
    }
    
    bool Left { get {
#ifdef MCU
        return Button0;
#else            
        if ((keyQueue.Count != 0) && (keyQueue[0] == Key.Left))
        {
            keyQueue.Remove(0);
            return true;
        }
        return false;
#endif
    } }
    bool Right { get {
#ifdef MCU
        return Button3;
#else                
        if ((keyQueue.Count != 0) && (keyQueue[0] == Key.Right))
        {
            keyQueue.Remove(0);
            return true;
        }
        return false;
#endif
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
#ifdef MCU
        return Button2;
#else                
        if ((keyQueue.Count != 0) && (keyQueue[0] == Key.Down))
        {
            keyQueue.Remove(0);
            return true;
        }
        return false;
#endif
    } }
    bool Space { get {
#ifdef MCU
        return Button1;
#else                
        if ((keyQueue.Count != 0) && (keyQueue[0] == Key.Space))
        {
            keyQueue.Remove(0);
            return true;
        }
        return false;
#endif
    } }
    bool Exit { get {
#ifdef MCU
        return false;
#else                
        if ((keyQueue.Count != 0) && (keyQueue[0] == Key.Escape))
        {
            keyQueue.Remove(0);
            return true;
        }
        return false;
#endif
    } }
    
    Clear()
    {
        keyQueue.Clear();
    }
    
}
