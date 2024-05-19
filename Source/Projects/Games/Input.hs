unit Input
{
    uses "/Source/System/Keyboard"
    
    <Key> keyQueue;
    
    Initialize()
    {
    }
    
#ifdef MCU    
    ButtonISR(byte pin, PinStatus status)
    {
        if (status == PinStatus.Rising)
        {
            switch (PinToButton(pin))
            {
                case "Key0": { keyQueue.Append(Key.Left);  } 
                case "Key1": { keyQueue.Append(Key.Space); } 
                case "Key2": { keyQueue.Append(Key.Down);  } 
                case "Key3": { keyQueue.Append(Key.Right); } 
            }   
        }
    }
#endif    
    
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
