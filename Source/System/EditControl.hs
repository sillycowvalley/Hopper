unit EditControl
{
    uses "System"
    uses "Screen"
#ifndef SERIAL_CONSOLE
    uses "Keyboard"
#endif    
    
    delegate bool ValidEditCharacter(char c);
#ifndef SERIAL_CONSOLE    
    uint foreColor = Colour.MatrixGreen;
    uint backColor = Colour.Black;
#endif    
    ValidEditCharacter validate;

    SetValidation(ValidEditCharacter validator)
    {
        validate = validator;
    }
#ifndef SERIAL_CONSOLE
    SetColours(uint fore, uint back)
    {
        foreColor = fore;
        backColor = back;
    }
#endif

    bool OnKey(Key key, uint leftX, uint fieldWidth, ref string textContent, ref uint currentX)
    {
        bool consumed = false;
        uint pos = currentX - leftX;
        uint currentWidth = textContent.Length;
#ifdef SERIAL_CONSOLE
        uint fieldPos = pos;
        uint previousWidth = currentWidth;
#endif
        
        bool isShifted = (Key.Shift == (key & Key.Shift));
        bool isControlled = (Key.Control == (key & Key.Control));
        bool isAlted = (Key.Alt == (key & Key.Alt));
        Key unmaskedKey = (key & Key.Mask);
        
        if (unmaskedKey == Key.Click)
        {
            uint cx = Keyboard.ClickX;
            uint cy = Keyboard.ClickY;
            //OutputDebug(cx.ToString() + " " + cy.ToString());
        }
        
        switch (unmaskedKey)
        {
            case Key.Backspace:
            {
                if (pos > 0)
                {
                    string newText;
                    if (pos > 1)
                    {
                        // keep what's left of cursor-1
                        newText = textContent.Substring(0, pos-1);
                    }
                    if (pos < currentWidth)
                    {
                        // append what is right of cursor
                        newText = newText + textContent.Substring(pos);
                    }
                    pos--;
                    textContent = newText;
                    consumed = true;
                }
            }
            case Key.Delete:
            {
                if (pos < currentWidth)
                {
                    string newText;
                    if (pos > 0)
                    {
                        // keep what's left of cursor
                        newText = textContent.Substring(0, pos);
                    }
                    if (pos+1 < currentWidth)
                    {
                        // append what is right of cursor+1
                        newText = newText + textContent.Substring(pos+1);
                    }
                    textContent = newText;
                    consumed = true;
                }
            }
            case Key.Left:
            {
                if (pos > 0)
                {
                    pos--;
                    consumed = true;
                }
            }
            case Key.Home:
            {
                if (pos > 0)
                {
                    pos = 0;
                    consumed = true;
                }
            }
            case Key.Right:
            {
                if (pos < currentWidth)
                {
                    pos++;
                    consumed = true;
                }
            }
            case Key.End:
            {
                if (pos < currentWidth)
                {
                    pos = currentWidth;
                    consumed = true;
                }
            }
            default:
            {
                if (currentWidth+1 < fieldWidth)
                {
                    if (isAlted || isControlled)
                    {
                        // don't convert <ctrl><a> to 'a', etc
                    }
                    else
                    {
                        if (key == (Key.ModSpace | Key.Shift))
                        {
                            key = Key.Space;
                        }
                        uint  k = uint(key);
                        if ((k > 31) && (k < 128))
                        {
                            char c = char(key);
                            if (validate(c))
                            {
                                textContent = textContent.InsertChar(pos, c);
                                pos++;
                                consumed = true;
                            }
                        }
                    }
                }
            } // default
        } // switch (key)
        
        if (consumed)
        {
            // redraw
#ifdef SERIAL_CONSOLE 
            currentX = leftX + pos;
            uint padWidth = 0;
            if (fieldWidth > 0)
            {
                padWidth = fieldWidth - 1;
            }
            currentWidth = textContent.Length;
            string paddedText = textContent.Pad(' ', previousWidth);
            uint paddedLength = paddedText.Length;
            
            while (fieldPos > 0)
            {
                IO.Write(char(0x08));
                fieldPos--;
            }
            for (uint x=0; x < paddedLength; x++)
            {
                IO.Write(paddedText[x]);
                fieldPos++;
            } 
            /*
            uint backs = paddedLength - currentWidth;
            while (backs > 0)
            {
                IO.Write(char(0x08));
                backs--;
                fieldPos--;
            }
            */
            while (fieldPos > pos)
            {
                IO.Write(char(0x08));
                fieldPos--;
            }
#else
            Screen.Suspend();
            uint y = Screen.CursorY;
            currentX = leftX + pos;
            uint padWidth = 0;
            if (fieldWidth > 0)
            {
                padWidth = fieldWidth - 1;
            }
            currentWidth = textContent.Length;
            string paddedText = textContent.Pad(' ', fieldWidth-1);
            uint paddedLength = paddedText.Length;
            for (uint x=0; x < paddedLength; x++)
            {
                Screen.DrawChar(leftX+x, y, paddedText[x], foreColor, backColor);
            }
            Screen.SetCursor(currentX, y);
            Screen.Resume(true);
#endif
        }
        return consumed;
    }
    
/*
    bool OnKey(char ch, uint leftX, uint fieldWidth, ref string textContent, ref uint currentX)
    {
        bool consumed = false;
        if (ch == char(0x08))
        {
            if (currentX > leftX)
            {
                IO.Write(char(0x08) + " " + char(0x08));
                currentX--;
                textContent = textContent.Substring(0, textContent.Length - 1);
            }
            consumed = true;
        }
        else 
        {
            uint currentWidth = currentX - leftX;
            if (currentWidth+1 < fieldWidth)
            {
                if ((ch > char(31)) && (ch < char(128)))
                {
                    if (validate(ch))
                    {
                        IO.Write(ch);
                        currentX++;
                        textContent = textContent +  ch;
                        consumed = true;
                    }
                }
            }
        }
        return consumed;
    }
*/    

}
