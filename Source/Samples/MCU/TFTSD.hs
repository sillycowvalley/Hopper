program TFTandSDdemo
{
    uses "/Source/Library/Boards/SparkfunProMicroRP2040"
    uses "/Source/Library/Devices/Adafruit240x135ColorTFT"
    
    uses "/Source/Library/Graphics/Vectors"
    
    uses "/Source/Library/Fonts/Hitachi5x7"
    
    const string lorumIpsum = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Suspendisse iaculis tortor vitae imperdiet tempus. Quisque eget sapien ex. Donec molestie tincidunt sem imperdiet condimentum. Nulla facilisi. Class aptent taciti sociosqu ad litora vestibulum.";

    DrawText()
    {
        IO.WriteLn("  DrawText");
        EchoToLCD = true;
        Screen.ForeColour = Colour.Black;
        Screen.BackColour = Colour.White;
        Screen.Clear();
        IO.WriteLn(lorumIpsum);
        IO.WriteLn();
        Screen.ForeColour = Colour.Red;
        IO.WriteLn(lorumIpsum);
        Screen.ForeColour = Colour.Black;
        EchoToLCD = false;
    }
    
    DrawRGB()
    {
        IO.WriteLn("  DrawRGB");
        int pw3  = Display.PixelWidth/3;
        int pw23 = Display.PixelWidth*2/3;
        int w = pw3;
        if (Display.PixelWidth % 3 != 0)
        {
            w++;
        }
        FilledRectangle(0,    0, w, Display.PixelHeight, Colour.Red);
        FilledRectangle(pw3,  0, w, Display.PixelHeight, Colour.Green);
        FilledRectangle(pw23, 0, w, Display.PixelHeight, Colour.Blue);
    }
    DrawCMYK()
    {
        IO.WriteLn("  DrawCMYK");
        int pw2  = Display.PixelWidth/2;
        int pw34 = Display.PixelWidth*3/4;
        int pw4  = Display.PixelWidth/4;
        int w = pw4;
        if (Display.PixelWidth % 4 != 0)
        {
            w++;
        }
        FilledRectangle(0,    0, w, Display.PixelHeight, Colour.Cyan);
        FilledRectangle(pw4,  0, w, Display.PixelHeight, Colour.Magenta);
        FilledRectangle(pw2,  0, w, Display.PixelHeight, Colour.Yellow);
        FilledRectangle(pw34, 0, w, Display.PixelHeight, Colour.Black);
    }
    
    DrawShades()
    {
        IO.WriteLn("  DrawShades");
        int pw2  = Display.PixelWidth/2;
        int pw34 = Display.PixelWidth*3/4;
        int pw4  = Display.PixelWidth/4;
        int w = pw4;
        if (Display.PixelWidth % 4 != 0)
        {
            w++;
        }
        Display.Suspend();
        FilledRectangle(0,    0, w, Display.PixelHeight, Colour.White);
        FilledRectangle(pw4,  0, w, Display.PixelHeight, Colour.LightGray);
        FilledRectangle(pw2,  0, w, Display.PixelHeight, Colour.DarkGray);
        FilledRectangle(pw34, 0, w, Display.PixelHeight, Colour.Black);
        Display.Resume(); 
    }
    
    {
        //IsPortrait = true;
        //FlipY = true;
        
        //FlipX = true;
        //FlipY = true;
        
        DeviceDriver.SDCS = Board.GP29;
        DeviceDriver.CS   = Board.SPI0SS;
        DeviceDriver.DC   = Board.GP28;
        
        if (!DeviceDriver.Begin())
        {
            IO.WriteLn("Failed to initialize display");
            return;
        }

        if (!SD.Mount())
        {
            IO.WriteLn("Failed to mount SD card");
            return;
        }
        
        loop
        {
            /*
            DrawRGB();
            DelaySeconds(3);
            
            DrawCMYK();
            DelaySeconds(3);
            
            DrawShades();
            DelaySeconds(3);
            
            DrawText();
            DelaySeconds(3);
            */
            
            <string> imagePaths;
            
            directory dir = Directory.Open("/sd/content/");
            if (dir.IsValid())
            {
                IO.WriteLn("Images on SD card:");
                uint count = dir.GetFileCount();
                for (uint i = 0; i < count; i++)
                {
                    string imagePath = dir.GetFile(i);
                    long   size     = File.GetSize(imagePath);
                    IO.WriteLn("  " + imagePath.Pad(' ', 32) + size.ToString() + " bytes");
                    imagePaths.Append(imagePath);
                }
            }
            foreach (var imagePath in imagePaths)
            {
                //if (!imagePath.Contains("tigger")) { continue; }
                if (!imagePath.EndsWith(".bin")) { continue; }
                IO.WriteLn("Render Image:");
                IO.WriteLn("  " + Path.GetFileName(imagePath));
                if (!Vectors.Header(imagePath))
                {
                    IO.WriteLn("  Issue with header");
                    continue;
                }
                
                long start = Millis;
                
                
                Display.Clear(Vectors.BackColour);
                
                int dx = (Display.PixelWidth  - Vectors.Width) / 2;
                int dy = (Display.PixelHeight - Vectors.Height) / 2;
                Vectors.Render(dx, dy);
                long elapsed = Millis - start;
                WriteLn("Elapsed: " + elapsed.ToString());
                
                IO.Write("  ");
                for (uint i = 0; i < 5; i++)
                {                
                    IO.Write(".");
                    Time.DelaySeconds(1);
                }
                IO.WriteLn();
                
                
            }
        }
    }
}
