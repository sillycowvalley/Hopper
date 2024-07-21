program SNVGM
{
    uses "/Source/Library/Boards/PiPico"
    uses "/Source/Library/Audio/SNVGMPlayer"
    
    uses "/Source/System/Directory"
    uses "/Source/System/File"
    uses "/Source/System/Path"

    Hopper()
    {
        LED = false;
        SNVGMPlayer.Initialize();
        
        loop
        {
            IO.WriteLn();
            IO.WriteLn("Samples:");
            IO.WriteLn();
            directory dir = Directory.Open("/Samples/");
            uint tracks = dir.GetFileCount();
            uint index;
            <uint,string> trackPaths;
            string filePath;
            for (uint track = 0; track < tracks; track++)
            {
                filePath = dir.GetFile(track);
                string extension =  (Path.GetExtension(filePath)).ToLower();
                if (extension == ".vg")
                {
                    trackPaths[index] = filePath;
                    IO.WriteLn(index.ToString() + " : " + filePath);
                    index++;
                }
            }
            loop
            {
                char ch = IO.Read();
                if (Char.IsDigit(ch))
                {
                    // '0'..'9'
                    index = byte(ch)-48;
                    // 0..9
                    if (index < trackPaths.Count) { break; }
                }
                if ((ch == char(0x03)) || (ch == char(0x1B))) // <ctrl><C> or <esc>
                {
                    return;
                }
            }                
            
            filePath = trackPaths[index];
            IO.WriteLn();
            SNVGMPlayer.Play(filePath);
            SNVGMPlayer.Silence();
        }
    }
}

