program TestFiles
{
#define PORTABLE
    uses "/Source/System/System"
    
    uses "/Source/System/IO"
    uses "/Source/System/Diagnostics"
    uses "/Source/System/Screen"
    uses "/Source/System/Keyboard"
    
    PrintFailed(string message)
    {
        PrintLn("  " + message, MatrixRed, 0);
    }
    
    TestFiles()
    {
        WriteLn("File text IO");
        
        string testPath = "/temp/testfile.txt";
        if (File.Exists(testPath))
        {
            File.Delete(testPath);
        }
        
        file testFile = File.Create(testPath);
        if (!testFile.IsValid())
        {
            PrintFailed("File.Create(..) failed");
        }
        File.Append(testFile, "Test Content");
        if (!testFile.IsValid())
        {
            PrintFailed("File.Append(..) failed");
        }
        File.Flush(testFile);
        if (!testFile.IsValid())
        {
            PrintFailed("File.Flush(..) failed");
        }
        
        file testFile2 = File.Open(testPath);
        if (!testFile2.IsValid())
        {
            PrintFailed("File.Open(..) failed");
        }
        string ln = testFile2.ReadLine();
        if (!testFile2.IsValid())
        {
            PrintFailed("File.ReadLine(..) failed");
        }
        if (ln != "Test Content")
        {
            PrintLn("'" + ln + "'");
            PrintFailed("File text IO failed");
        }
        
        long pos = 0;
        ln = "";
        loop
        {
            byte b = File.Read(testFile2, pos);
            if (!testFile2.IsValid())
            {
                break;
            }
            ln = ln + char(b);
            pos = pos + 1;
        }
        if (ln != "Test Content")
        {
            PrintLn("'" + ln + "'");
            PrintFailed("File char IO failed");
        }
        
        WriteLn("File byte IO");
        
        if (File.Exists(testPath))
        {
            File.Delete(testPath);
        }
        
        testFile = File.Create(testPath);
        if (!testFile.IsValid())
        {
            PrintFailed("File.Create(..) failed");
        }
        byte content = 42;
        File.Append(testFile, content);
        if (!testFile.IsValid())
        {
            PrintFailed("File.Append(..) failed");
        }
        File.Flush(testFile);
        if (!testFile.IsValid())
        {
            PrintFailed("File.Flush(..) failed");
        }
        
        testFile2 = File.Open(testPath);
        if (!testFile2.IsValid())
        {
            PrintFailed("File.Open(..) failed");
        }
        content = testFile2.Read();
        if (!testFile2.IsValid())
        {
            PrintFailed("File.Read(..) failed");
        }
        if (content != 42)
        {
            PrintLn("'" + content.ToString() + "'");
            PrintFailed("File byte IO failed");
        }
        
        if (File.Exists(testPath))
        {
            File.Delete(testPath);
        }
    }
    
    TestDirectories()
    {
        WriteLn("Directories");
        
        Directory.Delete("/temp/testfolder");
        Directory.Delete("/temp/testfolder2");
        Directory.Delete("/temp/testfolder3");
        
        
        string testPath = "/temp/";
        if (!Directory.Exists(testPath))
        {
            PrintFailed("Directory.Exists(..) failed 1");
        }
        
        testPath = "/temp/testfolder";
        Directory.Create(testPath);
        
        directory testDirectory = Directory.Open(testPath);
        if (!testDirectory.IsValid())
        {
            PrintFailed("Directory.Create(..) failed");    
        }
        if (!Directory.Exists(testPath))
        {
            PrintFailed("Directory.Exists(..) failed 2");
        }
        
        file testFile = File.Create("/temp/testfolder/file1.txt");
        if (!testFile.IsValid())
        {
            PrintFailed("File.Create(..) failed");
        }
        File.Append(testFile, "Test Content 1");
        if (!testFile.IsValid())
        {
            PrintFailed("File.Append(..) failed");
        }
        File.Flush(testFile);
        testFile = File.Create("/temp/testfolder/file2.txt");
        if (!testFile.IsValid())
        {
            PrintFailed("File.Create(..) failed");
        }
        File.Append(testFile, "Test Content 2");
        if (!testFile.IsValid())
        {
            PrintFailed("File.Append(..) failed");
        }
        File.Flush(testFile);
        testFile = File.Create("/temp/testfolder/file3.txt");
        if (!testFile.IsValid())
        {
            PrintFailed("File.Create(..) failed");
        }
        File.Append(testFile, "Test Content 3");
        if (!testFile.IsValid())
        {
            PrintFailed("File.Append(..) failed");
        }
        File.Flush(testFile);
        Directory.Create("/temp/testfolder2");
        Directory.Create("/temp/testfolder3");
        
        directory sourceDirectory = Directory.Open("/temp/testfolder");
        if (!sourceDirectory.IsValid())
        {
            PrintFailed("Directory.Open(..) failed");    
        }
        uint fileCount = sourceDirectory.GetFileCount();
        if (fileCount != 3)
        {
            PrintFailed("Directory.GetFileCount(..) failed");    
        }
        string str = "";
        for (uint i = 0; i < fileCount; i ++)
        {
            str = str + sourceDirectory.GetFile(i);
        }
        if (str.Length < 30)
        {
            PrintFailed("Directory.GetFile(..) failed");    
        }
        while (sourceDirectory.GetFileCount() > 0)
        {
            File.Delete(sourceDirectory.GetFile(0));
        }
        
        
        sourceDirectory = Directory.Open("/temp");
        uint directoryCount = sourceDirectory.GetDirectoryCount();
        if (directoryCount != 3)
        {
            PrintFailed("Directory.GetDirectoryCount(..) failed");    
        }
        str = "";
        for (uint i = 0; i < directoryCount; i ++)
        {
            str = str + sourceDirectory.GetDirectory(i);
        }
        if (str.Length < 30)
        {
            PrintFailed("Directory.GetDirectory(..) failed");    
        }
        Directory.Delete("/temp/testfolder2");
        Directory.Delete("/temp/testfolder3");
        
        long timestamp1 = Directory.GetTime("/");
        long timestamp2 = Directory.GetTime("/temp/testfolder");
        if (timestamp1 == 0)
        {
            PrintFailed("Directory.GetTime(..) failed");    
        }
        if (timestamp1 >= timestamp2)
        {
            PrintFailed("Directory.GetTime(..) failed");    
        }
        
        Directory.Delete(testPath);
        if (Directory.Exists(testPath))
        {
            PrintFailed("Directory.Delete(..) failed");
        }
        
        
    }
    
    {
        EchoToLCD = true;
        Screen.Clear();
        
        TestDirectories();
        TestFiles();
        
        // File:
        //  long GetSize(string path) system;
        //  long GetTime(string path) system;
        //  bool Exists(ref string filePath, ref string extension, string searchFolder)
        
        WriteLn();
        WriteLn("TestSuite Ok");
        
        //Key k = ReadKey();
    }
}
