program Zopper
{
  uses "/Source/System/System"
  uses "/Source/System/Screen"
  uses "/Source/System/Keyboard"
  uses "/Source/Compiler/Tokens/Token"
  uses "/Source/Compiler/Tokens/Scanner"
  uses "/Source/Z80/Chunks"
  uses "/Source/Z80/Parser"

  
  {
    loop
    {
      <string> args = System.Arguments;
      if (args.Length != 1)
      {
        PrintLn("Invalid arguments for ZOPPER:");
        PrintLn("  ZOPPER <source file>");
        break;
      }
      string sourcePath =args[0];
      if (!File.Exists(sourcePath))
      {
        PrintLn("Source file not found for ZOPPER:");
        PrintLn("  ZOPPER <source file>");
        break;
      }
      
      long startTime = Millis;
      
      Chunks.New();
      
      loop
      {
          // first pass
          if (Parser.BuildSymbols(sourcePath))
          {
             // error!
             break;
          }
          
          // second pass
          PrintLn();
          if (Parser.Compile(sourcePath))
          {
             // error!
             PrintLn("Failed! ", Color.MatrixRed, Color.LightGray);
             break;
          }
          PrintLn("Success. ", Color.MatrixGreen, Color.LightGray);
          
          <string> sourceList = Parser.GetSourceList();
          
          string extension = Path.GetExtension(sourcePath);
          string zasmPath = sourcePath.Replace(extension, ".zasm");
          zasmPath = Path.GetFileName(zasmPath);
          zasmPath = Path.Combine("/Debug/", zasmPath);
          Chunks.Disassemble(zasmPath, sourceList);
          PrintLn("Disassembled output saved to '" +zasmPath +"'");
          
          string ihexPath = sourcePath.Replace(extension, ".hex");
          ihexPath = Path.GetFileName(ihexPath);
          ihexPath = Path.Combine("/Bin/", ihexPath);
          uint bytesOfCode = Chunks.WriteIHex(ihexPath);
          PrintLn("IHex saved to '" +ihexPath +"'");
          Print(bytesOfCode.ToString() +" bytes of code", Color.MatrixBlue, Color.SlateBlue);
          
          string objPath = sourcePath.Replace(extension, ".obj");
          objPath = Path.GetFileName(objPath);
          objPath = Path.Combine("/Bin/", objPath);
          Chunks.WriteObj(objPath);
          PrintLn("Obj saved to '" +objPath +"'");
          
          long elapsedTime = Millis - startTime;
          float seconds = elapsedTime / 1000.0;
          PrintLn("  " +seconds.ToString() + "s", Color.MatrixBlue, Color.SlateBlue); 
          
          break;
      }
      
      break;
    }
  }
}
