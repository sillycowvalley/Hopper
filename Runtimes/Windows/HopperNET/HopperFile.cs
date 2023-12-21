using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.IO;
using System.Reflection;

namespace HopperNET
{
    public class HopperPath
    {
        static string hopperRoot;
        public static void InitializeFolders()
        {
            if (Directory.Exists(@"C:\Repos\Hopper\"))
            {
                hopperRoot = @"C:\Repos\Hopper\";             // C drive dev location
            }
            else if (Directory.Exists(@"D:\Repos\Hopper"))
            {
                hopperRoot = @"D:\Repos\Hopper";             // D drive dev location
            }
            Assembly currentAssem = Assembly.GetExecutingAssembly();
            string exePath = currentAssem.Location;
            string exeFolder = Path.GetDirectoryName(exePath);
            string subFolder = Path.GetFileName(exeFolder);
            exeFolder = Path.GetDirectoryName(exeFolder);
            if (subFolder == "Bin")
            {
                hopperRoot = exeFolder;
            }
            string tempFolder = Path.Combine(hopperRoot, "Temp");
            if (!Directory.Exists(tempFolder))
            {
                Directory.CreateDirectory(tempFolder);
            }
            string debugFolder = Path.Combine(hopperRoot, "Debug");
            if (!Directory.Exists(debugFolder))
            {
                Directory.CreateDirectory(debugFolder);
            }
            string objFolder = Path.Combine(debugFolder, "Obj");
            if (!Directory.Exists(objFolder))
            {
                Directory.CreateDirectory(objFolder);
            }
        }
        public static string ToWindowsPath(string path)
        {
            if (!String.IsNullOrEmpty(path) && (path[0] != '/'))
            {
                path = HopperSystem.CurrentDirectory + path;
            }
            path = path.Replace("/", @"\");
            path = hopperRoot + path;
            return path;
        }
        public static string ToHopperPath(string path)
        {
            if (!path.Contains(hopperRoot))
            {
                throw new InvalidDataException();
            }
            path = path.Replace(hopperRoot, "");
            path = path.Replace(@"\", "/");
            return path;
        }
        public static bool IsCanonicalFullPath(String fullPath)
        {
            bool result = false;
            for (;;) // single exit point
            {
                if (String.IsNullOrEmpty(fullPath))
                {
                    break;
                }
                if (fullPath[0] != '/')
                {
                    break; // all full paths start with '/'
                }
                //ushort index = 0;
                //if (!String_IndexOf(fullPath, L'/', &index))
                //{
                //    break; // empty subFolder name not allowed
                //}
                result = true;
                break;
            }
            return result;
        }
    }
    public class HopperFile : Variant
    {
        public HopperFile()
        {
            Type = HopperType.tFile;
        }
        public override Variant Clone()
        {
            HopperFile clone = new HopperFile();
            clone.isValid = isValid;
            clone.path = path;
            clone.pos = pos;
            clone.reading = reading;
            clone.writing = writing;
            clone.bytes = bytes;
            clone.content = content;
            return clone;
        }
#if DEBUG
        public override void Validate()
        {
            Diagnostics.ASSERT(Type == HopperType.tFile, "HopperFile validation failed");
        }
#endif

        bool isValid;
        byte[] bytes;
        List<byte> content;
        Int32 pos = 0;
        string path;
        bool reading;
        bool writing;
        public static bool Exists(string path)
        {
            return File.Exists(HopperPath.ToWindowsPath(path));
        }
        public static void Delete(string path)
        {
            path = HopperPath.ToWindowsPath(path);
            if (File.Exists(path))
            {
                File.Delete(path);
            }
        }
        public static Int32 GetSize(string path)
        {
            long length = 0;
            path = HopperPath.ToWindowsPath(path);
            if (File.Exists(path))
            {
                FileInfo fi = new FileInfo(path);
                length = fi.Length;
            }
            return (Int32)length;
        }
        

        public byte Read()
        {
            byte b = 0;
            if (reading && isValid && (pos < bytes.Length))
            {
                b = bytes[pos];
                pos++;
            }
            else
            {
                isValid = false;
            }
            return b;
        }
        public string ReadLine()
        {
            string ln = "";
            if (reading && isValid && (pos < bytes.Length))
            {
                for (; ; )
                {
                    if (pos == bytes.Length)
                    {
                        isValid = ln.Length > 0; // EOF
                        break;
                    }
                    byte buffer = bytes[pos];
                    pos++;
                    if (buffer == 0x0D)
                    {
                        continue;
                    }
                    if (buffer == 0x0A)
                    {
                        break;
                    }
                    ln = ln + (char)buffer;
                }
            }
            else
            {
                isValid = false;
            }
            return ln;
        }
        public byte Read(Int32 seekpos)
        {
            byte b = 0;
            if (reading && isValid && (seekpos < bytes.Length))
            {
                b = bytes[seekpos];
            }
            else
            {
                isValid = false;
            }
            return b;
        }
        public void Append(byte b)
        {
            if (writing && isValid)
            {
                content.Add(b);
            }
            else
            {
                isValid = false;
            }
        }
        public void Append(string str)
        {
            if (writing && isValid)
            {
                foreach (char c in str)
                {
                    content.Add((byte)c);
                }
            }
            else
            {
                isValid = false;
            }
        }
        public void Flush()
        {
            if (writing && isValid)
            {
                File.WriteAllBytes(path, content.ToArray());
            }
            else
            {
                isValid = false;
            }
        }
        public bool IsValid()
        {
            return isValid;
        }

        public static HopperFile Open(string path)
        {
            path = HopperPath.ToWindowsPath(path);
            HopperFile hopperFile = new HopperFile();
            if (File.Exists(path))
            {
                hopperFile.bytes = File.ReadAllBytes(path);
                hopperFile.isValid = true;
                hopperFile.reading = true;
                hopperFile.writing = false;
            }
            return hopperFile;
        }
        public static HopperFile Create(string path)
        {
            path = HopperPath.ToWindowsPath(path);
            HopperFile hopperFile = new HopperFile();
            if (File.Exists(path))
            {
                File.Delete(path);
                hopperFile.isValid = true;
            }
            hopperFile.path = path;
            hopperFile.isValid = true;
            hopperFile.reading = false;
            hopperFile.writing = true;
            hopperFile.content = new List<Byte>();
            return hopperFile;
        }
        public static Int32 GetTime(string path)
        {
            long filetime = 0;
            path = HopperPath.ToWindowsPath(path);
            if (File.Exists(path))
            {
                FileInfo fi = new FileInfo(path);
                DateTime dt = fi.LastWriteTime;
                long unixTime = ((DateTimeOffset)dt).ToUnixTimeSeconds();
                filetime = unixTime;
            }
            return (Int32)filetime;
        }

    }

    public class HopperDirectory : Variant
    {
        public static Int32 GetTime(string path)
        {
            long filetime = 0;
            path = HopperPath.ToWindowsPath(path);
            if (Directory.Exists(path))
            {
                FileInfo fi = new FileInfo(path);
                DateTime dt = fi.LastWriteTime;
                long unixTime = ((DateTimeOffset)dt).ToUnixTimeSeconds();
                filetime = unixTime;
            }
            return (Int32)filetime;
        }

        public HopperDirectory()
        {
            Type = HopperType.tDirectory;
        }
        bool isValid;
        string path;
#if DEBUG
        public override void Validate()
        {
            Diagnostics.ASSERT(Type == HopperType.tDirectory, "HopperDirectory validation failed");
        }
#endif
        public override Variant Clone()
        {
            HopperDirectory clone = new HopperDirectory();
            clone.isValid = isValid;
            clone.path = path;
            return clone;
        }

        public static bool Exists(string path)
        {
            return Directory.Exists(HopperPath.ToWindowsPath(path));
        }
        public static HopperDirectory Open(string fullDirectoryPath)
        {
            HopperDirectory directory = new HopperDirectory();
            if (!HopperPath.IsCanonicalFullPath(fullDirectoryPath))
            {
                return directory;
            }
            string path = HopperPath.ToWindowsPath(fullDirectoryPath);
            if (Directory.Exists(path))
            {
                directory.isValid = true;
                directory.path = path;
            }
            return directory;
        }
        public bool IsValid()
        {
            return isValid;
        }
        public static void Create(string path)
        {
            path = HopperPath.ToWindowsPath(path);
            HopperDirectory directory = new HopperDirectory();
            if (Directory.Exists(path))
            {
                directory.isValid = true;
                directory.path = path;
            }
            else
            {
                try
                {
                    DirectoryInfo info = Directory.CreateDirectory(path);
                    directory.path = path;
                    directory.isValid = true;
                }
                catch (IOException)
                {
                    // something went wrong so !isValid
                }
            }
        }
        public static void Delete(string path)
        {
            path = HopperPath.ToWindowsPath(path);
            if (Directory.Exists(path))
            {
                try
                {
                    Directory.Delete(path);
                }
                catch (IOException)
                {
                    // something went wrong
                }
            }
        }

        public ushort GetDirectoryCount()
        {
            ushort count = 0;
            if (isValid)
            {
                count = (ushort)Directory.GetDirectories(path).Length; // this API correctly does not return pseudo directories ".." and "."
            }
            return count;
        }

        public HopperString GetDirectory(ushort index)
        {
            HopperString directory = new HopperString();
            if (isValid)
            {
                ushort count = 0;
                foreach (String dir in Directory.GetDirectories(path))
                {
                    if (count == index)
                    {
                        directory.Value = HopperPath.ToHopperPath(dir);
                        if (!dir.EndsWith(@"\"))
                        {
                            directory.Value += "/";
                        }
                        break;
                    }
                    count++;
                }
            }
            return directory; // full hopper path, leading and trailing /
        }

        public HopperString GetFile(ushort index)
        {
            HopperString file = new HopperString();
            if (isValid)
            {
                ushort count = 0;
                foreach (String f in Directory.GetFiles(path))
                {
                    if (count == index)
                    {
                        file.Value = HopperPath.ToHopperPath(f);
                        break;
                    }
                    count++;
                }
            }
            return file; // full hopper path
        }

        public ushort GetFileCount()
        {
            ushort count = 0;
            if (isValid)
            {
                count = (ushort)Directory.GetFiles(path).Length;
            }
            return count;
        }
        
    }
}
