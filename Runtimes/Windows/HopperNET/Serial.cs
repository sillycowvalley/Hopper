using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.IO.Ports;
using System.Management;
using System.IO.Pipes;
using System.IO;
using System.Threading;

namespace HopperNET
{
    public static class MutexHelper
    {
        private static Mutex m_mutex = new Mutex(false, "Global\\HopperIPCMutex");

        public static void Take()
        {
            try
            {
                m_mutex.WaitOne();
            }
            catch (AbandonedMutexException)
            {

            }
        }

        public static void Leave()
        {
            m_mutex.ReleaseMutex();
        }
    }

    class Serial
    {
        static bool continueSerial;
        static SerialPort serialPort;
        static string lastPort;
        static List<string> btPorts = new List<string>();

        static bool isHopperCOM0Server = false; // Portable Runtime
        static bool isHopperCOM0Client = false; // HM, Debug or Term

        static string ipcRunning = "/Temp/IPCisRunning.bin";
        static string ipcServerToClient = "/Temp/IPCserverToClient.bin";
        static string ipcClientToServer = "/Temp/IPCclientToServer.bin";
        
        private static void AtomicWrite(string path, char outChar)
        {
            try
            {
                MutexHelper.Take();

                if (!File.Exists(path))
                {
                    // create
                    var bytes = new byte[1];
                    bytes[0] = (byte)outChar;
                    File.WriteAllBytes(path, bytes);
                }
                else
                {
                    // append
                    var existingBytes = File.ReadAllBytes(path);
                    var bytes = new byte[existingBytes.Length + 1];
                    for (int i = 0; i < existingBytes.Length; i++)
                    {
                        bytes[i] = existingBytes[i];
                    }
                    bytes[existingBytes.Length] = (byte)outChar;
                    File.WriteAllBytes(path, bytes);
                }
            }
            finally
            {
                MutexHelper.Leave();
            }
            //string verbose = ((byte)outChar > 31) ? " '" + outChar + "'" : "";
            //Diagnostics.OutputDebug("\nAtomicWrite: " + ((byte)outChar).ToString("X2") + verbose);
        }

        private static void AtomicWrite(string path, byte[] bytes)
        {
            try
            {
                MutexHelper.Take();

                if (!File.Exists(path))
                {
                    // create
                    File.WriteAllBytes(path, bytes);
                }
                else
                {
                    // append
                    var existingBytes = File.ReadAllBytes(path);
                    var newBytes = new byte[existingBytes.Length + bytes.Length];
                    int index = 0;
                    foreach (byte b in existingBytes)
                    {
                        newBytes[index] = b;
                        index++;
                    }
                    foreach (byte b in bytes)
                    {
                        newBytes[index] = b;
                        index++;
                    }
                    File.WriteAllBytes(path, newBytes);
                }
            }
            finally
            {
                MutexHelper.Leave();
            }
            //Diagnostics.OutputDebug("\nAtomicWrite: " + (bytes.Length).ToString("") + " bytes");
        }
        static string atomicReadBuffer = String.Empty;
        
        private static char AtomicRead(string path)
        {
            char readChar = ' ';
            if (atomicReadBuffer.Length > 0)
            {
                readChar = atomicReadBuffer[0];
                atomicReadBuffer = atomicReadBuffer.Substring(1);
                return readChar;
            }
            while (!AtomicExists(path))
            {
                // read is blocking
            }
            try
            {
                MutexHelper.Take();
                while (!AtomicExists(path))
                {
                    // <-- in case it vanished in the non-atomic part : we may lock here ..
                }
                
                byte [] buffer = File.ReadAllBytes(path);
                foreach (var b in buffer)
                {
                    atomicReadBuffer = atomicReadBuffer + (char)b;
                }
                readChar = atomicReadBuffer[0];
                atomicReadBuffer = atomicReadBuffer.Substring(1);

                File.Delete(path);
            }
            finally
            {
                MutexHelper.Leave();
            }
            //string verbose = ((byte)readChar > 31) ? " '" + readChar + "'" : "";
            //Diagnostics.OutputDebug("\nAtomicRead: " + ((byte)readChar).ToString("X2") + verbose);
            return readChar;
        }

        private static bool AtomicExists(string path)
        {
            bool exists = false;
            try
            {
                MutexHelper.Take();
                exists = File.Exists(path);
            }
            finally
            {
                MutexHelper.Leave();
            }
            return exists;
        }
        private static void AtomicDelete(string path)
        {
            try
            {
                MutexHelper.Take();
                File.Delete(path);
            }
            finally
            {
                MutexHelper.Leave();
            }
        }



        public static void Connect()
        {
            String[] names = SerialPort.GetPortNames();
            string portName = names[names.Length - 1];
            connect(portName);
        }
        public static void Connect(uint port)
        {
            string portName = "COM" + port.ToString();
            connect(portName);
        }
        public static List<String> GetPorts()
        {
            if (ipcRunning.Contains("/"))
            {
                ipcClientToServer = HopperPath.ToWindowsPath(ipcClientToServer);
                ipcServerToClient = HopperPath.ToWindowsPath(ipcServerToClient);
                ipcRunning = HopperPath.ToWindowsPath(ipcRunning);
            }

            List<String> list = new List<string>();
            if (AtomicExists(ipcRunning))
            {
                list.Add("COM0");
            }
            if (btPorts.Count > 0)
            {
                String[] names = SerialPort.GetPortNames();
                foreach (string name in names)
                {
                    if (!btPorts.Contains(name))
                    {
                        list.Add(name);
                    }
                }
            }
            else
            {
                // stop adding Bluetooth ports to the list of COM ports (because the timeout on failing to connect is >= 1 minute)
                ManagementObjectSearcher serialSearcher = new ManagementObjectSearcher("root\\CIMV2", "SELECT * FROM Win32_SerialPort");
                var query = from ManagementObject s in serialSearcher.Get() select new { Name = s["Name"], DeviceID = s["DeviceID"], PNPDeviceID = s["PNPDeviceID"] };
                foreach (var port in query)
                {
                    //Console.WriteLine("{0} - {1}", port.DeviceID, port.Name);
                    var pnpDeviceId = port.PNPDeviceID.ToString();
                    if (!pnpDeviceId.Contains("BTHENUM"))
                    {
                        list.Add(port.DeviceID as String);
                    }
                    else
                    {
                        btPorts.Add(port.DeviceID as String); // cache the results since this is a slow process
                    }
                }
            }
            return list;
        }

        

        static void  connect(string portName)
        {
            if (portName == "COM4242")
            {
                if (ipcRunning.Contains("/"))
                {
                    ipcClientToServer = HopperPath.ToWindowsPath(ipcClientToServer);
                    ipcServerToClient = HopperPath.ToWindowsPath(ipcServerToClient);
                    ipcRunning = HopperPath.ToWindowsPath(ipcRunning);
                }

                isHopperCOM0Server = true;
                isHopperCOM0Client = false;
                AtomicDelete(ipcRunning);
                AtomicDelete(ipcServerToClient);
                AtomicDelete(ipcClientToServer);
                AtomicWrite(ipcRunning, '!');
                return;
            }
            else if (portName == "COM0")
            {
                if (ipcRunning.Contains("/"))
                {
                    ipcClientToServer = HopperPath.ToWindowsPath(ipcClientToServer);
                    ipcServerToClient = HopperPath.ToWindowsPath(ipcServerToClient);
                    ipcRunning = HopperPath.ToWindowsPath(ipcRunning);
                }

                isHopperCOM0Server = false;
                isHopperCOM0Client = true;
                AtomicDelete(ipcClientToServer);
                atomicReadBuffer = String.Empty;
                return;
            }
            else 
            {
                // any real COM port
                isHopperCOM0Client = false;
                AtomicDelete(ipcClientToServer);

            }

            if ((null != serialPort) && serialPort.IsOpen)
            {
                serialPort.Close(); // if it is already connected, close it first
                serialPort = null;
            }

            serialPort = new SerialPort();
            serialPort.PortName = portName;
            serialPort.BaudRate = 57600;
            serialPort.Parity = Parity.None;
            serialPort.DataBits = 8;
            serialPort.StopBits = StopBits.One;
            serialPort.Handshake = Handshake.None;

            // Pi Pico needs these:
            serialPort.RtsEnable = true;
            serialPort.DtrEnable = true;

            serialPort.ReadTimeout = 500;
            serialPort.WriteTimeout = 500;

            List<String> ports = GetPorts();
            if (!ports.Contains(portName))
            {
                return; // port doesn't exist so don't waste 60 seconds trying to connect to it
            }

            try
            {
                serialPort.Open();
                continueSerial = serialPort.IsOpen;
                lastPort = portName;
            }
            catch (Exception ex)
            {
                serialPort = null;
                Diagnostics.OutputDebug("connect: " + ex.Message);
            }
        }
        public static void Close()
        {
            if (IsValid())
            {
                if (isHopperCOM0Client)
                {
                    isHopperCOM0Client = false;
                    AtomicDelete(ipcClientToServer);
                }
                else if (isHopperCOM0Server)
                {
                    // clean up on server close
                    AtomicDelete(ipcServerToClient);
                    AtomicDelete(ipcClientToServer);
                    AtomicDelete(ipcRunning);
                    isHopperCOM0Server = false;
                }
                else
                {
                    serialPort.Close();
                }
            }
        }
        public static bool IsValid()
        {
            bool isValid = false;
            try
            {
                if (isHopperCOM0Client)
                {
                    isValid = AtomicExists(ipcRunning);
                }
                else if (isHopperCOM0Server)
                {
                    isValid = AtomicExists(ipcRunning);
                }
                else
                {
                    if ((null == serialPort) && !String.IsNullOrEmpty(lastPort))
                    {
                        // try reconnecting
                        connect(lastPort);
                    }
                    isValid = (null != serialPort) && serialPort.IsOpen;
                }
            }
            catch (Exception ex)
            {
                Diagnostics.OutputDebug("IsValid: " + ex.Message);
            }
            return isValid;
        }

        public static void WriteChar(char outChar)
        {
            byte[] b = new byte[1];
            b[0] = (byte)outChar;
            if (IsValid())
            {
                try
                {
                    if (isHopperCOM0Client)
                    {
                        // only one char at a time
                        AtomicWrite(ipcClientToServer, outChar);
                    }
                    else if (isHopperCOM0Server)
                    {
                        // only one char at a time
                        AtomicWrite(ipcServerToClient, outChar);
                    }
                    else
                    {
                        serialPort.Write(b, 0, 1);
                    }
                }
                catch (Exception ex)
                {
                    Diagnostics.OutputDebug("WriteChar: " + ex.Message);
                }
            }
        }
        public static void WriteString(HopperString str)
        {
            if (IsValid())
            {
                byte[] b = new byte[str.Value.Length];
                for (int i = 0; i < str.Value.Length; i++)
                {
                    b[i] = (byte)str.Value[i];
                }

                try
                {
                    if (isHopperCOM0Client)
                    {
                        // only one char at a time
                        AtomicWrite(ipcClientToServer, b);
                    }
                    else if (isHopperCOM0Server)
                    {
                        // only one char at a time
                        AtomicWrite(ipcServerToClient, b);
                    }
                    else
                    {
                        serialPort.Write(b, 0, b.Length);
                    }
                }
                catch (Exception ex)
                {
                    Diagnostics.OutputDebug("WriteChar: " + ex.Message);
                }
            }
        }


        public static char ReadChar()
        {
            char readChar = (char)0;
            if (IsValid())
            {
                try
                {
                    if (isHopperCOM0Client)
                    {
                        // read is blocking
                        readChar = AtomicRead(ipcServerToClient);
                    }
                    else if (isHopperCOM0Server)
                    {
                        // read is blocking
                        readChar = AtomicRead(ipcClientToServer);
                    }
                    else
                    {
                        readChar = (char)serialPort.ReadChar();
                    }
                }
                catch (Exception ex)
                {
                    Diagnostics.OutputDebug("ReadChar: " + ex.Message);
                }
            }
            return readChar;
        }

        internal static bool IsAvailableGet()
        {
            bool isAvail = IsValid();
            if (isAvail)
            {
                isAvail = false;
                try
                {
                    if (isHopperCOM0Client)
                    {
                        isAvail = !String.IsNullOrEmpty(atomicReadBuffer) || AtomicExists(ipcServerToClient);
                    }                            
                    else if (isHopperCOM0Server)
                    {
                        isAvail = AtomicExists(ipcClientToServer);
                    }
                    else
                    {
                        if (serialPort.BytesToRead > 0)
                        {
                            isAvail = true;
                        }
                    }
                }
                catch (Exception ex)
                {
                    serialPort = null;
                    Diagnostics.OutputDebug("IsAvailableGet: " + ex.Message);
                }
            }
            return isAvail;
        }

        
    }
}
