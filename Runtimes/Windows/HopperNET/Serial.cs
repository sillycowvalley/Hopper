

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
using System.Runtime.InteropServices;
using Microsoft.Win32.SafeHandles;
using System.ComponentModel;

namespace HopperNET
{
    
    class Serial
    {
        static bool continueSerial;
        static SerialPort serialPort;
        static string lastPort;
        static List<string> btPorts = new List<string>();

        static bool isHopperCOM0Server = false; // Portable Runtime or Emulator
        static bool isHopperCOM0Client = false; // HM, Debug or Term

        static NamedPipeServerStream com0Writer = null;
        static NamedPipeClientStream com0Reader = null;

        static string ipCPipeName = "/Temp/IPCPipeName.bin";

        private static bool CheckCom0Exists()
        {
            bool com0Exists = false;
            if (ipCPipeName.Contains("/"))
            {
                ipCPipeName = HopperPath.ToWindowsPath(ipCPipeName);
            }
            uint count = 0;
            for (; ; )
            {
                Thread.Sleep(10);
                if (File.Exists(ipCPipeName))
                {
                    com0Exists = true;
                    break;
                }
                count++;
                if (count == 100)
                {
                    break;
                }
            }
            return com0Exists;
        }

        private static NamedPipeServerStream CreatePipeServerStream(bool server, bool manageName)
        {
            if (ipCPipeName.Contains("/"))
            {
                ipCPipeName = HopperPath.ToWindowsPath(ipCPipeName);
                if (manageName)
                {
                    File.Delete(ipCPipeName);
                }
            }
            NamedPipeServerStream result = null;
            if (manageName)
            {
                uint iName = 0;
                for (; ; )
                {
                    string comPipeName = iName.ToString("X8");
                    try
                    {
                        result = new NamedPipeServerStream((server ? "com0Server-" : "com0Client-") + comPipeName, PipeDirection.Out);
                    }
                    catch (Exception ex)
                    {
                        iName++;
                        continue;
                    }
                    File.WriteAllText(ipCPipeName, comPipeName);
                    break;
                }
            }
            else
            {
                for (; ; )
                {
                    Thread.Sleep(100);
                    if (File.Exists(ipCPipeName))
                    {
                        break;
                    }
                }
                string comPipeName = File.ReadAllText(ipCPipeName);
                try
                {
                    result = new NamedPipeServerStream((server ? "com0Server-" : "com0Client-") + comPipeName, PipeDirection.Out);
                }
                catch (Exception ex)
                {
                    int why = 0;
                }
            }
            return result;
        }

        private static NamedPipeClientStream CreatePipeClientStream(bool server)
        {
            if (ipCPipeName.Contains("/"))
            {
                ipCPipeName = HopperPath.ToWindowsPath(ipCPipeName);
            }
            for (; ;)
            {
                Thread.Sleep(100);
                if (File.Exists(ipCPipeName))
                {
                    break;
                }
            }
            string comPipeName = File.ReadAllText(ipCPipeName);
            return new NamedPipeClientStream(".", (server ? "com0Client-" : "com0Server-") + comPipeName, PipeDirection.In);
        }

        static Thread pipeThread = null;
        static bool pipeThreadExit = false;
        static Queue<char> pipeReadBuffer = new Queue<char>();

        static void PipeThreadReader()
        {
            lock (pipeReadBuffer)
            {
                pipeReadBuffer.Clear();
            }
            while (!isHopperCOM0Server && !isHopperCOM0Client)
            {
                // spin
            }
            if (null == com0Reader)
            {
                if (isHopperCOM0Server)
                {
                    com0Reader = CreatePipeClientStream(true);
                }
                if (isHopperCOM0Client)
                {
                    com0Reader = CreatePipeClientStream(false);

                }
                com0Reader.Connect();
            }

            while (!pipeThreadExit)
            {
                if ((isHopperCOM0Server || isHopperCOM0Client) && (com0Reader != null))
                {
                    if (com0Reader.IsConnected)
                    {
                        break;
                    }
                }
                Thread.Sleep(10);
            }
            while (!pipeThreadExit)
            {
                if ((isHopperCOM0Server || isHopperCOM0Client) && (com0Reader != null))
                {
                    if (!com0Reader.IsConnected)
                    {
                        break;
                    }
                    try
                    {
                        int ibyte = com0Reader.ReadByte();
                        if (ibyte != -1)
                        {
                            lock (pipeReadBuffer)
                            {
                                pipeReadBuffer.Enqueue((char)ibyte);
                            }
                        }
                        else
                        {
                            Thread.Sleep(10);
                        }
                    }
                    catch (Exception ex)
                    {
                        int why = 0;
                    }
                }
            }
            if (null != com0Reader)
            {
                com0Reader.Close();
                com0Reader = null;
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
            List<String> list = new List<string>();
            try
            {
                // assume server never calls GetPorts(), only HM, Debug or Term

                // already connected?
                bool com0Exists = (com0Reader != null) && com0Reader.IsConnected;
                if (!com0Exists)
                {
                    // is it possible to connect?
                    com0Exists = CheckCom0Exists();
                }
                if (com0Exists)
                {
                    list.Add("COM0");
                }
            }
            catch (Exception ex)
            {
                Diagnostics.OutputDebug("client connect: " + ex.Message);
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
                if (null == com0Writer)
                {
                    if (pipeThread == null)
                    {
                        pipeThread = new Thread(new ThreadStart(PipeThreadReader));
                        pipeThread.Start();
                    }
                    com0Writer = CreatePipeServerStream(true, true);
                    isHopperCOM0Server = true; // Portable Runtime or Emulator
                    isHopperCOM0Client = false; // HM, Debug or Term
                }
                return;
            }
            else if (portName == "COM0")
            {
                //if (pipeThread != null)
                //{
                //    Com0Close(); // abort existing thread
                //}
                if (pipeThread == null)
                {
                    pipeThread = new Thread(new ThreadStart(PipeThreadReader));
                    pipeThread.Start();
                }
                try
                {
                    if (null == com0Writer)
                    {
                        com0Writer = CreatePipeServerStream(false, false);
                    }
                    if (null != com0Writer)
                    {
                        isHopperCOM0Server = false; // Portable Runtime or Emulator
                        isHopperCOM0Client = true; // HM, Debug or Term
                    }
                }
                catch (Exception ex)
                {
                    Diagnostics.OutputDebug("client connect: " + ex.Message);
                }
                return;
            }
            else 
            {
                if (isHopperCOM0Client || isHopperCOM0Server)
                {
                    Com0Close();
                }

                // any real COM port
                isHopperCOM0Client = false;

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

        public static void Com0Close()
        {
            if (isHopperCOM0Client || isHopperCOM0Server)
            {
                if (pipeThread != null)
                {
                    pipeThreadExit = true;
                    pipeThread.Abort();
                    pipeThread = null;
                }
                if (null != com0Writer)
                {
                    com0Writer.Close();
                    com0Writer = null;
                }
            }
            if (File.Exists(ipCPipeName) && isHopperCOM0Server)
            {
                File.Delete(ipCPipeName);
            }
            isHopperCOM0Client = false;
            isHopperCOM0Server = false;
        }

        public static void Close()
        {
            if (isHopperCOM0Client || isHopperCOM0Server)
            {
                Com0Close();
            }
            else if (IsValid())
            {
                serialPort.Close();
            }

        }
        public static bool IsValid()
        {
            bool isValid = false;
            try
            {
                if (isHopperCOM0Client || isHopperCOM0Server)
                {
                    if (!com0Writer.IsConnected)
                    {
                        try
                        {
                            com0Writer.WaitForConnection();
                        }
                        catch (Exception ex)
                        {
                            int why = 0;
                        }
                    }
                    isValid = com0Writer.IsConnected;
                    if (isValid && isHopperCOM0Client)
                    {
                        isValid = (null != com0Reader) && com0Reader.IsConnected;
                    }
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
                    if (isHopperCOM0Client || isHopperCOM0Server)
                    {
                        com0Writer.Write(b, 0, 1);
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
                    if (isHopperCOM0Client || isHopperCOM0Server)
                    {
                        com0Writer.Write(b, 0, b.Length);
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
                    if (isHopperCOM0Client || isHopperCOM0Server)
                    {
                        // read is blocking
                        for (; ; )
                        {
                            lock (pipeReadBuffer)
                            {
                                if (pipeReadBuffer.Count != 0)
                                {
                                    break;
                                }
                            }
                        }
                        lock (pipeReadBuffer)
                        {
                            readChar = (char)pipeReadBuffer.Dequeue();
                        }
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
                    if (isHopperCOM0Client || isHopperCOM0Server)
                    {
                        lock (pipeReadBuffer)
                        {
                            isAvail  = pipeReadBuffer.Count != 0;
                        }
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
