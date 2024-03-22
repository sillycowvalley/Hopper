

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

        static NamedPipeServerStream com0Server = null;
        static NamedPipeClientStream com0Client = null;

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

        private static NamedPipeServerStream CreatePipeServerStream()
        {
            if (ipCPipeName.Contains("/"))
            {
                ipCPipeName = HopperPath.ToWindowsPath(ipCPipeName);
                File.Delete(ipCPipeName);
            }
            NamedPipeServerStream result = null;
            uint iName = 0;
            for (; ; )
            {
                string comPipeName = iName.ToString("X8");
                try
                {
                    result = new NamedPipeServerStream("com0Server-" + comPipeName, PipeDirection.InOut, NamedPipeServerStream.MaxAllowedServerInstances, PipeTransmissionMode.Byte, PipeOptions.Asynchronous);
                    File.WriteAllText(ipCPipeName, comPipeName);
                    result.WaitForConnection();
                }
                catch (Exception ex)
                {
                    iName++;
                    continue;
                }
                break;
            }
            return result;
        }
        private static NamedPipeClientStream CreatePipeClientStream()
        {
            if (ipCPipeName.Contains("/"))
            {
                ipCPipeName = HopperPath.ToWindowsPath(ipCPipeName);
            }
            NamedPipeClientStream result = null;
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
                result = new NamedPipeClientStream(".", "com0Server-" + comPipeName, PipeDirection.InOut, PipeOptions.Asynchronous);
            }
            catch (Exception ex)
            {
                int why = 0;
            }
            return result;
        }

        public static void Com0Close()
        {
            /*
            if (isHopperCOM0Client || isHopperCOM0Server)
            {
                pipeThreadExit = true;
                if ((pipeReadThread != null) || (pipeWriteThread != null))
                {
                    if (pipeReadThread != null)
                    {
                        pipeReadThread.Abort();
                        pipeReadThread = null;
                    }
                    if (pipeWriteThread != null)
                    {
                        pipeWriteThread.Abort();
                        pipeWriteThread = null;
                    }
                }
                if (isHopperCOM0Client && (null != com0Client))
                {
                    com0Client.Close();
                    com0Client = null;
                }
                if (isHopperCOM0Server && (null != com0Server))
                {
                    com0Server.Close();
                    com0Server = null;
                }
            }
            if (File.Exists(ipCPipeName) && isHopperCOM0Server)
            {
                File.Delete(ipCPipeName);
            }
            */
            isHopperCOM0Client = false;
            isHopperCOM0Server = false;
        }


        static Thread pipeReadThread = null;
        static bool pipeThreadExit = false;
        static Queue<char> pipeReadBuffer = new Queue<char>();

        static Thread pipeWriteThread = null;
        static Queue<char> pipeWriteBuffer = new Queue<char>();

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
            while (!pipeThreadExit)
            {
                if (isHopperCOM0Server && (com0Server != null))
                {
                    if (com0Server.IsConnected)
                    {
                        break;
                    }
                }
                if (isHopperCOM0Client && (com0Client != null))
                {
                    if (com0Client.IsConnected)
                    {
                        break;
                    }
                }
                Thread.Sleep(10);
            }
            while (!pipeThreadExit)
            {
                if (isHopperCOM0Server && (com0Server != null))
                {
                    if (!com0Server.IsConnected)
                    {
                        break;
                    }
                    try
                    {
                        int ibyte = com0Server.ReadByte();
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
                if (isHopperCOM0Client && (com0Client != null))
                {
                    if (!com0Client.IsConnected)
                    {
                        break;
                    }
                    try
                    {
                        int ibyte = com0Client.ReadByte();
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
        }

        static void PipeThreadWriter()
        {
            byte[] b = new byte[1];

            lock (pipeWriteBuffer)
            {
                pipeWriteBuffer.Clear();
            }
            while (!isHopperCOM0Server && !isHopperCOM0Client)
            {
                // spin
            }
            while (!pipeThreadExit)
            {
                if (isHopperCOM0Server && (com0Server != null))
                {
                    if (com0Server.IsConnected)
                    {
                        break;
                    }
                }
                if (isHopperCOM0Client && (com0Client != null))
                {
                    if (com0Client.IsConnected)
                    {
                        break;
                    }
                }
                Thread.Sleep(10);
            }
            while (!pipeThreadExit)
            {
                bool isEmpty = false;
                lock (pipeWriteBuffer)
                {
                    isEmpty = (pipeWriteBuffer.Count == 0);
                }
                if (isEmpty)
                {
                    Thread.Sleep(10);
                    continue;
                }
                if (isHopperCOM0Server && (com0Server != null))
                {
                    if (!com0Server.IsConnected)
                    {
                        break;
                    }
                    char c;
                    lock (pipeWriteBuffer)
                    {
                        c = pipeWriteBuffer.Dequeue();
                    }
                    try
                    {
                        b[0] = (byte)c;
                        com0Server.Write(b, 0, 1);
                    }
                    catch (Exception ex)
                    {
                        Thread.Sleep(10);
                    }
                }
                if (isHopperCOM0Client && (com0Client != null))
                {
                    if (!com0Client.IsConnected)
                    {
                        break;
                    }
                    char c;
                    lock (pipeWriteBuffer)
                    {
                        c = pipeWriteBuffer.Dequeue();
                    }
                    try
                    {
                        b[0] = (byte)c;
                        com0Client.Write(b, 0, 1);
                    }
                    catch (Exception ex)
                    {
                        Thread.Sleep(10);
                    }
                }
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
                bool com0Exists = (com0Client != null) && com0Client.IsConnected;
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
                if (null == com0Server)
                {
                    if (pipeReadThread == null)
                    {
                        pipeReadThread = new Thread(new ThreadStart(PipeThreadReader));
                        pipeReadThread.Start();
                        pipeWriteThread = new Thread(new ThreadStart(PipeThreadWriter));
                        pipeWriteThread.Start();
                    }
                    com0Server = CreatePipeServerStream();
                }
                if (null != com0Server)
                {
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
                if (pipeReadThread == null)
                {
                    pipeReadThread = new Thread(new ThreadStart(PipeThreadReader));
                    pipeReadThread.Start();
                    pipeWriteThread = new Thread(new ThreadStart(PipeThreadWriter));
                    pipeWriteThread.Start();
                }
                try
                {
                    if (null == com0Client)
                    {
                        com0Client = CreatePipeClientStream();
                    }
                    if (null != com0Client)
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
                if (isHopperCOM0Server)
                {
                    /*
                    if (!com0Server.IsConnected)
                    {
                        try
                        {
                            com0Server.WaitForConnection();
                        }
                        catch (Exception ex)
                        {
                            int why = 0;
                        }
                    }
                    isValid = com0Server.IsConnected;
                    */
                    isValid = com0Server.CanRead && com0Server.CanWrite;
                }
                else if (isHopperCOM0Client)
                {
                    if (!com0Client.IsConnected)
                    {
                        try
                        {
                            com0Client.Connect();
                        }
                        catch (Exception ex)
                        {
                            int why = 0;
                        }
                    }
                    isValid = com0Client.IsConnected;
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
            if (IsValid())
            {
                if (isHopperCOM0Server || isHopperCOM0Client)
                {
                    pipeWriteBuffer.Enqueue(outChar);
                }
                else
                {
                    try
                    {
                        byte[] b = new byte[1];
                        b[0] = (byte)outChar;
                        serialPort.Write(b, 0, 1);
                    }
                    catch (Exception ex)
                    {
                        Diagnostics.OutputDebug("WriteChar: " + ex.Message);
                    }
                }
            }
        }
        public static void WriteString(HopperString str)
        {
            if (IsValid())
            {
                if (isHopperCOM0Server || isHopperCOM0Client)
                {
                    for (int i = 0; i < str.Value.Length; i++)
                    {
                        lock (pipeWriteBuffer)
                        {
                            pipeWriteBuffer.Enqueue(str.Value[i]);
                        }
                    }
                }
                else
                {
                    try
                    {
                        byte[] b = new byte[str.Value.Length];
                        for (int i = 0; i < str.Value.Length; i++)
                        {
                            b[i] = (byte)str.Value[i];
                        }
                        serialPort.Write(b, 0, b.Length);
                    }
                    catch (Exception ex)
                    {
                        Diagnostics.OutputDebug("WriteChar: " + ex.Message);
                    }
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
