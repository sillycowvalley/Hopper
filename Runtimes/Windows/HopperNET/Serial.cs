using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.IO.Ports;
using System.Management;

namespace HopperNET
{
    class Serial
    {
        static bool continueSerial;
        static SerialPort serialPort;
        static string lastPort;
        static List<string> btPorts = new List<string>();

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
                serialPort.Close();
            }
        }
        public static bool IsValid()
        {
            bool isValid = false;
            try
            {
                if ((null == serialPort) && !String.IsNullOrEmpty(lastPort))
                {
                    // try reconnecting
                    connect(lastPort);
                }
                isValid = (null != serialPort) && serialPort.IsOpen;
            }
            catch (Exception ex)
            {
                Diagnostics.OutputDebug("IsValid: " + ex.Message);
            }
            return isValid;
        }

        public static void WriteChar(char outChar)
        {
            string text = outChar.ToString();
            if (IsValid())
            {
                try
                {
                    serialPort.Write(text);
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
                    readChar = (char)serialPort.ReadChar();
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
                    if (serialPort.BytesToRead > 0)
                    {
                        isAvail = true;
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
