using System.IO.Ports;

namespace HopperNET
{
    internal static class Serial
    {
        static Dictionary<uint, string> portMap = new Dictionary<uint, string>();
        static SerialPort serialPort;
        static string lastPort;
        static string lastBaud;


        internal static void Close()
        {
            if (IsValid())
            {
                serialPort.Close();
            }
        }

        internal static void Connect()
        {
            List<string> portNames = GetPorts();
            string portName = portNames[portNames.Count - 1];
            connect(portName, "57600");
        }

        internal static void Connect(uint port)
        {
            GetPorts();
            string portName = portMap[port];
            connect(portName, "57600");
        }

        internal static void Connect(uint port, string baud)
        {
            GetPorts();
            string portName = portMap[port];
            connect(portName, baud);
        }

        static void connect(string portName, string baud)
        {
            if ((null != serialPort) && serialPort.IsOpen)
            {
                serialPort.Close(); // if it is already connected, close it first
                serialPort = null;
            }
            int baudRate = int.Parse(baud);
            serialPort = new SerialPort();
            serialPort.PortName = portName;
            serialPort.BaudRate = baudRate;
            //serialPort.BaudRate = 57600; //  57600 for 6502 machine (with 3.6MHz oscillator), 115200 for Arduino MEGA2560 apps from 8bitforce;
            //serialPort.BaudRate = 28800; //  28800  for 6502 machine (at 28K with 1.8Mhz oscillator)
            serialPort.Parity = Parity.None;
            serialPort.DataBits = 8;
            serialPort.StopBits = StopBits.One;
            serialPort.Handshake = Handshake.None;

            // Pi Pico needs these:
            serialPort.RtsEnable = true;
            serialPort.DtrEnable = true;

            serialPort.ReadTimeout = 500;
            serialPort.WriteTimeout = 500;

            if (!portMap.ContainsValue(portName))
            {
                return; // port doesn't exist so don't waste 60 seconds trying to connect to it
            }

            try
            {
                serialPort.Open();
                lastPort = portName;
                lastBaud = baud;
            }
            catch (Exception ex)
            {
                serialPort = null;
                Diagnostics.OutputDebug("connect: " + ex.Message);
            }
        }

        internal static List<string> GetPorts()
        {
            List<string> ports = new List<string>();

            var allPorts = SerialPort.GetPortNames();
            portMap = new Dictionary<uint, string>();

            if (Environment.OSVersion.Platform == PlatformID.Win32NT)
            {
                ports = new List<string>(allPorts);
                foreach (var port in ports)
                {
                    uint i = uint.Parse(port.Substring(3));
                    portMap[i] = port;
                }
            }
            if (Environment.OSVersion.Platform == PlatformID.Unix)
            {
                // Linux/macOS - filter for USB serial devices
                uint i = 0;
                foreach (string port in allPorts)
                {
                    if (port.Contains("ttyUSB") ||     // USB-to-serial adapters
                        port.Contains("ttyACM") ||     // USB CDC devices (like Pico)
                        port.Contains("usbserial") ||  // macOS USB serial
                        port.Contains("usbmodem")      // macOS USB modem (CDC)
                       )
                    {
                        i++;
                        portMap[i] = port;
                        ports.Add("COM" + i.ToString());
                    }
                }
            }
            return ports;
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

        internal static bool IsValid()
        {
            bool isValid = false;
            try
            {
                if ((null == serialPort) && !String.IsNullOrEmpty(lastPort))
                {
                    // try reconnecting
                    connect(lastPort, lastBaud);
                }
                isValid = (null != serialPort) && serialPort.IsOpen;
                //Diagnostics.OutputDebug("IsValid: " + (isValid ? "True" : "False"));
            }
            catch (Exception ex)
            {
                Diagnostics.OutputDebug("IsValid: " + ex.Message);
            }
            return isValid;
        }

        internal static uint ReadChar()
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
            //Diagnostics.OutputDebug("\nReadChar: 0x" + ((byte)readChar).ToString("X2") + " " + (readChar >= ' ' ? readChar : ' ') );
            return readChar;
        }

        internal static void WriteChar(char outChar)
        {
            if (IsValid())
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

        internal static void WriteString(HopperString str)
        {
            if (IsValid())
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
}
