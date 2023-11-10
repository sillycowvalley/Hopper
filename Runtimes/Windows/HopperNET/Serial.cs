using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.IO.Ports;

namespace HopperNET
{
    class Serial
    {
        static bool continueSerial;
        static SerialPort serialPort;
        static string lastPort;

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
            String[] names = SerialPort.GetPortNames();
            foreach (string name in names)
            {
                list.Add(name);
            }
            return list;
        }
        static void  connect(string portName)
        {
            if ((null != serialPort) && serialPort.IsOpen)
            {
                serialPort.Close(); // if it is already connected, close it first
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
