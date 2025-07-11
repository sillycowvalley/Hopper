using System.Device.Spi;
using System.Device.Gpio;

namespace HopperNET
{
    public static class SPI
    {
        private static Dictionary<byte, SpiDevice> devices = new Dictionary<byte, SpiDevice>();
        private static Dictionary<byte, SpiBusInfo> busInfo = new Dictionary<byte, SpiBusInfo>();
        private static GpioController gpioController;

        // Default configuration for SPI0
        public const byte DefaultI2CController0 = 0;
        public const byte DefaultCSPin0 = 8;     // CE0
        public const byte DefaultClkPin0 = 11;   // SCLK
        public const byte DefaultTxPin0 = 10;    // MOSI
        public const byte DefaultRxPin0 = 9;     // MISO

        // Default configuration for SPI1 (if available)
        public const byte DefaultCSPin1 = 7;     // CE1
        public const byte DefaultClkPin1 = 21;   // SCLK1
        public const byte DefaultTxPin1 = 20;    // MOSI1
        public const byte DefaultRxPin1 = 19;    // MISO1

        public enum DataOrder
        {
            LSBFirst = 0,
            MSBFirst = 1,
        }

        public enum DataMode
        {
            Mode0 = 0,
            Mode1 = 1,
            Mode2 = 2,
            Mode3 = 3,
        }

        private class SpiBusInfo
        {
            public byte SpiController { get; set; }
            public byte CSPin { get; set; }
            public byte ClkPin { get; set; }
            public byte TxPin { get; set; }
            public byte RxPin { get; set; }
            public int SpeedMaximum { get; set; }
            public DataOrder DataOrder { get; set; }
            public DataMode DataMode { get; set; }
            public bool SettingsConfigured { get; set; }
            public bool InTransaction { get; set; }
        }

        static SPI()
        {
            gpioController = new GpioController();
        }

        // SPI0 Default API
        public static void SetCSPin(byte csPin)
        {
            SetCSPin(0, csPin);
        }

        public static void SetClkPin(byte clkPin)
        {
            SetClkPin(0, clkPin);
        }

        public static void SetTxPin(byte txPin)
        {
            SetTxPin(0, txPin);
        }

        public static void SetRxPin(byte rxPin)
        {
            SetRxPin(0, rxPin);
        }

        public static void Settings(int speedMaximum, DataOrder dataOrder, DataMode dataMode)
        {
            Settings(0, speedMaximum, dataOrder, dataMode);
        }

        public static bool Begin()
        {
            return Begin(0);
        }

        public static void BeginTransaction()
        {
            BeginTransaction(0);
        }

        public static void EndTransaction()
        {
            EndTransaction(0);
        }

        public static byte ReadByte()
        {
            return ReadByte(0);
        }

        public static ushort ReadWord()
        {
            return ReadWord(0);
        }

        public static void ReadBuffer(byte[] data, uint startIndex, uint length)
        {
            ReadBuffer(0, data, startIndex, length);
        }

        public static void WriteByte(byte data)
        {
            WriteByte(0, data);
        }

        public static void WriteWord(ushort data)
        {
            WriteWord(0, data);
        }

        public static void WriteBytes(byte data, uint count)
        {
            WriteBytes(0, data, count);
        }

        public static void WriteWords(ushort data, uint count)
        {
            WriteWords(0, data, count);
        }

        public static void WriteBuffer(byte[] data, uint startIndex, uint length)
        {
            WriteBuffer(0, data, startIndex, length);
        }

        public static void WriteBuffer(ushort[] data, uint startIndex, uint length)
        {
            WriteBuffer(0, data, startIndex, length);
        }

        // Multi-controller API
        public static void SetCSPin(byte spiController, byte csPin)
        {
            EnsureBusInfo(spiController);
            busInfo[spiController].CSPin = csPin;
        }

        public static void SetClkPin(byte spiController, byte clkPin)
        {
            EnsureBusInfo(spiController);
            busInfo[spiController].ClkPin = clkPin;
        }

        public static void SetTxPin(byte spiController, byte txPin)
        {
            EnsureBusInfo(spiController);
            busInfo[spiController].TxPin = txPin;
        }

        public static void SetRxPin(byte spiController, byte rxPin)
        {
            EnsureBusInfo(spiController);
            busInfo[spiController].RxPin = rxPin;
        }

        public static void Settings(byte spiController, int speedMaximum, DataOrder dataOrder, DataMode dataMode)
        {
            EnsureBusInfo(spiController);
            var info = busInfo[spiController];
            info.SpeedMaximum = speedMaximum;
            info.DataOrder = dataOrder;
            info.DataMode = dataMode;
            info.SettingsConfigured = true;
        }

        public static bool Begin(byte spiController)
        {
#if !NOEXCEPTIONS
            try
            {
#endif
                if (!busInfo.ContainsKey(spiController))
                {
                    return false;
                }

                var info = busInfo[spiController];

                if (!info.SettingsConfigured)
                {
                    return false;
                }

                // Set up CS pin as output
                if (gpioController != null && !gpioController.IsPinOpen(info.CSPin))
                {
                    gpioController.OpenPin(info.CSPin, PinMode.Output);
                    gpioController.Write(info.CSPin, PinValue.High); // CS starts high (inactive)
                }

                // Create SPI device if not already created
                if (!devices.ContainsKey(spiController))
                {
                    var connectionSettings = new SpiConnectionSettings(spiController, info.CSPin)
                    {
                        ClockFrequency = info.SpeedMaximum,
                        Mode = (SpiMode)info.DataMode,
                        DataBitLength = 8,
                        ChipSelectLineActiveState = PinValue.Low
                    };

                    // Handle bit order - .NET SPI assumes MSB first
                    if (info.DataOrder == DataOrder.LSBFirst)
                    {
                        // Note: LSB first may need special handling depending on device
                        // Some devices require software bit reversal
                    }

                    devices[spiController] = SpiDevice.Create(connectionSettings);
                }

                return true;
#if !NOEXCEPTIONS
            }
            catch
            {
                return false;
            }
#endif
        }

        public static void BeginTransaction(byte spiController)
        {
            if (!busInfo.ContainsKey(spiController))
                return;

            var info = busInfo[spiController];
            if (!info.InTransaction && gpioController != null)
            {
                gpioController.Write(info.CSPin, PinValue.Low); // Assert CS
                info.InTransaction = true;
            }
        }

        public static void EndTransaction(byte spiController)
        {
            if (!busInfo.ContainsKey(spiController))
                return;

            var info = busInfo[spiController];
            if (info.InTransaction && gpioController != null)
            {
                gpioController.Write(info.CSPin, PinValue.High); // Deassert CS
                info.InTransaction = false;
            }
        }

        public static byte ReadByte(byte spiController)
        {
            if (!devices.ContainsKey(spiController))
                return 0;

            var buffer = new byte[1];
            devices[spiController].Read(buffer);
            return ReverseBitsIfNeeded(spiController, buffer[0]);
        }

        public static ushort ReadWord(byte spiController)
        {
            if (!devices.ContainsKey(spiController))
                return 0;

            var buffer = new byte[2];
            devices[spiController].Read(buffer);

            if (busInfo[spiController].DataOrder == DataOrder.LSBFirst)
            {
                return (ushort)(ReverseBitsIfNeeded(spiController, buffer[1]) << 8 |
                               ReverseBitsIfNeeded(spiController, buffer[0]));
            }
            else
            {
                return (ushort)(buffer[0] << 8 | buffer[1]);
            }
        }

        public static void ReadBuffer(byte spiController, byte[] data, uint startIndex, uint length)
        {
            if (!devices.ContainsKey(spiController) || data == null)
                return;

            var buffer = new byte[length];
            devices[spiController].Read(buffer);

            for (uint i = 0; i < length && (startIndex + i) < data.Length; i++)
            {
                data[startIndex + i] = ReverseBitsIfNeeded(spiController, buffer[i]);
            }
        }

        public static void WriteByte(byte spiController, byte data)
        {
            if (!devices.ContainsKey(spiController))
                return;

            var buffer = new byte[] { ReverseBitsIfNeeded(spiController, data) };
            devices[spiController].Write(buffer);
        }

        public static void WriteWord(byte spiController, ushort data)
        {
            if (!devices.ContainsKey(spiController))
                return;

            byte[] buffer;
            if (busInfo[spiController].DataOrder == DataOrder.LSBFirst)
            {
                buffer = new byte[]
                {
                    ReverseBitsIfNeeded(spiController, (byte)(data & 0xFF)),
                    ReverseBitsIfNeeded(spiController, (byte)((data >> 8) & 0xFF))
                };
            }
            else
            {
                buffer = new byte[]
                {
                    (byte)((data >> 8) & 0xFF),
                    (byte)(data & 0xFF)
                };
            }

            devices[spiController].Write(buffer);
        }

        public static void WriteBytes(byte spiController, byte data, uint count)
        {
            if (!devices.ContainsKey(spiController))
                return;

            var buffer = new byte[count];
            var processedData = ReverseBitsIfNeeded(spiController, data);

            for (uint i = 0; i < count; i++)
            {
                buffer[i] = processedData;
            }

            devices[spiController].Write(buffer);
        }

        public static void WriteWords(byte spiController, ushort data, uint count)
        {
            if (!devices.ContainsKey(spiController))
                return;

            for (uint i = 0; i < count; i++)
            {
                WriteWord(spiController, data);
            }
        }

        public static void WriteBuffer(byte spiController, byte[] data, uint startIndex, uint length)
        {
            if (!devices.ContainsKey(spiController) || data == null)
                return;

            var buffer = new byte[length];
            for (uint i = 0; i < length && (startIndex + i) < data.Length; i++)
            {
                buffer[i] = ReverseBitsIfNeeded(spiController, data[startIndex + i]);
            }

            devices[spiController].Write(buffer);
        }

        public static void WriteBuffer(byte spiController, ushort[] data, uint startIndex, uint length)
        {
            if (!devices.ContainsKey(spiController) || data == null)
                return;

            // Convert ushort array to byte array
            var buffer = new byte[length * 2];
            for (uint i = 0; i < length && (startIndex + i) < data.Length; i++)
            {
                var word = data[startIndex + i];
                if (busInfo[spiController].DataOrder == DataOrder.LSBFirst)
                {
                    buffer[i * 2] = ReverseBitsIfNeeded(spiController, (byte)(word & 0xFF));
                    buffer[i * 2 + 1] = ReverseBitsIfNeeded(spiController, (byte)((word >> 8) & 0xFF));
                }
                else
                {
                    buffer[i * 2] = (byte)((word >> 8) & 0xFF);
                    buffer[i * 2 + 1] = (byte)(word & 0xFF);
                }
            }

            devices[spiController].Write(buffer);
        }

        public static byte GetCSPin(byte spiController)
        {
            if (!busInfo.ContainsKey(spiController))
                return 0;

            return busInfo[spiController].CSPin;
        }

        private static void EnsureBusInfo(byte spiController)
        {
            if (!busInfo.ContainsKey(spiController))
            {
                busInfo[spiController] = new SpiBusInfo
                {
                    SpiController = spiController,
                    CSPin = (byte)(spiController == 0 ? DefaultCSPin0 : DefaultCSPin1),
                    ClkPin = (byte)(spiController == 0 ? DefaultClkPin0 : DefaultClkPin1),
                    TxPin = (byte)(spiController == 0 ? DefaultTxPin0 : DefaultTxPin1),
                    RxPin = (byte)(spiController == 0 ? DefaultRxPin0 : DefaultRxPin1),
                    SpeedMaximum = 1000000, // Default 1MHz
                    DataOrder = DataOrder.MSBFirst,
                    DataMode = DataMode.Mode0,
                    SettingsConfigured = false,
                    InTransaction = false
                };
            }
        }

        private static byte ReverseBitsIfNeeded(byte spiController, byte data)
        {
            if (!busInfo.ContainsKey(spiController) ||
                busInfo[spiController].DataOrder == DataOrder.MSBFirst)
            {
                return data;
            }

            // Reverse bits for LSB first
            byte reversed = 0;
            for (int i = 0; i < 8; i++)
            {
                reversed = (byte)((reversed << 1) | (data & 1));
                data >>= 1;
            }
            return reversed;
        }

        public static void Dispose()
        {
            foreach (var device in devices.Values)
            {
                device?.Dispose();
            }
            devices.Clear();
            busInfo.Clear();
            gpioController?.Dispose();
            gpioController = null;
        }
    }
}