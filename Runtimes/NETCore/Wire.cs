using System.Device.I2c;


namespace HopperNET
{
    public static class Wire
    {
        private static Dictionary<ushort, I2cDevice> devices = new Dictionary<ushort, I2cDevice>();
        private static Dictionary<byte, I2cBusInfo> busInfo = new Dictionary<byte, I2cBusInfo>();

        // Default configuration - adjust for your board
        public const byte DefaultI2CController = 0;
        public const byte DefaultI2CSDAPin = 2;
        public const byte DefaultI2CSCLPin = 3;

        private class I2cBusInfo
        {
            public int BusId { get; set; }
            public byte SDAPin { get; set; }
            public byte SCLPin { get; set; }
            public uint FreqkHz { get; set; }
            public byte CurrentAddress { get; set; }
            public List<byte> TxBuffer { get; set; } = new List<byte>();
            public List<byte> RxBuffer { get; set; } = new List<byte>();
            public int RxIndex { get; set; } = 0;
        }

        

        public static bool Initialize()
        {
            return Initialize(DefaultI2CController, DefaultI2CSDAPin, DefaultI2CSCLPin);
        }

        public static bool Initialize(byte i2cController, byte sdaPin, byte sclPin)
        {
            return Initialize(i2cController, sdaPin, sclPin, 400);
        }

        public static bool Initialize(byte i2cController, byte sdaPin, byte sclPin, uint freqkHz)
        {
#if !NOEXCEPTIONS
            try
            {
#endif
                Configure(i2cController, sdaPin, sclPin, freqkHz);
                return Begin(i2cController);
#if !NOEXCEPTIONS
            }
            catch
            {
                return false;
            }
#endif
        }

        public static void Configure(byte i2cController, byte sdaPin, byte sclPin)
        {
            Configure(i2cController, sdaPin, sclPin, 400);
        }

        public static void Configure(byte i2cController, byte sdaPin, byte sclPin, uint freqkHz)
        {
            if (busInfo.ContainsKey(i2cController))
            {
                busInfo.Remove(i2cController);
            }

            busInfo[i2cController] = new I2cBusInfo
            {
                BusId = i2cController,
                SDAPin = sdaPin,
                SCLPin = sclPin,
                FreqkHz = freqkHz
            };
        }

        public static bool Begin()
        {
            return Begin(DefaultI2CController);
        }

        public static bool Begin(byte i2cController)
        {
#if !NOEXCEPTIONS
            try
            {
#endif
                if (!Wire.busInfo.ContainsKey(i2cController))
                {
                    return false;
                }

                var busInfo = Wire.busInfo[i2cController];

                // I2cConnectionSettings only needs bus ID and device address
                // The actual I2C bus configuration is handled at the hardware level

                return true;
#if !NOEXCEPTIONS
            }
            catch
            {
                return false;
            }
#endif
        }

        public static void BeginTx(byte address)
        {
            BeginTx(DefaultI2CController, address);
        }

        public static void BeginTx(byte i2cController, byte address)
        {
            if (!Wire.busInfo.ContainsKey(i2cController))
            {
                return;
            }

            var busInfo = Wire.busInfo[i2cController];
            busInfo.CurrentAddress = address;
            busInfo.TxBuffer.Clear();
        }

        public static byte EndTx()
        {
            return EndTx(DefaultI2CController);
        }

        public static byte EndTx(byte i2cController)
        {
            if (!Wire.busInfo.ContainsKey(i2cController))
            {
                return 1; // Busy timeout
            }

            var busInfo = Wire.busInfo[i2cController];
#if !NOEXCEPTIONS
            try
            {
#endif
                // Get or create I2C device for this address
                var deviceKey = (ushort)((i2cController << 8) | busInfo.CurrentAddress);

                if (!devices.ContainsKey(deviceKey))
                {
                    var settings = new I2cConnectionSettings(busInfo.BusId, busInfo.CurrentAddress);
                    devices[deviceKey] = I2cDevice.Create(settings);
                }

                var device = devices[deviceKey];

                if (busInfo.TxBuffer.Count > 0)
                {
                    device.Write(busInfo.TxBuffer.ToArray());
                }

                return 0; // Success
#if !NOEXCEPTIONS
            }
            catch (System.IO.IOException)
            {
                return 3; // Address timeout (NACK)
            }
            catch (TimeoutException)
            {
                return 4; // Data timeout
            }
            catch
            {
                return 2; // START bit timeout or other error
            }
#endif
        }

        public static void Write(byte data)
        {
            Write(DefaultI2CController, data);
        }

        public static void Write(byte i2cController, byte data)
        {
            if (!busInfo.ContainsKey(i2cController))
            {
                return;
            }

            busInfo[i2cController].TxBuffer.Add(data);
        }

        public static void Write(byte i2cController, byte[] data, uint startIndex, uint length)
        {
            if (!Wire.busInfo.ContainsKey(i2cController))
            {
                return;
            }

            var busInfo = Wire.busInfo[i2cController];
            for (uint i = 0; i < length; i++)
            {
                if (startIndex + i < data.Length)
                {
                    busInfo.TxBuffer.Add(data[startIndex + i]);
                }
            }
        }

        public static void Write(byte i2cController, bool[] data, uint startIndex, uint length)
        {
            if (!Wire.busInfo.ContainsKey(i2cController))
            {
                return;
            }

            var busInfo = Wire.busInfo[i2cController];
            for (uint i = 0; i < length; i++)
            {
                if (startIndex + i < data.Length)
                {
                    busInfo.TxBuffer.Add((byte)(data[startIndex + i] ? 1 : 0));
                }
            }
        }

        public static byte RequestFrom(byte address, byte bytes)
        {
            return RequestFrom(DefaultI2CController, address, bytes);
        }

        public static byte RequestFrom(byte i2cController, byte address, byte bytes)
        {
            if (!Wire.busInfo.ContainsKey(i2cController))
            {
                return 0;
            }

            var busInfo = Wire.busInfo[i2cController];
            busInfo.RxBuffer.Clear();
            busInfo.RxIndex = 0;
#if !NOEXCEPTIONS
            try
            {
#endif
                // Get or create I2C device for this address
                var deviceKey = (ushort)((i2cController << 8) | address);

                if (!devices.ContainsKey(deviceKey))
                {
                    var settings = new I2cConnectionSettings(busInfo.BusId, address);

                    devices[deviceKey] = I2cDevice.Create(settings);
                }

                var device = devices[deviceKey];

                // Read the requested number of bytes
                byte[] buffer = new byte[bytes];
                device.Read(buffer);

                // Store in our buffer
                busInfo.RxBuffer.AddRange(buffer);

                return (byte)busInfo.RxBuffer.Count;
#if !NOEXCEPTIONS
            }
            catch
            {
                return 0; // Read failed
            }
#endif
        }

        public static byte Read()
        {
            return Read(DefaultI2CController);
        }

        public static byte Read(byte i2cController)
        {
            if (!Wire.busInfo.ContainsKey(i2cController))
                return 0;

            var busInfo = Wire.busInfo[i2cController];

            if (busInfo.RxIndex < busInfo.RxBuffer.Count)
            {
                return busInfo.RxBuffer[busInfo.RxIndex++];
            }

            return 0;
        }

        public static void Dispose()
        {
            foreach (var device in devices.Values)
            {
                device?.Dispose();
            }
            devices.Clear();
            busInfo.Clear();
        }
    }

}
