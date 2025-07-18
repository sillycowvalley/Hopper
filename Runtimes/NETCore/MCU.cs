using System;
using System.Collections.Generic;
using System.Device.Gpio;
using System.Device.Pwm;
using System.Linq;
using System.Net.NetworkInformation;
using System.Text;
using System.Threading.Tasks;

namespace HopperNET
{
    internal enum PinModeOption
    {
        Input = 0x00,
        Output = 0x01,
        InputPullup = 0x02,
        InputPulldown = 0x03,
    }

    internal enum PinStatus
    {
        Low = 0,
        High = 1,
        Change = 2,
        Falling = 3,
        Rising = 4,
    }


    internal class MCU
    {
        private GpioController gpio = null;
        private static byte analogWriteResolution = 8;
        private static Dictionary<byte, PwmChannel> pwmChannels = new Dictionary<byte, PwmChannel>();

        Queue<ISRStruct> isrQueue = null;
        public MCU(Queue<ISRStruct> isrQueue)
        {
            this.isrQueue = isrQueue;
        }
        bool pinOk(ushort pin)
        {
            for (; ; )
            {
                if (gpio == null)
                {
#if !NOEXCEPTIONS
                    try
                    {
#endif
                        gpio = new GpioController();
#if !NOEXCEPTIONS
                    }
                    catch (PlatformNotSupportedException)
                    {
                        break;
                    }
#endif
                }
                if (!gpio.IsPinOpen(pin))
                {
                    gpio.OpenPin(pin);
                }
                if (!gpio.IsPinOpen(pin))
                {
                    break;
                }
                return true;
            }
            return false;
        }
        public void PinMode(ushort pin, ushort pinMode)
        {
            if (pinOk(pin))
            {
                switch ((PinModeOption)pinMode)
                {
                    case PinModeOption.Input:
                        gpio.SetPinMode(pin, System.Device.Gpio.PinMode.Input);
                        break;
                    case PinModeOption.Output:
                        gpio.SetPinMode(pin, System.Device.Gpio.PinMode.Output);
                        break;
                    case PinModeOption.InputPullup:
                        gpio.SetPinMode(pin, System.Device.Gpio.PinMode.InputPullUp);
                        break;
                    case PinModeOption.InputPulldown:
                        gpio.SetPinMode(pin, System.Device.Gpio.PinMode.InputPullDown);
                        break;
                }
            }
        }

        internal bool DigitalRead(ushort pin)
        {
            bool result = false;
            if (pinOk(pin))
            {
                return gpio.Read(pin) == PinValue.High;
            }
            return result;
        }

        internal void DigitWrite(ushort pin, bool value)
        {
            if (pinOk(pin))
            {
                gpio.Write(pin, value ? PinValue.High : PinValue.Low);
            }
        }

        private Dictionary<byte, ISRStruct> pinCallbacks = new Dictionary<byte, ISRStruct>();

        private void OnPinValueChanged(object sender, PinValueChangedEventArgs e)
        {
            byte pin = (byte)e.PinNumber;
            if (pinCallbacks.TryGetValue(pin, out ISRStruct isrStruct))
            {
                PinStatus status;
                switch (e.ChangeType)
                {
                    case PinEventTypes.Rising:
                        status = PinStatus.Rising;
                        break;
                    case PinEventTypes.Falling:
                        status = PinStatus.Falling;
                        break;
                    default:
                        status = PinStatus.Change;
                        break;
                }
                isrStruct.status = (byte)status;
                lock (isrQueue)
                {
                    isrQueue.Enqueue(isrStruct);
                }
            }
        }

        internal bool AttachToPin(ushort pin, ushort isrDelegate, ushort pinStatus)
        {
            bool result = false;
            if (pinOk(pin))
            {
                ISRStruct isrStruct = new ISRStruct();
                isrStruct.pin = (byte)pin;
                isrStruct.isrDelegate = isrDelegate;
                isrStruct.interruptType = InterruptType.ePin;
                isrStruct.status = (byte)pinStatus;

                pinCallbacks[(byte)pin] = isrStruct;

                PinEventTypes eventTypes = PinEventTypes.None;
                switch ((PinStatus)pinStatus)
                {
                    case PinStatus.Rising:
                        eventTypes = PinEventTypes.Rising;
                        result = true;
                        break;
                    case PinStatus.Falling:
                        eventTypes = PinEventTypes.Falling;
                        result = true;
                        break;
                    case PinStatus.Change:
                        eventTypes = PinEventTypes.Rising | PinEventTypes.Falling;
                        result = true;
                        break;
                    default:
                        return false; // Low/High states don't make sense for interrupts
                }
                gpio.RegisterCallbackForPinValueChangedEvent(pin, eventTypes, OnPinValueChanged);
            }
            return result;
        }

        public void Release()
        {
            if (gpio != null)
            {
                foreach (var pwm in pwmChannels.Values)
                {
                    pwm.Stop();
                    pwm.Dispose();
                }
                pwmChannels.Clear();

                gpio.Dispose(); // Automatically closes all pins
                gpio = null;
            }
        }

        internal void AnalogWrite(byte pin, ushort value)
        {
            for (; ; )
            {
                if (gpio == null)
                {
#if !NOEXCEPTIONS
                    try
                    {
#endif
                        gpio = new GpioController();
#if !NOEXCEPTIONS
                    }
                    catch (PlatformNotSupportedException)
                    {
                        break;
                    }
#endif
                }
                if ((value == 0) && pwmChannels.ContainsKey(pin))
                {
                    // Stop and dispose PWM when value is 0
                    pwmChannels[pin].Stop();
                    pwmChannels[pin].Dispose();
                    pwmChannels.Remove(pin);
                    return;
                }
                if (!pwmChannels.ContainsKey(pin))
                {
                    // Create PWM channel for this pin
                    // Note: chip and channel numbers are platform-specific
                    int chip = 0;
                    int channel = pin;
                    pwmChannels[pin] = PwmChannel.Create(chip, channel, 1000); // 1kHz frequency
                    pwmChannels[pin].Start();
                }

                // Convert value to duty cycle based on resolution
                double maxValue = Math.Pow(2, analogWriteResolution) - 1;
                double dutyCycle = Math.Min(value / maxValue, 1.0);
                pwmChannels[pin].DutyCycle = dutyCycle;
            }
        }

        internal void AnalogWriteResolution(ushort bits)
        {
            analogWriteResolution = (byte)bits;
        }
    }
}
