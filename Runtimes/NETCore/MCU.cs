﻿using System;
using System.Collections.Generic;
using System.Device.Gpio;
using System.Device.Pwm;
using System.Linq;
using System.Net.NetworkInformation;
using System.Text;
using System.Threading.Tasks;

namespace HopperNET
{
    public enum PinModeOption
    {
        Input = 0x00,
        Output = 0x01,
        InputPullup = 0x02,
        InputPulldown = 0x03,
    }

    public enum PinStatus
    {
        Low = 0,
        High = 1,
        Change = 2,
        Falling = 3,
        Rising = 4,
    }


    public class MCU
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
        public void PinMode(ushort pin, PinModeOption pinMode)
        {
            if (pinOk(pin))
            {
                switch (pinMode)
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

        private List<ISRStruct> pinCallbacks = new List<ISRStruct>();

        private void OnPinValueChanged(object sender, PinValueChangedEventArgs e)
        {
            byte pin = (byte)e.PinNumber;
            lock (pinCallbacks)
            {
                foreach (ISRStruct isrStruct in pinCallbacks)
                {
                    if (isrStruct.pin == pin)
                    {
                        byte status = 0;
                        switch (e.ChangeType)
                        {
                            case PinEventTypes.Rising:
                                status = (byte)PinStatus.Rising;
                                break;
                            case PinEventTypes.Falling:
                                status = (byte)PinStatus.Falling;
                                break;
                            default:
                                status = (byte)PinStatus.Change;
                                break;
                        }
                        if (0 != (isrStruct.status & status))
                        {
                            lock (isrQueue)
                            {
                                isrQueue.Enqueue(isrStruct);
                            }
                        }
                    }
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

                lock (pinCallbacks)
                {
                    pinCallbacks.Add(isrStruct);
                }

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
                SPI.Dispose();
                foreach (var pwm in pwmChannels.Values)
                {
                    pwm.Stop();
                    pwm.Dispose();
                }
                pwmChannels.Clear();

                gpio.Dispose(); // Automatically closes all pins

                lock (pinCallbacks)
                {
                    pinCallbacks.Clear();
                }

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
