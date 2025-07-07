### Windows COM ports under Ubuntu on WSL 2: Using USBIPD-WIN

## USB/IP with usbipd-win

This is the most reliable method for sharing COM ports between Windows and WSL2:

**1. Install usbipd-win on Windows:**
- Download from: https://github.com/dorssel/usbipd-win/releases
- Or use winget: `winget install usbipd`

**2. Install usbip tools in Ubuntu:**
```bash
sudo apt update
sudo apt install linux-tools-virtual hwdata
sudo update-alternatives --install /usr/local/bin/usbip usbip /usr/lib/linux-tools/*/usbip 20
```

**3. Share the USB serial device:**
In Windows PowerShell (as Administrator):
```powershell
# List available devices
usbipd wsl list

# Share the device (replace X-Y with your device's bus ID)
usbipd wsl attach --busid X-Y
```

**4. Access in Ubuntu:**
The device should appear as `/dev/ttyUSB0` or `/dev/ttyACM0`

**Verification:**
```bash
# List available serial devices
ls -la /dev/tty*

# Test with screen
sudo apt install screen
screen /dev/ttyUSB0 9600
```

This method provides reliable, low-latency serial communication that's essential for microcontroller development work.