### Windows COM ports under Ubuntu on WSL 2: Using USBIPD-WIN

WSL 2's virtualized architecture means it doesn't have direct access to the host's serial ports. For USB-to-serial adapters, the recommended solution is to use the open-source project `usbipd-win`. This tool allows you to "pass through" a USB device from Windows to WSL 2.

Here's a step-by-step guide to get it working:

#### 1\. Install `usbipd-win` on Windows

First, you need to install the `usbipd-win` tool on your Windows machine.

  * Open **PowerShell as an administrator**.
  * Install `usbipd-win` using winget:
    ```powershell
    winget install --interactive --exact dorssel.usbipd-win
    ```

#### 2\. Install USB/IP Tools in Ubuntu

Next, you'll need to install the client-side tools in your Ubuntu distribution.

  * Open your Ubuntu terminal in WSL.
  * Update your package lists and install the necessary tools:
    ```bash
    sudo apt update
    sudo apt install linux-tools-virtual hwdata
    sudo update-alternatives --install /usr/local/bin/usbip usbip /usr/lib/linux-tools/*-generic/usbip 20
    ```

#### 3\. Share the USB-to-Serial Device from Windows

Now, you need to identify and share your USB-to-serial adapter from Windows.

  * In the **administrator PowerShell** window, list the USB devices connected to your machine:
    ```powershell
    usbipd wsl list
    ```
  * Find your USB-to-serial adapter in the list. Note its **BUSID**.
  * Bind the device to share it with WSL, replacing `<busid>` with the actual BUSID of your device:
    ```powershell
    usbipd bind --busid <busid>
    ```
    You might be prompted to approve the action.

#### 4\. Attach the Device in WSL

Finally, attach the shared device to your WSL 2 instance.

  * In the **administrator PowerShell** window, run the following command, again replacing `<busid>` with your device's BUSID:
    ```powershell
    usbipd attach --wsl --busid <busid>
    ```

Your USB-to-serial adapter should now be accessible within your Ubuntu environment. You can verify this by running `lsusb` in your Ubuntu terminal. The device will likely appear as `/dev/ttyUSB0` or a similar name, which you can then use with your applications.