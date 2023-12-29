import machine
import utime
import sys

ledPin = machine.Pin(25, machine.Pin.OUT)

sys.stdout.write('!')

while True:
    ledPin.value(True)
    sys.stdout.write('+')
    utime.sleep_ms(500)
    ledPin.value(False)
    sys.stdout.write('-')
    utime.sleep_ms(500)