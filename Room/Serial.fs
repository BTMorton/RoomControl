module Serial

open System.IO;
open System.IO.Ports;

let create baud port = new SerialPort(port, baud, Parity.None, 8, StopBits.One)

let openSerial (serial: SerialPort) = serial.Open()

let closeSerial (serial: SerialPort) = serial.Close()

let create96 = create 9600

let create192 = create 19200

let readLine (serial: SerialPort) = serial.ReadLine()