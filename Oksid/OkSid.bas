Attribute VB_Name = "OkSid"
Option Explicit

Public Const OKSID_OPEN = 1
Public Const OKSID_CLOSE = 2
Public Const OKSID_DMXOUTOFF = 3
Public Const OKSID_DMXOUT = 4
Public Const OKSID_DMXIN = 8
Public Const OKSID_OK = 1
Public Const OKSID_ERROR_COMMAND = -1
Public Const OKSID_NOTHING = 2
Public Const OKSID_CONTROLLER1 = 0
Public Const OKSID_CONTROLLER2 = 100
Public Const OKSID_CONTROLLER3 = 200
Public Const OKSID_CONTROLLER4 = 300

Public Declare Function OksidCommand Lib "usb2dmx.dll" _
        (ByVal Command As Long, ByVal Parameter As Long, _
            ByRef Buffer As Byte) As Long

