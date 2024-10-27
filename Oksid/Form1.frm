VERSION 5.00
Begin VB.Form mainForm 
   Caption         =   "OkSid Elektor manual tester..."
   ClientHeight    =   4335
   ClientLeft      =   60
   ClientTop       =   450
   ClientWidth     =   9375
   LinkTopic       =   "Form1"
   ScaleHeight     =   289
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   625
   StartUpPosition =   3  'Windows Default
   Begin VB.CheckBox chkSendWhenScroll 
      Caption         =   "Send also when scrolling the slider (if not timer)"
      Height          =   195
      Left            =   5160
      TabIndex        =   43
      Top             =   1320
      Width           =   3915
   End
   Begin VB.CommandButton cmdClean 
      Caption         =   "Clean debug"
      Height          =   315
      Left            =   7500
      TabIndex        =   42
      Top             =   3840
      Width           =   1575
   End
   Begin VB.Timer tmrSend 
      Enabled         =   0   'False
      Interval        =   50
      Left            =   8760
      Top             =   720
   End
   Begin VB.ListBox lstDebug 
      Height          =   1815
      Left            =   4680
      TabIndex        =   39
      Top             =   1920
      Width           =   4395
   End
   Begin VB.HScrollBar sldPage 
      Height          =   255
      Left            =   240
      Max             =   500
      TabIndex        =   38
      Top             =   3420
      Width           =   4275
   End
   Begin VB.CheckBox chkSend 
      Caption         =   "Send to OkSid with this method"
      Height          =   255
      Left            =   4740
      TabIndex        =   37
      Top             =   120
      Width           =   2475
   End
   Begin VB.OptionButton optChange 
      Caption         =   "Send only when Values change"
      Height          =   255
      Left            =   5160
      TabIndex        =   36
      Top             =   900
      Value           =   -1  'True
      Width           =   2535
   End
   Begin VB.OptionButton optTimer 
      Caption         =   "Send with a timer"
      Height          =   255
      Left            =   5160
      TabIndex        =   35
      Top             =   540
      Width           =   1815
   End
   Begin VB.VScrollBar sldCh 
      Height          =   2415
      Index           =   12
      Left            =   4200
      Max             =   0
      Min             =   255
      TabIndex        =   11
      Top             =   480
      Width           =   255
   End
   Begin VB.VScrollBar sldCh 
      Height          =   2415
      Index           =   11
      Left            =   3840
      Max             =   0
      Min             =   255
      TabIndex        =   10
      Top             =   480
      Width           =   255
   End
   Begin VB.VScrollBar sldCh 
      Height          =   2415
      Index           =   10
      Left            =   3480
      Max             =   0
      Min             =   255
      TabIndex        =   9
      Top             =   480
      Width           =   255
   End
   Begin VB.VScrollBar sldCh 
      Height          =   2415
      Index           =   9
      Left            =   3120
      Max             =   0
      Min             =   255
      TabIndex        =   8
      Top             =   480
      Width           =   255
   End
   Begin VB.VScrollBar sldCh 
      Height          =   2415
      Index           =   8
      Left            =   2760
      Max             =   0
      Min             =   255
      TabIndex        =   7
      Top             =   480
      Width           =   255
   End
   Begin VB.VScrollBar sldCh 
      Height          =   2415
      Index           =   7
      Left            =   2400
      Max             =   0
      Min             =   255
      TabIndex        =   6
      Top             =   480
      Width           =   255
   End
   Begin VB.VScrollBar sldCh 
      Height          =   2415
      Index           =   6
      Left            =   2040
      Max             =   0
      Min             =   255
      TabIndex        =   5
      Top             =   480
      Width           =   255
   End
   Begin VB.VScrollBar sldCh 
      Height          =   2415
      Index           =   5
      Left            =   1680
      Max             =   0
      Min             =   255
      TabIndex        =   4
      Top             =   480
      Width           =   255
   End
   Begin VB.VScrollBar sldCh 
      Height          =   2415
      Index           =   4
      Left            =   1320
      Max             =   0
      Min             =   255
      TabIndex        =   3
      Top             =   480
      Width           =   255
   End
   Begin VB.VScrollBar sldCh 
      Height          =   2415
      Index           =   3
      Left            =   960
      Max             =   0
      Min             =   255
      TabIndex        =   2
      Top             =   480
      Width           =   255
   End
   Begin VB.VScrollBar sldCh 
      Height          =   2415
      Index           =   2
      Left            =   600
      Max             =   0
      Min             =   255
      TabIndex        =   1
      Top             =   480
      Width           =   255
   End
   Begin VB.VScrollBar sldCh 
      Height          =   2415
      Index           =   1
      Left            =   240
      Max             =   0
      Min             =   255
      TabIndex        =   0
      Top             =   480
      Width           =   255
   End
   Begin VB.Shape shpLed 
      BackColor       =   &H00000000&
      BackStyle       =   1  'Opaque
      Height          =   255
      Left            =   9060
      Shape           =   3  'Circle
      Top             =   120
      Width           =   195
   End
   Begin VB.Label lblVal 
      Alignment       =   2  'Center
      BackColor       =   &H00FFFFFF&
      Caption         =   "0"
      Height          =   195
      Index           =   12
      Left            =   4140
      TabIndex        =   41
      Top             =   180
      Width           =   375
   End
   Begin VB.Label lblDebug 
      Caption         =   "Debug Info"
      Height          =   255
      Left            =   4680
      TabIndex        =   40
      Top             =   1620
      Width           =   4395
   End
   Begin VB.Label lblCh 
      Alignment       =   2  'Center
      BackColor       =   &H00FFFFFF&
      BackStyle       =   0  'Transparent
      Caption         =   "10"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   195
      Index           =   10
      Left            =   3420
      TabIndex        =   34
      Top             =   3060
      Width           =   375
   End
   Begin VB.Label lblCh 
      Alignment       =   2  'Center
      BackColor       =   &H00FFFFFF&
      BackStyle       =   0  'Transparent
      Caption         =   "9"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   195
      Index           =   9
      Left            =   3060
      TabIndex        =   33
      Top             =   3060
      Width           =   375
   End
   Begin VB.Label lblCh 
      Alignment       =   2  'Center
      BackColor       =   &H00FFFFFF&
      BackStyle       =   0  'Transparent
      Caption         =   "8"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   195
      Index           =   8
      Left            =   2700
      TabIndex        =   32
      Top             =   3060
      Width           =   375
   End
   Begin VB.Label lblCh 
      Alignment       =   2  'Center
      BackColor       =   &H00FFFFFF&
      BackStyle       =   0  'Transparent
      Caption         =   "7"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   195
      Index           =   7
      Left            =   2340
      TabIndex        =   31
      Top             =   3060
      Width           =   375
   End
   Begin VB.Label lblCh 
      Alignment       =   2  'Center
      BackColor       =   &H00FFFFFF&
      BackStyle       =   0  'Transparent
      Caption         =   "6"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   195
      Index           =   6
      Left            =   1980
      TabIndex        =   30
      Top             =   3060
      Width           =   375
   End
   Begin VB.Label lblCh 
      Alignment       =   2  'Center
      BackColor       =   &H00FFFFFF&
      BackStyle       =   0  'Transparent
      Caption         =   "5"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   195
      Index           =   5
      Left            =   1620
      TabIndex        =   29
      Top             =   3060
      Width           =   375
   End
   Begin VB.Label lblCh 
      Alignment       =   2  'Center
      BackColor       =   &H00FFFFFF&
      BackStyle       =   0  'Transparent
      Caption         =   "4"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   195
      Index           =   4
      Left            =   1260
      TabIndex        =   28
      Top             =   3060
      Width           =   375
   End
   Begin VB.Label lblCh 
      Alignment       =   2  'Center
      BackColor       =   &H00FFFFFF&
      BackStyle       =   0  'Transparent
      Caption         =   "3"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   195
      Index           =   3
      Left            =   900
      TabIndex        =   27
      Top             =   3060
      Width           =   375
   End
   Begin VB.Label lblCh 
      Alignment       =   2  'Center
      BackColor       =   &H00FFFFFF&
      BackStyle       =   0  'Transparent
      Caption         =   "2"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   195
      Index           =   2
      Left            =   540
      TabIndex        =   26
      Top             =   3060
      Width           =   375
   End
   Begin VB.Label lblCh 
      Alignment       =   2  'Center
      BackColor       =   &H00FFFFFF&
      BackStyle       =   0  'Transparent
      Caption         =   "1"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   195
      Index           =   1
      Left            =   180
      TabIndex        =   25
      Top             =   3060
      Width           =   375
   End
   Begin VB.Label lblCh 
      Alignment       =   2  'Center
      BackColor       =   &H00FFFFFF&
      BackStyle       =   0  'Transparent
      Caption         =   "11"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   195
      Index           =   11
      Left            =   3780
      TabIndex        =   24
      Top             =   3060
      Width           =   375
   End
   Begin VB.Label lblCh 
      Alignment       =   2  'Center
      BackColor       =   &H00FFFFFF&
      BackStyle       =   0  'Transparent
      Caption         =   "12"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   195
      Index           =   12
      Left            =   4140
      TabIndex        =   23
      Top             =   3060
      Width           =   375
   End
   Begin VB.Label lblVal 
      Alignment       =   2  'Center
      BackColor       =   &H00FFFFFF&
      Caption         =   "0"
      Height          =   195
      Index           =   11
      Left            =   3780
      TabIndex        =   22
      Top             =   180
      Width           =   375
   End
   Begin VB.Label lblVal 
      Alignment       =   2  'Center
      BackColor       =   &H00FFFFFF&
      Caption         =   "0"
      Height          =   195
      Index           =   10
      Left            =   3420
      TabIndex        =   21
      Top             =   180
      Width           =   375
   End
   Begin VB.Label lblVal 
      Alignment       =   2  'Center
      BackColor       =   &H00FFFFFF&
      Caption         =   "0"
      Height          =   195
      Index           =   9
      Left            =   3060
      TabIndex        =   20
      Top             =   180
      Width           =   375
   End
   Begin VB.Label lblVal 
      Alignment       =   2  'Center
      BackColor       =   &H00FFFFFF&
      Caption         =   "0"
      Height          =   195
      Index           =   8
      Left            =   2700
      TabIndex        =   19
      Top             =   180
      Width           =   375
   End
   Begin VB.Label lblVal 
      Alignment       =   2  'Center
      BackColor       =   &H00FFFFFF&
      Caption         =   "0"
      Height          =   195
      Index           =   7
      Left            =   2340
      TabIndex        =   18
      Top             =   180
      Width           =   375
   End
   Begin VB.Label lblVal 
      Alignment       =   2  'Center
      BackColor       =   &H00FFFFFF&
      Caption         =   "0"
      Height          =   195
      Index           =   6
      Left            =   1980
      TabIndex        =   17
      Top             =   180
      Width           =   375
   End
   Begin VB.Label lblVal 
      Alignment       =   2  'Center
      BackColor       =   &H00FFFFFF&
      Caption         =   "0"
      Height          =   195
      Index           =   5
      Left            =   1620
      TabIndex        =   16
      Top             =   180
      Width           =   375
   End
   Begin VB.Label lblVal 
      Alignment       =   2  'Center
      BackColor       =   &H00FFFFFF&
      Caption         =   "0"
      Height          =   195
      Index           =   4
      Left            =   1260
      TabIndex        =   15
      Top             =   180
      Width           =   375
   End
   Begin VB.Label lblVal 
      Alignment       =   2  'Center
      BackColor       =   &H00FFFFFF&
      Caption         =   "0"
      Height          =   195
      Index           =   3
      Left            =   900
      TabIndex        =   14
      Top             =   180
      Width           =   375
   End
   Begin VB.Label lblVal 
      Alignment       =   2  'Center
      BackColor       =   &H00FFFFFF&
      Caption         =   "0"
      Height          =   195
      Index           =   2
      Left            =   540
      TabIndex        =   13
      Top             =   180
      Width           =   375
   End
   Begin VB.Label lblVal 
      Alignment       =   2  'Center
      BackColor       =   &H00FFFFFF&
      Caption         =   "0"
      Height          =   195
      Index           =   1
      Left            =   180
      TabIndex        =   12
      Top             =   180
      Width           =   375
   End
End
Attribute VB_Name = "mainForm"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Dim DmxArray(1 To 512) As Byte
Dim Res As Long

Private Declare Sub InitCommonControls Lib "comctl32.dll" ()

Private Sub Form_Initialize()
    InitCommonControls
End Sub

Private Sub chkSend_Click()

    If chkSend.Value = 1 Then
        If optTimer.Value = True Then
            lstDebug.AddItem "Now timer is enabled to send."
            tmrSend.Enabled = True
        End If
        If optChange.Value = True Then
            lstDebug.AddItem "Now every change will send a dmx frame."
        End If
    Else
        If optTimer.Value = True Then
            lstDebug.AddItem "Now timer is disabled. We dont send."
            tmrSend.Enabled = False
        End If
        If optChange.Value = True Then lstDebug.AddItem "Now changes will not be sended."
        shpLed.BackColor = vbBlack
    End If

End Sub

Private Sub cmdClean_Click()
    lstDebug.Clear
End Sub

Private Sub optChange_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
    If chkSend.Value = 1 Then
        If optChange.Value = True Then
            lstDebug.AddItem "Now every change will send a dmx frame."
            tmrSend.Enabled = False
        End If
        If optChange.Value = False Then
            lstDebug.AddItem "Now timer is enabled to send."
            tmrSend.Enabled = True
        End If
    Else
         If optChange.Value = True Then
            lstDebug.AddItem "If you check to send, changes will send a dmx frame."
        End If
        If optChange.Value = False Then
            lstDebug.AddItem "If you check to send, timer will be enabled to send."
        End If
    End If
End Sub



Private Sub optTimer_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
    If chkSend.Value = 1 Then
        If optTimer.Value = True Then
            lstDebug.AddItem "Timer is enabled. We send dmx."
            tmrSend.Enabled = True
        End If
        If optTimer.Value = False Then
            lstDebug.AddItem "Now timer is disabled. We dont send."
            tmrSend.Enabled = False
        End If
    Else
        If optTimer.Value = True Then
            lstDebug.AddItem "If you check to send, timer will send dmx."
        End If
        If optTimer.Value = False Then
            lstDebug.AddItem "If you check to send, timer will not send dmx but changes will."
        End If
    End If

End Sub

Private Sub sldCh_Change(Index As Integer)
Static ActCh As Integer

    ActCh = Index + sldPage.Value
    DmxArray(ActCh) = sldCh(Index).Value
    lstDebug.AddItem "Channel " & ActCh & " is now " & DmxArray(ActCh)
    lblVal(Index).Caption = DmxArray(ActCh)
    
    If chkSend.Value = 1 Then
        If optChange.Value = True Then
            lstDebug.AddItem "...sended by change to the interface."
            Res = OksidCommand(OKSID_DMXOUT, 512, DmxArray(1))
            lstDebug.AddItem "...and the interface response is " & Res
            shpLed.BackColor = IIf(shpLed.BackColor = vbGreen, vbBlack, vbGreen)
        End If
    End If
End Sub

Private Sub sldCh_Scroll(Index As Integer)
    If chkSendWhenScroll.Value = 1 Then sldCh_Change (Index)
End Sub

Private Sub sldPage_Change()
Dim AuxIndex As Integer
'Actualize labels: values and ch numbers
For AuxIndex = 1 To 12
    lblVal(AuxIndex).Caption = DmxArray(AuxIndex + sldPage.Value)
    lblCh(AuxIndex).Caption = AuxIndex + sldPage.Value
    sldCh(AuxIndex).Value = DmxArray(AuxIndex + sldPage.Value)
Next AuxIndex

End Sub

Private Sub tmrSend_Timer()
    Res = OksidCommand(OKSID_DMXOUT, 512, DmxArray(1))
    lstDebug.AddItem "Sended one frame with timer, response " & Res
    shpLed.BackColor = IIf(shpLed.BackColor = vbGreen, vbBlack, vbGreen)
End Sub
