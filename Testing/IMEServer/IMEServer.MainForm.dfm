object MainForm: TMainForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'IMEServer'
  ClientHeight = 282
  ClientWidth = 288
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object PortLabel: TLabel
    Left = 8
    Top = 136
    Width = 56
    Height = 13
    Caption = 'Port#: 5555'
  end
  object Label2: TLabel
    Left = 8
    Top = 160
    Width = 30
    Height = 13
    Caption = 'Delay:'
  end
  object DelayLabel: TLabel
    Left = 40
    Top = 160
    Width = 15
    Height = 13
    Caption = '3.0'
  end
  object Label1: TLabel
    Left = 8
    Top = 216
    Width = 51
    Height = 13
    Caption = 'Server log:'
  end
  object DelayTrackBar: TTrackBar
    Left = 8
    Top = 176
    Width = 145
    Height = 33
    Max = 12
    TabOrder = 0
    OnChange = DelayTrackBarChange
  end
  object ClientsListBox: TListBox
    Left = 8
    Top = 8
    Width = 273
    Height = 121
    Columns = 1
    ItemHeight = 13
    TabOrder = 1
  end
  object LogMemo: TMemo
    Left = 8
    Top = 232
    Width = 273
    Height = 41
    ReadOnly = True
    TabOrder = 2
  end
  object DelayedStepTimer: TTimer
    Enabled = False
    Interval = 200
    OnTimer = DelayedStepTimerTimer
    Left = 160
    Top = 184
  end
  object TCPServer: TIdTCPServer
    Active = True
    Bindings = <>
    CommandHandlers = <>
    DefaultPort = 5555
    Greeting.NumericCode = 0
    MaxConnectionReply.NumericCode = 0
    OnConnect = TCPServerConnect
    OnExecute = TCPServerExecute
    OnDisconnect = TCPServerDisconnect
    OnException = TCPServerException
    ReplyExceptionCode = 0
    ReplyTexts = <>
    ReplyUnknownCommand.NumericCode = 0
    ThreadMgr = IdThreadMgrDefault
    Left = 192
    Top = 152
  end
  object IdThreadMgrDefault: TIdThreadMgrDefault
    Left = 160
    Top = 152
  end
end
