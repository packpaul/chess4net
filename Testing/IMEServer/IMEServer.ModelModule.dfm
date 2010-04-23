object ModelModule: TModelModule
  OldCreateOrder = False
  OnDestroy = DataModuleDestroy
  Left = 488
  Top = 230
  Height = 129
  Width = 318
  object DelayedStepTimer: TTimer
    Enabled = False
    Interval = 200
    OnTimer = DelayedStepTimerTimer
    Left = 224
    Top = 8
  end
  object TCPServer: TIdTCPServer
    Active = True
    Bindings = <>
    CommandHandlers = <>
    DefaultPort = 5555
    Greeting.NumericCode = 0
    MaxConnectionReply.NumericCode = 0
    OnExecute = TCPServerExecute
    OnDisconnect = TCPServerDisconnect
    OnException = TCPServerException
    ReplyExceptionCode = 0
    ReplyTexts = <>
    ReplyUnknownCommand.NumericCode = 0
    ThreadMgr = IdThreadMgrDefault
    Left = 136
    Top = 8
  end
  object IdThreadMgrDefault: TIdThreadMgrDefault
    Left = 48
    Top = 8
  end
end
