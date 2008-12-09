object Connector: TConnector
  OldCreateOrder = False
  Left = 711
  Top = 347
  Height = 150
  Width = 215
  object Client: TClientSocket
    Active = False
    ClientType = ctNonBlocking
    Port = 0
    OnConnect = ClientConnect
    OnDisconnect = ClientDisconnect
    OnRead = ClientRead
    OnError = ClientError
    Left = 24
    Top = 8
  end
  object Server: TServerSocket
    Active = False
    Port = 0
    ServerType = stNonBlocking
    OnClientConnect = ServerClientConnect
    OnClientDisconnect = ServerClientDisconnect
    OnClientRead = ServerClientRead
    OnClientError = ServerClientError
    Left = 24
    Top = 64
  end
  object sendTimer: TTimer
    Interval = 50
    OnTimer = sendTimerTimer
    Left = 72
    Top = 8
  end
end
