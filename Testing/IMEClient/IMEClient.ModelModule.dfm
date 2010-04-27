object ModelModule: TModelModule
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Left = 270
  Top = 295
  Height = 105
  Width = 222
  object TCPClient: TIdTCPClient
    MaxLineAction = maException
    ReadTimeout = 0
    OnDisconnected = TCPClientDisconnected
    Host = '127.0.0.1'
    OnConnected = TCPClientConnected
    Port = 5555
    Left = 16
    Top = 8
  end
  object UnloadPluginTimer: TTimer
    Enabled = False
    Interval = 500
    OnTimer = UnloadPluginTimerTimer
    Left = 104
    Top = 8
  end
end
