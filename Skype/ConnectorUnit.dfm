object Connector: TConnector
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Left = 43
  Top = 227
  Height = 120
  Width = 288
  object sendTimer: TTimer
    Enabled = False
    Interval = 100
    OnTimer = sendTimerTimer
    Left = 16
    Top = 8
  end
  object showContactsTimer: TTimer
    Enabled = False
    Interval = 500
    OnTimer = showContactsTimerTimer
    Left = 96
    Top = 8
  end
  object userConnectingTimer: TTimer
    Enabled = False
    Interval = 500
    OnTimer = userConnectingTimerTimer
    Left = 200
    Top = 8
  end
end
