object Connector: TConnector
  OldCreateOrder = False
  Left = 325
  Top = 207
  Height = 105
  Width = 112
  object sendTimer: TTimer
    Enabled = False
    Interval = 100
    OnTimer = sendTimerTimer
    Left = 16
    Top = 8
  end
end
