object Skype: TSkype
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Left = 670
  Top = 380
  Height = 170
  Width = 200
  object FinishAttachmentTimer: TTimer
    Enabled = False
    OnTimer = FinishAttachmentTimerTimer
    Left = 72
    Top = 16
  end
  object PendingSkypeAPICommandsTimer: TTimer
    Enabled = False
    Interval = 1
    OnTimer = PendingSkypeAPICommandsTimerTimer
    Left = 72
    Top = 72
  end
end
