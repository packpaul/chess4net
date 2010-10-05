object Skype: TSkype
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Left = 670
  Top = 380
  Height = 150
  Width = 215
  object FinishAttachmentTimer: TTimer
    Enabled = False
    OnTimer = FinishAttachmentTimerTimer
    Left = 48
    Top = 16
  end
end
