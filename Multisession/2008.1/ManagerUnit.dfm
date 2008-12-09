object Manager: TManager
  Left = 448
  Top = 100
  BorderIcons = []
  BorderStyle = bsToolWindow
  Caption = 'Manager'
  ClientHeight = 72
  ClientWidth = 204
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefault
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object ActionList: TActionList
    Left = 176
    Top = 8
    object OptionsAction: TAction
      Caption = 'Options...'
      OnExecute = OptionsActionExecute
    end
    object AboutAction: TAction
      Caption = 'About...'
      OnExecute = AboutActionExecute
    end
  end
  object ConnectedPopupMenu: TPopupMenu
    AutoPopup = False
    Left = 8
    Top = 8
    object StartAdjournedGameConnected: TMenuItem
      Caption = 'Start Adjourned Game'
      Visible = False
      OnClick = StartAdjournedGameConnectedClick
    end
    object StartStandartGameConnected: TMenuItem
      Caption = 'Start Standart Game'
      OnClick = StartStandartGameConnectedClick
    end
    object StartPPRandomGameConnected: TMenuItem
      Caption = 'Start PP Random Game'
      OnClick = StartPPRandomGameConnectedClick
    end
    object N5: TMenuItem
      Caption = '-'
    end
    object ChangeColorConnected: TMenuItem
      Caption = 'Change Color'
      OnClick = ChangeColorConnectedClick
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object GameOptionsConnected: TMenuItem
      Caption = 'Game Options...'
      OnClick = GameOptionsConnectedClick
    end
    object LookFeelOptionsConnected: TMenuItem
      Action = OptionsAction
      Caption = 'Look && Feel Options...'
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object AboutConnected: TMenuItem
      Action = AboutAction
    end
  end
  object GamePopupMenu: TPopupMenu
    AutoPopup = False
    OnPopup = GamePopupMenuPopup
    Left = 40
    Top = 8
    object AbortGame: TMenuItem
      Caption = 'Abort'
      OnClick = AbortGameClick
    end
    object DrawGame: TMenuItem
      Caption = 'Draw'
      OnClick = DrawGameClick
    end
    object ResignGame: TMenuItem
      Caption = 'Resign'
      OnClick = ResignGameClick
    end
    object N6: TMenuItem
      Caption = '-'
    end
    object AdjournGame: TMenuItem
      Caption = 'Adjourn'
      Visible = False
      OnClick = AdjournGameClick
    end
    object GamePause: TMenuItem
      Caption = 'Pause'
      Visible = False
      OnClick = GamePauseClick
    end
    object TakebackGame: TMenuItem
      Caption = 'Takeback'
      Visible = False
      OnClick = TakebackGameClick
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object LookFeelOptionsGame: TMenuItem
      Action = OptionsAction
      Caption = 'Look && Feel Options...'
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object AboutGame: TMenuItem
      Action = AboutAction
    end
  end
  object ConnectorTimer: TTimer
    Enabled = False
    Interval = 500
    OnTimer = ConnectorTimerTimer
    Left = 8
    Top = 40
  end
end
