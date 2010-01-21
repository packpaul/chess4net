object Manager: TManager
  Left = 428
  Top = 220
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
  object MainPopupMenu: TPopupMenu
    AutoPopup = False
    Left = 8
    Top = 8
    object ConnectMain: TMenuItem
      Caption = 'Connect...'
      OnClick = ConnectMainClick
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object LookFeelOptionsMain: TMenuItem
      Action = OptionsAction
      Caption = 'Look && Feel Options...'
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object ExitMain: TMenuItem
      Action = ExitAction
    end
  end
  object ActionList: TActionList
    Left = 176
    Top = 8
    object ExitAction: TAction
      Caption = 'Exit'
      OnExecute = ExitActionExecute
    end
    object OptionsAction: TAction
      Caption = 'Options...'
      OnExecute = OptionsActionExecute
    end
    object CopyAction: TAction
      Category = 'Edit'
      Caption = 'Copy'
    end
    object PasteAction: TAction
      Category = 'Edit'
      Caption = 'Paste'
    end
  end
  object ConnectedPopupMenu: TPopupMenu
    AutoPopup = False
    Left = 40
    Top = 8
    object StartStandartGameConnected: TMenuItem
      Caption = 'Start Standart Game'
      OnClick = StartStandartGameConnectedClick
    end
    object StartPPRandomGameConnected: TMenuItem
      Caption = 'Start PP Random Game'
      OnClick = StartPPRandomGameConnectedClick
    end
    object ChangeColorConnected: TMenuItem
      Caption = 'Change Color'
      OnClick = ChangeColorConnectedClick
    end
    object GameOptionsConnected: TMenuItem
      Caption = 'Game Options...'
      OnClick = GameOptionsConnectedClick
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object LookFeelOptionsConnected: TMenuItem
      Action = OptionsAction
      Caption = 'Look && Feel Options...'
    end
    object MenuItem4: TMenuItem
      Caption = '-'
    end
    object DisconnectConnected: TMenuItem
      Caption = 'Disconnect'
      OnClick = DisconnectConnectedClick
    end
    object ExitConnected: TMenuItem
      Action = ExitAction
    end
  end
  object GamePopupMenu: TPopupMenu
    AutoPopup = False
    Left = 72
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
    object N5: TMenuItem
      Caption = '-'
    end
    object ExitGame: TMenuItem
      Action = ExitAction
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
