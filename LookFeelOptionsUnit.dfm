object LookFeelOptionsForm: TLookFeelOptionsForm
  Left = 305
  Top = 196
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Look & Feel Options'
  ClientHeight = 185
  ClientWidth = 280
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object AnimateLabel: TLabel
    Left = 8
    Top = 8
    Width = 68
    Height = 13
    Caption = 'Animate Move'
  end
  object OkButton: TButton
    Left = 192
    Top = 24
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object CancelButton: TButton
    Left = 192
    Top = 56
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object AnimationComboBox: TComboBox
    Left = 88
    Top = 8
    Width = 65
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 0
    Text = 'No'
    Items.Strings = (
      'No'
      'Slowly'
      'Quickly')
  end
  object BoxPanel: TPanel
    Left = 8
    Top = 40
    Width = 161
    Height = 129
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 1
    object HilightLastMoveBox: TCheckBox
      Left = 8
      Top = 8
      Width = 145
      Height = 17
      Caption = 'Highlight Last Move'
      TabOrder = 0
    end
    object CoordinatesBox: TCheckBox
      Left = 8
      Top = 56
      Width = 145
      Height = 17
      Caption = 'Show Coordinates'
      TabOrder = 1
    end
    object StayOnTopBox: TCheckBox
      Left = 8
      Top = 80
      Width = 145
      Height = 17
      Caption = 'Stay Always on Top'
      TabOrder = 2
    end
    object ExtraExitBox: TCheckBox
      Left = 8
      Top = 104
      Width = 145
      Height = 17
      Caption = 'Extra Exit on ESC'
      TabOrder = 3
    end
    object FlashIncomingMoveBox: TCheckBox
      Left = 8
      Top = 32
      Width = 145
      Height = 17
      Caption = 'Flash on incoming move'
      TabOrder = 4
    end
  end
end
