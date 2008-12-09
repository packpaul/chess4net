object GameOptionsForm: TGameOptionsForm
  Left = 486
  Top = 202
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Game Options'
  ClientHeight = 503
  ClientWidth = 365
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object OkButton: TButton
    Left = 280
    Top = 16
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object CancelButton: TButton
    Left = 280
    Top = 48
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 257
    Height = 265
    Caption = 'Time Control'
    TabOrder = 0
    object EqualTimeCheckBox: TCheckBox
      Left = 16
      Top = 24
      Width = 153
      Height = 17
      Caption = 'Equal time for both players'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = EqualTimeCheckBoxClick
    end
    object YouGroupBox: TGroupBox
      Left = 32
      Top = 48
      Width = 193
      Height = 97
      Caption = 'Your time'
      TabOrder = 1
      object YouMinLabel: TLabel
        Left = 16
        Top = 40
        Width = 87
        Height = 13
        Caption = 'Minutes per game:'
      end
      object YouIncLabel: TLabel
        Left = 16
        Top = 64
        Width = 104
        Height = 13
        Caption = 'Increment in seconds:'
      end
      object YouMinEdit: TEdit
        Left = 128
        Top = 40
        Width = 41
        Height = 21
        BiDiMode = bdLeftToRight
        MaxLength = 3
        ParentBiDiMode = False
        TabOrder = 1
        Text = '5'
        OnChange = YouEditChange
      end
      object YouIncEdit: TEdit
        Left = 128
        Top = 64
        Width = 41
        Height = 21
        TabOrder = 2
        Text = '0'
        OnChange = YouEditChange
      end
      object YouUnlimitedCheckBox: TCheckBox
        Left = 16
        Top = 16
        Width = 65
        Height = 17
        Caption = 'Unlimited'
        TabOrder = 0
        OnClick = UnlimitedCheckBoxClick
      end
      object YouMinUpDown: TUpDown
        Left = 169
        Top = 40
        Width = 15
        Height = 21
        Associate = YouMinEdit
        Min = 1
        Max = 999
        Position = 5
        TabOrder = 3
      end
      object YouIncUpDown: TUpDown
        Left = 169
        Top = 64
        Width = 15
        Height = 21
        Associate = YouIncEdit
        Max = 999
        TabOrder = 4
      end
    end
    object OpponentGroupBox: TGroupBox
      Left = 32
      Top = 152
      Width = 193
      Height = 97
      Caption = 'Opponent'#39's time'
      TabOrder = 2
      object OpponentMinLabel: TLabel
        Left = 16
        Top = 40
        Width = 87
        Height = 13
        Caption = 'Minutes per game:'
      end
      object OpponentIncLabel: TLabel
        Left = 16
        Top = 64
        Width = 104
        Height = 13
        Caption = 'Increment in seconds:'
      end
      object OpponentIncEdit: TEdit
        Left = 128
        Top = 64
        Width = 41
        Height = 21
        TabOrder = 2
        Text = '0'
        OnChange = OpponentEditChange
      end
      object OpponentMinEdit: TEdit
        Left = 128
        Top = 40
        Width = 41
        Height = 21
        BiDiMode = bdLeftToRight
        MaxLength = 3
        ParentBiDiMode = False
        TabOrder = 1
        Text = '5'
        OnChange = OpponentEditChange
      end
      object OpponentUnlimitedCheckBox: TCheckBox
        Left = 16
        Top = 16
        Width = 65
        Height = 17
        Caption = 'Unlimited'
        TabOrder = 0
        OnClick = UnlimitedCheckBoxClick
      end
      object OpponentMinUpDown: TUpDown
        Left = 169
        Top = 40
        Width = 15
        Height = 21
        Associate = OpponentMinEdit
        Min = 1
        Max = 999
        Position = 5
        TabOrder = 3
      end
      object OpponentIncUpDown: TUpDown
        Left = 169
        Top = 64
        Width = 15
        Height = 21
        Associate = OpponentIncEdit
        Max = 999
        TabOrder = 4
      end
    end
  end
  object Panel1: TPanel
    Left = 8
    Top = 390
    Width = 257
    Height = 105
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 2
    object AutoFlagCheckBox: TCheckBox
      Left = 8
      Top = 80
      Width = 73
      Height = 17
      Caption = 'Auto Flag'
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
    object TakeBackCheckBox: TCheckBox
      Left = 8
      Top = 56
      Width = 177
      Height = 17
      Caption = 'Allow takebacks to your partner'
      TabOrder = 2
    end
    object GamePauseCheckBox: TCheckBox
      Left = 8
      Top = 8
      Width = 129
      Height = 17
      Caption = 'Game can be paused'
      TabOrder = 0
    end
    object GameAdjournCheckBox: TCheckBox
      Left = 8
      Top = 32
      Width = 145
      Height = 17
      Caption = 'Game can be adjourned'
      TabOrder = 1
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 280
    Width = 257
    Height = 97
    Caption = 'Training Mode'
    TabOrder = 1
    object ExtBaseLabel: TLabel
      Left = 16
      Top = 40
      Width = 67
      Height = 13
      Caption = 'External base:'
    end
    object TrainingEnabledCheckBox: TCheckBox
      Left = 16
      Top = 16
      Width = 97
      Height = 17
      Caption = 'Enabled'
      TabOrder = 0
      OnClick = TrainingEnabledCheckBoxClick
    end
    object ExtBaseComboBox: TComboBox
      Left = 104
      Top = 36
      Width = 137
      Height = 21
      Enabled = False
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 1
      Text = '<No>'
      OnChange = ExtBaseComboBoxChange
      Items.Strings = (
        '<No>')
    end
    object UsrBaseCheckBox: TCheckBox
      Left = 104
      Top = 64
      Width = 97
      Height = 17
      Caption = 'Use user base'
      Enabled = False
      TabOrder = 2
    end
  end
end
