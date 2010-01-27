object MainForm: TMainForm
  Left = 250
  Top = 283
  Width = 844
  Height = 309
  Caption = 'SkypeTS'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Microsoft Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Skype1GroupBox: TGroupBox
    Left = 8
    Top = 8
    Width = 401
    Height = 257
    Caption = 'Skype1'
    TabOrder = 0
    object Handle1Label: TLabel
      Left = 16
      Top = 19
      Width = 35
      Height = 13
      Caption = 'handle:'
    end
    object FullName1Label: TLabel
      Left = 16
      Top = 43
      Width = 45
      Height = 13
      Caption = 'full name:'
    end
    object DisplayName1Label: TLabel
      Left = 16
      Top = 67
      Width = 64
      Height = 13
      Caption = 'display name:'
    end
    object Bevel1: TBevel
      Left = 8
      Top = 93
      Width = 385
      Height = 1
    end
    object IM1Label: TLabel
      Left = 8
      Top = 104
      Width = 85
      Height = 13
      Caption = 'Instant messages:'
    end
    object Handle1Edit: TTntEdit
      Left = 88
      Top = 16
      Width = 177
      Height = 21
      TabOrder = 0
      Text = 'handle1'
    end
    object FullName1Edit: TTntEdit
      Left = 88
      Top = 40
      Width = 177
      Height = 21
      TabOrder = 1
      Text = '<Full name 1>'
    end
    object DisplayName1Edit: TTntEdit
      Left = 88
      Top = 64
      Width = 177
      Height = 21
      TabOrder = 2
      Text = '<Display name 1>'
    end
    object Attach1Button: TButton
      Left = 296
      Top = 40
      Width = 75
      Height = 25
      Caption = 'Attach'
      TabOrder = 3
      OnClick = Attach1ButtonClick
    end
    object SendIM1Edit: TTntEdit
      Left = 8
      Top = 218
      Width = 297
      Height = 21
      TabOrder = 5
    end
    object SendIM1Button: TButton
      Left = 316
      Top = 216
      Width = 75
      Height = 25
      Caption = 'Send IM'
      TabOrder = 6
      OnClick = SendIM1ButtonClick
    end
    object IM1Memo: TTntMemo
      Left = 8
      Top = 120
      Width = 382
      Height = 89
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 4
    end
  end
  object Skype2GroupBox: TGroupBox
    Left = 420
    Top = 8
    Width = 401
    Height = 257
    Caption = 'Skype2'
    TabOrder = 1
    object Handle2Label: TLabel
      Left = 16
      Top = 19
      Width = 35
      Height = 13
      Caption = 'handle:'
    end
    object FullName2Label: TLabel
      Left = 16
      Top = 43
      Width = 45
      Height = 13
      Caption = 'full name:'
    end
    object DisplayName2Label: TLabel
      Left = 16
      Top = 67
      Width = 64
      Height = 13
      Caption = 'display name:'
    end
    object Bevel2: TBevel
      Left = 8
      Top = 93
      Width = 385
      Height = 1
    end
    object IM2Label: TLabel
      Left = 8
      Top = 104
      Width = 85
      Height = 13
      Caption = 'Instant messages:'
    end
    object Handle2Edit: TTntEdit
      Left = 88
      Top = 16
      Width = 177
      Height = 21
      TabOrder = 0
      Text = 'handle2'
    end
    object FullName2Edit: TTntEdit
      Left = 88
      Top = 40
      Width = 177
      Height = 21
      TabOrder = 1
      Text = '<Full name 2>'
    end
    object DisplayName2Edit: TTntEdit
      Left = 88
      Top = 64
      Width = 177
      Height = 21
      TabOrder = 2
      Text = '<Display name 2>'
    end
    object Attach2Button: TButton
      Left = 296
      Top = 40
      Width = 75
      Height = 25
      Caption = 'Attach'
      TabOrder = 3
      OnClick = Attach2ButtonClick
    end
    object SendIM2Edit: TTntEdit
      Left = 8
      Top = 218
      Width = 297
      Height = 21
      TabOrder = 5
    end
    object SendIM2Button: TButton
      Left = 316
      Top = 216
      Width = 75
      Height = 25
      Caption = 'Send IM'
      TabOrder = 6
      OnClick = SendIM2ButtonClick
    end
    object IM2Memo: TTntMemo
      Left = 8
      Top = 120
      Width = 382
      Height = 89
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 4
    end
  end
end
