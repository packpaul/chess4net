object SkypeFrame: TSkypeFrame
  Left = 0
  Top = 0
  Width = 401
  Height = 257
  TabOrder = 0
  object SkypeGroupBox: TGroupBox
    Left = 0
    Top = 0
    Width = 401
    Height = 257
    Caption = 'Skype'
    TabOrder = 0
    object HandleLabel: TLabel
      Left = 16
      Top = 19
      Width = 35
      Height = 13
      Caption = 'handle:'
    end
    object FullNameLabel: TLabel
      Left = 16
      Top = 43
      Width = 45
      Height = 13
      Caption = 'full name:'
    end
    object DisplayNameLabel: TLabel
      Left = 16
      Top = 67
      Width = 64
      Height = 13
      Caption = 'display name:'
    end
    object Bevel: TBevel
      Left = 8
      Top = 93
      Width = 385
      Height = 1
    end
    object IMLabel: TLabel
      Left = 8
      Top = 104
      Width = 85
      Height = 13
      Caption = 'Instant messages:'
    end
    object ContactLabel: TLabel
      Left = 72
      Top = 194
      Width = 40
      Height = 13
      Caption = 'Contact:'
    end
    object HandleEdit: TTntEdit
      Left = 88
      Top = 16
      Width = 177
      Height = 21
      TabOrder = 0
      Text = 'handle'
    end
    object FullNameEdit: TTntEdit
      Left = 88
      Top = 40
      Width = 177
      Height = 21
      TabOrder = 1
      Text = '<Full name>'
    end
    object DisplayNameEdit: TTntEdit
      Left = 88
      Top = 64
      Width = 177
      Height = 21
      TabOrder = 2
      Text = '<Display name>'
    end
    object AttachButton: TButton
      Left = 296
      Top = 40
      Width = 75
      Height = 25
      Caption = 'Attach'
      TabOrder = 3
      OnClick = AttachButtonClick
    end
    object SendIMEdit: TTntEdit
      Left = 8
      Top = 218
      Width = 297
      Height = 21
      TabOrder = 6
    end
    object SendIMButton: TButton
      Left = 316
      Top = 192
      Width = 75
      Height = 49
      Caption = 'Send IM'
      TabOrder = 7
      OnClick = SendIMButtonClick
    end
    object IMMemo: TTntMemo
      Left = 8
      Top = 120
      Width = 385
      Height = 65
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 4
    end
    object ContactComboBox: TComboBox
      Left = 120
      Top = 190
      Width = 185
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 5
    end
  end
end
