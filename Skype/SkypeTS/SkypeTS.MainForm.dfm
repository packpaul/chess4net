object MainForm: TMainForm
  Left = 190
  Top = 133
  Width = 844
  Height = 555
  Caption = 'SkypeTS'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Microsoft Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  inline SkypeFrame1: TSkypeFrame
    Left = 8
    Top = 8
    Width = 401
    Height = 257
    TabOrder = 0
    inherited SkypeGroupBox: TGroupBox
      Caption = 'Skype1'
      inherited HandleEdit: TTntEdit
        Text = 'handle1'
      end
      inherited FullNameEdit: TTntEdit
        Text = '<Full name 1>'
      end
      inherited DisplayNameEdit: TTntEdit
        Text = '<Display name 1>'
      end
    end
  end
  inline SkypeFrame2: TSkypeFrame
    Tag = 1
    Left = 416
    Top = 8
    Width = 401
    Height = 257
    TabOrder = 1
    inherited SkypeGroupBox: TGroupBox
      Caption = 'Skype 2'
      inherited HandleEdit: TTntEdit
        Text = 'handle2'
      end
      inherited FullNameEdit: TTntEdit
        Text = '<Full name 2>'
      end
      inherited DisplayNameEdit: TTntEdit
        Text = '<Display name 2>'
      end
    end
  end
  inline SkypeFrame3: TSkypeFrame
    Tag = 2
    Left = 8
    Top = 265
    Width = 401
    Height = 257
    TabOrder = 2
    inherited SkypeGroupBox: TGroupBox
      Caption = 'Skype3'
      inherited HandleEdit: TTntEdit
        Text = 'handle3'
      end
      inherited FullNameEdit: TTntEdit
        Text = '<Full name 3>'
      end
      inherited DisplayNameEdit: TTntEdit
        Text = '<Display name 3>'
      end
    end
  end
  inline SkypeFrame4: TSkypeFrame
    Tag = 3
    Left = 416
    Top = 265
    Width = 401
    Height = 257
    TabOrder = 3
    inherited SkypeGroupBox: TGroupBox
      Caption = 'Skype4'
      inherited HandleEdit: TTntEdit
        Text = 'handle4'
      end
      inherited FullNameEdit: TTntEdit
        Text = '<Full name 4>'
      end
      inherited DisplayNameEdit: TTntEdit
        Text = '<Display name 4>'
      end
    end
  end
end
