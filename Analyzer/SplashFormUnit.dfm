object SplashForm: TSplashForm
  Left = 303
  Top = 205
  BorderStyle = bsNone
  Caption = 'SplashForm'
  ClientHeight = 341
  ClientWidth = 597
  Color = clSkyBlue
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnHide = FormHide
  PixelsPerInch = 96
  TextHeight = 13
  object VerLabel: TLabel
    Left = 256
    Top = 104
    Width = 67
    Height = 13
    Caption = 'ver. <version>'
  end
  object StaticText2: TStaticText
    Left = 224
    Top = 128
    Width = 141
    Height = 17
    Caption = 'makes you play chess better!'
    TabOrder = 0
  end
  object StaticText3: TStaticText
    Left = 208
    Top = 72
    Width = 170
    Height = 26
    Caption = 'Chess4Net Analyzer'
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Arial Black'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
  end
  object StaticText1: TStaticText
    Left = 496
    Top = 320
    Width = 92
    Height = 17
    Caption = 'by Pavel Perminov'
    TabOrder = 2
  end
  object ShowingTimer: TTimer
    Enabled = False
    Interval = 3500
    OnTimer = ShowingTimerTimer
    Left = 24
    Top = 16
  end
end
