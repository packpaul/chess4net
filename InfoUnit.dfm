object InfoForm: TInfoForm
  Left = 489
  Top = 506
  BorderStyle = bsDialog
  ClientHeight = 187
  ClientWidth = 241
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Microsoft Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PluginNameLabel: TLabel
    Left = 4
    Top = 8
    Width = 233
    Height = 17
    Alignment = taCenter
    AutoSize = False
    Caption = 'Chess4Net <version>'
    WordWrap = True
  end
  object PlayingViaLabel: TLabel
    Left = 4
    Top = 27
    Width = 233
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'Plugin for playing chess over <Messenger name>'
  end
  object Label2: TLabel
    Left = 4
    Top = 46
    Width = 233
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'Written by Pavel Perminov'
  end
  object Label3: TLabel
    Left = 4
    Top = 65
    Width = 233
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = #169' 2007-2010 All rights reserved'
  end
  object Label4: TLabel
    Left = 42
    Top = 84
    Width = 31
    Height = 13
    AutoSize = False
    Caption = 'URLs:'
  end
  object Label5: TLabel
    Left = 42
    Top = 122
    Width = 32
    Height = 13
    AutoSize = False
    Caption = 'E-Mail:'
  end
  object URLLabel: TLabel
    Left = 77
    Top = 84
    Width = 65
    Height = 13
    Cursor = crHandPoint
    Caption = 'http://<URL>'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    ParentFont = False
    OnClick = URLLabelClick
  end
  object EMailLabel: TLabel
    Left = 77
    Top = 122
    Width = 41
    Height = 13
    Cursor = crHandPoint
    Caption = '<E-Mail>'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    ParentFont = False
    OnClick = EMailLabelClick
  end
  object URL2Label: TLabel
    Left = 77
    Top = 103
    Width = 71
    Height = 13
    Cursor = crHandPoint
    Caption = 'http://<URL2>'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    ParentFont = False
    OnClick = URLLabelClick
  end
  object OkButton: TButton
    Left = 84
    Top = 148
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = OkButtonClick
  end
end
