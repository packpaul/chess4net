object MainForm: TMainForm
  Left = 274
  Top = 322
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'IMEClient'
  ClientHeight = 248
  ClientWidth = 407
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefault
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 248
    Top = 8
    Width = 51
    Height = 13
    Caption = 'Handle ID:'
  end
  object Label2: TLabel
    Left = 248
    Top = 48
    Width = 66
    Height = 13
    Caption = 'Handle name:'
  end
  object memIn: TMemo
    Left = 8
    Top = 8
    Width = 233
    Height = 161
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object memOut: TMemo
    Left = 8
    Top = 176
    Width = 233
    Height = 33
    ReadOnly = True
    TabOrder = 0
    OnChange = memOutChange
  end
  object btnSend: TButton
    Left = 80
    Top = 216
    Width = 75
    Height = 25
    Caption = 'Send'
    TabOrder = 1
    OnClick = btnSendClick
  end
  object HandleNameEdit: TEdit
    Left = 248
    Top = 64
    Width = 153
    Height = 21
    TabOrder = 4
    Text = 'HandleName'
    OnChange = HandleNameEditChange
  end
  object HandleIdEdit: TMaskEdit
    Left = 248
    Top = 24
    Width = 153
    Height = 21
    EditMask = '9999;1;_'
    MaxLength = 4
    TabOrder = 3
    Text = '1234'
    OnChange = HandleIdEditChange
  end
  object DisConnectButton: TButton
    Left = 288
    Top = 216
    Width = 75
    Height = 25
    Caption = 'Connect'
    TabOrder = 5
    OnClick = DisConnectButtonClick
  end
  object ContactsListBox: TListBox
    Left = 248
    Top = 96
    Width = 153
    Height = 113
    ItemHeight = 13
    TabOrder = 6
    OnClick = ContactsListBoxClick
  end
end
