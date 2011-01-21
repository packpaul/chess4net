object SelectLineForm: TSelectLineForm
  Left = 579
  Top = 449
  BorderStyle = bsDialog
  Caption = 'Select Line'
  ClientHeight = 144
  ClientWidth = 200
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object MovesListBox: TListBox
    Left = 0
    Top = 0
    Width = 200
    Height = 105
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 0
    OnDblClick = MovesListBoxDblClick
  end
  object CancelButton: TButton
    Left = 40
    Top = 112
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    TabOrder = 2
    OnClick = CancelButtonClick
  end
  object OKButton: TButton
    Left = 120
    Top = 112
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    TabOrder = 1
    OnClick = OKButtonClick
  end
end
