object IncorrectMoveForm: TIncorrectMoveForm
  Left = 482
  Top = 277
  BorderStyle = bsDialog
  ClientHeight = 88
  ClientWidth = 280
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    280
    88)
  PixelsPerInch = 96
  TextHeight = 13
  object MsgLabel: TLabel
    Left = 8
    Top = 8
    Width = 263
    Height = 13
    Alignment = taCenter
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = '<Incorrect move!>'
  end
  object TryAgainButton: TButton
    Left = 160
    Top = 48
    Width = 105
    Height = 25
    Cancel = True
    Caption = '<Try Again>'
    TabOrder = 0
    OnClick = TryAgainButtonClick
  end
  object ShowHintButton: TButton
    Left = 16
    Top = 48
    Width = 105
    Height = 25
    Caption = '<Show Hint>'
    TabOrder = 1
    OnClick = ShowHintButtonClick
  end
end
