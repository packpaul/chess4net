object AnalysisModeSelectionForm: TAnalysisModeSelectionForm
  Left = 579
  Top = 449
  BorderStyle = bsDialog
  Caption = 'Analysis mode'
  ClientHeight = 147
  ClientWidth = 257
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  DesignSize = (
    257
    147)
  PixelsPerInch = 96
  TextHeight = 13
  object CancelButton: TButton
    Left = 97
    Top = 115
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object OKButton: TButton
    Left = 177
    Top = 115
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object ReplyMoveRadioGroup: TRadioGroup
    Left = 8
    Top = 8
    Width = 244
    Height = 100
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Reply move selection'
    ItemIndex = 0
    Items.Strings = (
      'Select first move of line'
      'Select random move based on tree weight'
      'Select random move')
    TabOrder = 2
  end
end
