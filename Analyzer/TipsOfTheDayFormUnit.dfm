object TipsOfTheDayForm: TTipsOfTheDayForm
  Left = 457
  Top = 418
  Width = 308
  Height = 160
  BorderStyle = bsSizeToolWin
  Caption = 'Tips Of The Day'
  Color = clBtnFace
  Constraints.MinHeight = 160
  Constraints.MinWidth = 308
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    300
    133)
  PixelsPerInch = 96
  TextHeight = 13
  object PrevTipButton: TButton
    Left = 62
    Top = 104
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Previous Tip'
    TabOrder = 2
  end
  object NextTipButton: TButton
    Left = 142
    Top = 104
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Next Tip'
    TabOrder = 0
  end
  object CloseButton: TButton
    Left = 222
    Top = 104
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'CloseButton'
    TabOrder = 1
  end
  object TipMemo: TTntMemo
    Left = 0
    Top = 0
    Width = 297
    Height = 97
    TabStop = False
    Anchors = [akLeft, akTop, akRight, akBottom]
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    Lines.Strings = (
      '<A tip of the day>')
    ParentFont = False
    TabOrder = 3
  end
end
