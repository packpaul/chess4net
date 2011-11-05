object TipsOfTheDayForm: TTipsOfTheDayForm
  Left = 457
  Top = 338
  Width = 308
  Height = 186
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
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    300
    159)
  PixelsPerInch = 96
  TextHeight = 13
  object PrevTipButton: TButton
    Left = 62
    Top = 104
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Previous Tip'
    TabOrder = 3
    OnClick = PrevTipButtonClick
  end
  object NextTipButton: TButton
    Left = 142
    Top = 104
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Next Tip'
    TabOrder = 0
    OnClick = NextTipButtonClick
  end
  object CloseButton: TButton
    Left = 222
    Top = 104
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Close'
    TabOrder = 1
    OnClick = CloseButtonClick
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
    ReadOnly = True
    TabOrder = 4
  end
  object ShowOnStartupCheckBox: TCheckBox
    Left = 8
    Top = 136
    Width = 289
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Show on startup'
    TabOrder = 2
    OnClick = ShowOnStartupCheckBoxClick
  end
end
