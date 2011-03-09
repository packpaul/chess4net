object CommentsEditForm: TCommentsEditForm
  Left = 454
  Top = 338
  Width = 382
  Height = 190
  BorderIcons = [biSystemMenu]
  Caption = 'Edit Comments'
  Color = clBtnFace
  Constraints.MinHeight = 90
  Constraints.MinWidth = 200
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  DesignSize = (
    374
    163)
  PixelsPerInch = 96
  TextHeight = 13
  object OkButton: TButton
    Left = 293
    Top = 132
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object CommentsMemo: TTntMemo
    Left = 0
    Top = 0
    Width = 374
    Height = 125
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Microsoft Sans Serif'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 0
    OnChange = CommentsMemoChange
  end
  object CancelButton: TButton
    Left = 215
    Top = 132
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 1
  end
end
