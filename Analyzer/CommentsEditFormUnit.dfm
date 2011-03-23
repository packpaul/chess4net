object CommentsEditForm: TCommentsEditForm
  Left = 454
  Top = 338
  Width = 391
  Height = 203
  BorderIcons = [biSystemMenu]
  Caption = 'Edit Comments'
  Color = clBtnFace
  Constraints.MinHeight = 110
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
    375
    167)
  PixelsPerInch = 96
  TextHeight = 13
  object OkButton: TButton
    Left = 294
    Top = 135
    Width = 75
    Height = 25
    Hint = 'OK (Ctrl+Enter)'
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    Default = True
    ModalResult = 1
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
  end
  object CommentsMemo: TTntMemo
    Left = 0
    Top = 0
    Width = 375
    Height = 125
    Align = alTop
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
    Left = 216
    Top = 135
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 1
  end
end
