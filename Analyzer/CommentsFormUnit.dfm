object CommentsForm: TCommentsForm
  Left = 429
  Top = 635
  Width = 601
  Height = 122
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeToolWin
  Caption = 'Comments'
  Color = clBtnFace
  Constraints.MinHeight = 90
  Constraints.MinWidth = 100
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    593
    95)
  PixelsPerInch = 96
  TextHeight = 13
  object EditButton: TButton
    Left = 512
    Top = 64
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Edit'
    TabOrder = 1
    OnClick = EditButtonClick
  end
  object CommentsMemo: TTntMemo
    Left = 0
    Top = 0
    Width = 593
    Height = 57
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Microsoft Sans Serif'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
  end
end
