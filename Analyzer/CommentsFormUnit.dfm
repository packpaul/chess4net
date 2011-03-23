object CommentsForm: TCommentsForm
  Left = 429
  Top = 635
  Width = 591
  Height = 128
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeToolWin
  Caption = 'Comments'
  Color = clBtnFace
  Constraints.MinHeight = 110
  Constraints.MinWidth = 150
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
    575
    92)
  PixelsPerInch = 96
  TextHeight = 13
  object EditButton: TButton
    Left = 495
    Top = 62
    Width = 75
    Height = 25
    Action = AnalyseChessBoard.EditCommentAction
    Anchors = [akRight, akBottom]
    Caption = '&Edit'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
  end
  object CommentsMemo: TTntMemo
    Left = 0
    Top = 0
    Width = 575
    Height = 57
    Align = alTop
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
