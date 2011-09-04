object GamesListForm: TGamesListForm
  Left = 154
  Top = 211
  Width = 276
  Height = 423
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeToolWin
  Caption = 'Games List'
  Color = clBtnFace
  Constraints.MinHeight = 97
  Constraints.MinWidth = 276
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object GamesListBox: TListBox
    Left = 0
    Top = 0
    Width = 268
    Height = 349
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    OnClick = GamesListBoxClick
  end
  object ApplicationEvents: TApplicationEvents
    OnShowHint = ApplicationEventsShowHint
    Left = 16
    Top = 8
  end
end
