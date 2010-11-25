object ChessEngineInfoForm: TChessEngineInfoForm
  Left = 395
  Top = 443
  Width = 380
  Height = 159
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeToolWin
  Caption = 'Chess Engine Info'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnHide = FormHide
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object InfoMemo: TMemo
    Left = 0
    Top = 0
    Width = 372
    Height = 132
    Align = alClient
    ReadOnly = True
    TabOrder = 0
  end
end
