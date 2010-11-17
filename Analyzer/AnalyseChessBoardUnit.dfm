object AnalyseChessBoard: TAnalyseChessBoard
  Left = 429
  Top = 209
  Width = 435
  Height = 474
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'Chess4Net Analyzer <ver.>'
  Color = clBtnFace
  TransparentColorValue = clBackground
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = [fsBold]
  KeyPreview = True
  Menu = MainMenu
  OldCreateOrder = False
  OnCanResize = FormCanResize
  OnCreate = FormCreate
  DesignSize = (
    427
    428)
  PixelsPerInch = 96
  TextHeight = 16
  object ChessBoardPanel: TPanel
    Left = 0
    Top = 0
    Width = 356
    Height = 352
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 0
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 409
    Width = 427
    Height = 19
    Panels = <>
  end
  object MainMenu: TTntMainMenu
    object FileMenuItem: TTntMenuItem
      Caption = '&File'
      object OpenPGNMenuItem: TTntMenuItem
        Caption = '&Open PGN...'
      end
      object SavePGNMenuItem: TTntMenuItem
        Caption = '&Save PGN...'
      end
      object N2: TTntMenuItem
        Caption = '-'
      end
      object CopyPGNMenuItem: TTntMenuItem
        Caption = '&Copy PGN'
      end
      object PastePGNMenuItem: TTntMenuItem
        Caption = '&Paste PGN'
      end
      object N1: TTntMenuItem
        Caption = '-'
      end
      object ExitMenuItem: TTntMenuItem
        Caption = 'E&xit'
        OnClick = ExitMenuItemClick
      end
    end
    object HelpMenuItem: TTntMenuItem
      Caption = '&Help'
      object ContentsMenuItem: TTntMenuItem
        Caption = 'Contents...'
      end
      object N3: TTntMenuItem
        Caption = '-'
      end
      object AboutMenuItem: TTntMenuItem
        Caption = 'About...'
      end
    end
  end
end
