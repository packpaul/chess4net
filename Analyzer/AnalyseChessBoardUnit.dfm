object AnalyseChessBoard: TAnalyseChessBoard
  Left = 429
  Top = 209
  Width = 364
  Height = 417
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'Chess4Net Analyzer 2011.0 gamma'
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
  PopupMenu = PopupMenu
  OnCanResize = FormCanResize
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    356
    371)
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
  object StatusBar: TStatusBar
    Left = 0
    Top = 352
    Width = 356
    Height = 19
    Panels = <>
  end
  object MainMenu: TTntMainMenu
    object FileMenuItem: TTntMenuItem
      Caption = '&File'
      object OpenPGNMenuItem: TTntMenuItem
        Caption = '&Open PGN...'
        ShortCut = 16463
        OnClick = OpenPGNMenuItemClick
      end
      object SavePGNMenuItem: TTntMenuItem
        Caption = '&Save PGN...'
        Enabled = False
        ShortCut = 16467
      end
      object N2: TTntMenuItem
        Caption = '-'
      end
      object PastePGNMenuItem: TTntMenuItem
        Caption = '&Paste PGN'
        ShortCut = 16470
        OnClick = PastePGNMenuItemClick
      end
      object CopyPGNMenuItem: TTntMenuItem
        Caption = '&Copy PGN'
        Enabled = False
        ShortCut = 16451
      end
      object N1: TTntMenuItem
        Caption = '-'
      end
      object ExitMenuItem: TTntMenuItem
        Caption = 'E&xit'
        ShortCut = 32856
        OnClick = ExitMenuItemClick
      end
    end
    object ViewMenuItem: TTntMenuItem
      Caption = '&View'
      object ViewFlipBoardMenuItem: TTntMenuItem
        Caption = '&Flip Board'
        ShortCut = 70
        OnClick = ViewFlipBoardMenuItemClick
      end
      object N5: TTntMenuItem
        Caption = '-'
      end
      object ViewMoveListMenuItem: TTntMenuItem
        Caption = '&Move List'
        Enabled = False
        RadioItem = True
      end
      object ViewPosDBManagerMenuItem: TTntMenuItem
        Caption = '&Pos DB Manager'
        Enabled = False
      end
      object ViewChessEngineInfoMenuItem: TTntMenuItem
        Action = ChessEngineInfoAction
      end
    end
    object PositionMenuItem: TTntMenuItem
      Caption = '&Position'
      object PositionInitialMenuItem: TTntMenuItem
        Caption = '&Initial'
        OnClick = PositionInitialMenuItemClick
      end
      object N4: TTntMenuItem
        Caption = '-'
      end
      object PositionTakebackMoveMenuItem: TTntMenuItem
        Caption = '&Takeback Move'
        ShortCut = 37
        OnClick = PositionTakebackMoveMenuItemClick
      end
      object PositionForwardMoveMenuItem: TTntMenuItem
        Caption = '&Forward Move'
        ShortCut = 39
        OnClick = PositionForwardMoveMenuItemClick
      end
    end
    object HelpMenuItem: TTntMenuItem
      Caption = '&Help'
      object ContentsMenuItem: TTntMenuItem
        Caption = 'Contents...'
        Enabled = False
      end
      object N3: TTntMenuItem
        Caption = '-'
      end
      object AboutMenuItem: TTntMenuItem
        Caption = 'About...'
        Enabled = False
      end
    end
  end
  object PopupMenu: TTntPopupMenu
    Left = 32
    object PopupTakebackMoveMenuItem: TTntMenuItem
      Caption = 'Takeback Move'
      OnClick = PositionTakebackMoveMenuItemClick
    end
    object PopupForwardMoveMenuItem: TTntMenuItem
      Caption = 'Forward Move'
      OnClick = PositionForwardMoveMenuItemClick
    end
  end
  object OpenPGNDialog: TOpenDialog
    Filter = 'PGN Files (*.pgn)|*.pgn'
    Top = 32
  end
  object ActionList: TActionList
    Left = 64
    object ChessEngineInfoAction: TAction
      Caption = 'Chess &Engine Info'
      OnExecute = ChessEngineInfoActionExecute
      OnUpdate = ChessEngineInfoActionUpdate
    end
  end
end
