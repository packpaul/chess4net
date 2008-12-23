object GameLogForm: TGameLogForm
  Left = 836
  Top = 376
  Width = 134
  Height = 214
  BorderStyle = bsSizeToolWin
  Caption = 'Game Log'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object MovesListView: TListView
    Left = 0
    Top = 0
    Width = 126
    Height = 187
    Align = alClient
    Columns = <
      item
        Caption = '#'
        MaxWidth = 20
        MinWidth = 20
        Width = 20
      end
      item
        Caption = 'White'
        MaxWidth = 50
        MinWidth = 50
      end
      item
        Caption = 'Black'
        MaxWidth = 50
        MinWidth = 50
      end>
    PopupMenu = PopupMenu
    TabOrder = 0
    ViewStyle = vsReport
  end
  object PopupMenu: TPopupMenu
    Left = 8
    Top = 24
    object CopyToClipboardItem: TMenuItem
      Caption = 'Copy to clipboard'
      Enabled = False
      OnClick = CopyToClipboardItemClick
    end
  end
end
