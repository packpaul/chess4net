object PositionEditingForm: TPositionEditingForm
  Left = 430
  Top = 636
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Edit Position'
  ClientHeight = 188
  ClientWidth = 443
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Image7: TImage
    Left = 16
    Top = 48
    Width = 40
    Height = 40
  end
  object FENLabeledEdit: TLabeledEdit
    Left = 34
    Top = 161
    Width = 319
    Height = 21
    EditLabel.Width = 24
    EditLabel.Height = 13
    EditLabel.Caption = 'FEN:'
    LabelPosition = lpLeft
    TabOrder = 11
    OnChange = FENLabeledEditChange
    OnExit = FENLabeledEditExit
    OnKeyPress = FENLabeledEditKeyPress
  end
  object EmptyButton: TButton
    Left = 296
    Top = 8
    Width = 139
    Height = 25
    Caption = '&Empty'
    TabOrder = 0
    OnClick = EmptyButtonClick
  end
  object InitialButton: TButton
    Left = 296
    Top = 40
    Width = 139
    Height = 25
    Caption = '&Initial'
    TabOrder = 1
    OnClick = InitialButtonClick
  end
  object CloseButton: TButton
    Left = 360
    Top = 157
    Width = 75
    Height = 25
    Caption = '&Close'
    TabOrder = 12
    OnClick = CloseButtonClick
  end
  object SpecialButton: TButton
    Left = 296
    Top = 72
    Width = 139
    Height = 25
    Caption = '&Special...'
    TabOrder = 2
  end
  object PieceSelectionPanel: TPanel
    Left = 8
    Top = 8
    Width = 277
    Height = 100
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 3
    object WKImage: TImage
      Left = 8
      Top = 8
      Width = 40
      Height = 40
      OnMouseDown = PieceImageMouseDown
    end
    object BQImage: TImage
      Tag = 8
      Left = 52
      Top = 52
      Width = 40
      Height = 40
      OnMouseDown = PieceImageMouseDown
    end
    object WQImage: TImage
      Tag = 1
      Left = 52
      Top = 8
      Width = 40
      Height = 40
      OnMouseDown = PieceImageMouseDown
    end
    object BRImage: TImage
      Tag = 9
      Left = 96
      Top = 52
      Width = 40
      Height = 40
      OnMouseDown = PieceImageMouseDown
    end
    object WRImage: TImage
      Tag = 2
      Left = 96
      Top = 8
      Width = 40
      Height = 40
      OnMouseDown = PieceImageMouseDown
    end
    object BBImage: TImage
      Tag = 10
      Left = 140
      Top = 52
      Width = 40
      Height = 40
      OnMouseDown = PieceImageMouseDown
    end
    object WBImage: TImage
      Tag = 3
      Left = 140
      Top = 8
      Width = 40
      Height = 40
      OnMouseDown = PieceImageMouseDown
    end
    object BNImage: TImage
      Tag = 11
      Left = 184
      Top = 52
      Width = 40
      Height = 40
      OnMouseDown = PieceImageMouseDown
    end
    object WNImage: TImage
      Tag = 4
      Left = 184
      Top = 8
      Width = 40
      Height = 40
      OnMouseDown = PieceImageMouseDown
    end
    object BPImage: TImage
      Tag = 12
      Left = 228
      Top = 52
      Width = 40
      Height = 40
      OnMouseDown = PieceImageMouseDown
    end
    object WPImage: TImage
      Tag = 5
      Left = 228
      Top = 8
      Width = 40
      Height = 40
      OnMouseDown = PieceImageMouseDown
    end
    object BKImage: TImage
      Tag = 7
      Left = 8
      Top = 52
      Width = 40
      Height = 40
      OnMouseDown = PieceImageMouseDown
    end
  end
  object ColorRadioGroup: TRadioGroup
    Left = 8
    Top = 112
    Width = 129
    Height = 41
    Caption = 'Side to move'
    Columns = 2
    ItemIndex = 0
    Items.Strings = (
      'white'
      'black')
    TabOrder = 4
  end
  object EPFileComboBox: TComboBox
    Left = 387
    Top = 105
    Width = 49
    Height = 21
    Style = csDropDownList
    DropDownCount = 9
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 7
    Text = '<no>'
    Items.Strings = (
      '<no>'
      'a'
      'b'
      'c'
      'd'
      'e'
      'f'
      'g'
      'h')
  end
  object EPFileStaticText: TStaticText
    Left = 344
    Top = 107
    Width = 41
    Height = 17
    Caption = 'e.p. file:'
    TabOrder = 6
  end
  object MoveNEdit: TEdit
    Left = 388
    Top = 129
    Width = 30
    Height = 21
    TabOrder = 10
    Text = '1'
    OnChange = MoveNEditChange
  end
  object MoveNUpDown: TUpDown
    Left = 418
    Top = 129
    Width = 16
    Height = 21
    Associate = MoveNEdit
    Min = 1
    Max = 299
    Position = 1
    TabOrder = 9
  end
  object MoveNStaticText: TStaticText
    Left = 343
    Top = 131
    Width = 43
    Height = 17
    Caption = 'move #:'
    TabOrder = 8
  end
  object CastlingGroupBox: TGroupBox
    Left = 142
    Top = 112
    Width = 195
    Height = 41
    Caption = 'Castling capability'
    TabOrder = 5
    object CCWhiteStaticText: TStaticText
      Left = 4
      Top = 18
      Width = 32
      Height = 17
      Caption = 'white:'
      TabOrder = 0
    end
    object CCWhiteComboBox: TComboBox
      Left = 36
      Top = 15
      Width = 60
      Height = 21
      Style = csDropDownList
      DropDownCount = 9
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 1
      Text = '<both>'
      Items.Strings = (
        '<both>'
        '0-0'
        '0-0-0'
        '<no>')
    end
    object CCBlackStaticText: TStaticText
      Left = 98
      Top = 18
      Width = 33
      Height = 17
      Caption = 'black:'
      TabOrder = 2
    end
    object CCBlackComboBox: TComboBox
      Tag = 1
      Left = 130
      Top = 15
      Width = 60
      Height = 21
      Style = csDropDownList
      DropDownCount = 9
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 3
      Text = '<both>'
      Items.Strings = (
        '<both>'
        '0-0'
        '0-0-0'
        '<no>')
    end
  end
end
