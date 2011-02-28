unit PositionEditingFormUnit;

interface

uses
  Forms, StdCtrls, Controls, ExtCtrls, Classes, Messages,
  //
  ChessRulesEngine, AppEvnts, ComCtrls;

type
  IPositionEditable = interface
    procedure SetEditPiece(Piece: TFigure);
    function SetPosition(const strFEN: string): boolean;
    procedure StopEditing;
  end;

  TLabeledEdit = class(ExtCtrls.TLabeledEdit)
  private
    procedure CNKeyDown(var Message: TWMKeyDown); message CN_KEYDOWN;
  end;

  TEdit = class(StdCtrls.TEdit)
  private
    procedure CNKeyDown(var Message: TWMKeyDown); message CN_KEYDOWN;
  end;

  TPositionEditingForm = class(TForm)
    FENLabeledEdit: TLabeledEdit;
    EmptyButton: TButton;
    InitialButton: TButton;
    CloseButton: TButton;
    SpecialButton: TButton;
    PieceSelectionPanel: TPanel;
    WKImage: TImage;
    BQImage: TImage;
    WQImage: TImage;
    BRImage: TImage;
    WRImage: TImage;
    BBImage: TImage;
    WBImage: TImage;
    BNImage: TImage;
    WNImage: TImage;
    BPImage: TImage;
    WPImage: TImage;
    Image7: TImage;
    BKImage: TImage;
    ColorRadioGroup: TRadioGroup;
    EPFileComboBox: TComboBox;
    EPFileStaticText: TStaticText;
    MoveNEdit: TEdit;
    MoveNUpDown: TUpDown;
    MoveNStaticText: TStaticText;
    CastlingGroupBox: TGroupBox;
    CCWhiteStaticText: TStaticText;
    CCWhiteComboBox: TComboBox;
    CCBlackStaticText: TStaticText;
    CCBlackComboBox: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FENLabeledEditExit(Sender: TObject);
    procedure FENLabeledEditChange(Sender: TObject);
    procedure EmptyButtonClick(Sender: TObject);
    procedure InitialButtonClick(Sender: TObject);
    procedure MoveNEditChange(Sender: TObject);
    procedure PieceImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ColorRadioGroupClick(Sender: TObject);
    procedure CCComboBoxChange(Sender: TObject);
    procedure EPFileComboBoxChange(Sender: TObject);
    procedure FENLabeledEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FENLabeledEditDblClick(Sender: TObject);
  private
    m_PositionEditable: IPositionEditable;
    m_SelectedPiece: TFigure;
    m_bFENChanged: boolean;
    m_ChessRulesEngine: TChessRulesEngine;
    m_bFENUpdating: boolean;
    function FGetPieceImage(Piece: TFigure): TImage;
    procedure FLoadPieces;
    procedure FDrawPieceSelection(Piece: TFigure);
    procedure FErasePieceSelection(Piece: TFigure);
    procedure FSetEditPiece;
    function FSetUserFEN: boolean;
    procedure FSetPosition;
    procedure FStopEditing;

    function FGetFEN: string;
    procedure FSetFEN(const strValue: string);
    function FGetPositionColor: TFigureColor;
    procedure FSetPositionColor(Value: TFigureColor);
    function FGetCastlingCapability: TCastlingCapability;
    procedure FSetCastlingCapability(const Value: TCastlingCapability);
    function FGetEnPassant: integer;
    procedure FSetEnPassant(iValue: integer);
    procedure FSetSelectedPiece(Value: TFigure);
    function FGetMoveNumber: integer;
    procedure FSetMoveNumber(iValue: integer);

    property CastlingCapability: TCastlingCapability
      read FGetCastlingCapability write FSetCastlingCapability;
    property EnPassant: integer read FGetEnPassant write FSetEnPassant;

  public
    property SelectedPiece: TFigure
      read m_SelectedPiece write FSetSelectedPiece;
    property PositionEditable: IPositionEditable
      read m_PositionEditable write m_PositionEditable;
    property FEN: string read FGetFEN write FSetFEN;
    property PositionColor: TFigureColor
      read FGetPositionColor write FSetPositionColor;
    property MoveNumber: integer
      read FGetMoveNumber write FSetMoveNumber;
  end;

implementation

uses
  Windows, Graphics, SysUtils, Dialogs,
  //
  BitmapResUnit;

{$R *.dfm}

type
  TWinControlHlp = class(TWinControl)
  public
    class procedure CNKeyDown(WinControl: TWinControl;
      var Message: TWMKeyDown);
  end;

const
  MSG_FEN_WRONG = 'FEN format is wrong!';

////////////////////////////////////////////////////////////////////////////////
// TPositionEditingForm

procedure TPositionEditingForm.FormCreate(Sender: TObject);
begin
  FLoadPieces;
  FDrawPieceSelection(m_SelectedPiece);

  m_ChessRulesEngine := TChessRulesEngine.Create;
  m_ChessRulesEngine.MoveNotationFormat := mnfCh4NEx;
  m_ChessRulesEngine.FENFormat := TRUE;
end;


procedure TPositionEditingForm.FLoadPieces;

  procedure NDrawToPieceImage(f: TFigure; const PieceBitmap: TBitmap);
  const
    INDENT_SIZE = 2;
  var
    PieceImage: TImage;
  begin
    PieceImage := FGetPieceImage(f);
    Assert(Assigned(PieceImage));

    with PieceImage, PieceImage.Canvas do
    begin
      Brush.Color := Color;
      FillRect(Rect(0, 0, Width, Height));

      Brush.Color := clWhite;
      FillRect(Rect(1, 1, Width - 1, Height - 1));

      Draw(0, 0, PieceBitmap);
    end;

  end;

var
  f: TFigure;
  PieceBitmap: TBitmap;
begin // .FLoadPieces
  with TBitmapRes.Create(WKImage.Height) do
  try
    for f := Low(TFigure) to High(TFigure) do
    begin
      if (f = ES) then
        continue;

      CreateFigureBitmap(f, PieceBitmap);
      try
        NDrawToPieceImage(f, PieceBitmap);
      finally
        FreeAndNil(PieceBitmap);
      end;
    end;

  finally
    Free;
  end;
end;


function TPositionEditingForm.FGetPieceImage(Piece: TFigure): TImage;
var
  i: integer;
  PiceControl: TControl;
begin
  Result := nil;

  for i := 0 to PieceSelectionPanel.ControlCount - 1 do
  begin
    PiceControl := PieceSelectionPanel.Controls[i];
    if ((PiceControl.Tag = Ord(Piece)) and (PiceControl is TImage)) then
    begin
      Result := TImage(PiceControl);
      exit;
    end;
  end;
end;


procedure TPositionEditingForm.PieceImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FSetSelectedPiece(TFigure((Sender as TImage).Tag));
end;


procedure TPositionEditingForm.FSetEditPiece;
begin
  if (Assigned(m_PositionEditable)) then
    m_PositionEditable.SetEditPiece(m_SelectedPiece);
end;


procedure TPositionEditingForm.FDrawPieceSelection(Piece: TFigure);
var
  PieceImage: TImage;
begin
  PieceImage := FGetPieceImage(Piece);
  with PieceImage, PieceImage.Canvas do
  begin
    Brush.Color := clRed;
    FrameRect(Rect(0, 0, Width, Height));
  end;
end;


procedure TPositionEditingForm.FErasePieceSelection(Piece: TFigure);
var
  PieceImage: TImage;
begin
  PieceImage := FGetPieceImage(Piece);
  with PieceImage, PieceImage.Canvas do
  begin
    Brush.Color := Color;
    FrameRect(Rect(0, 0, Width, Height));
  end;
end;


procedure TPositionEditingForm.FormDestroy(Sender: TObject);
begin
  m_PositionEditable := nil;
  m_ChessRulesEngine.Free;
end;


procedure TPositionEditingForm.FormShow(Sender: TObject);
begin
  FSetEditPiece;
end;


procedure TPositionEditingForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;


procedure TPositionEditingForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  FStopEditing;
end;


procedure TPositionEditingForm.FStopEditing;
begin
  if (Assigned(m_PositionEditable)) then
    m_PositionEditable.StopEditing;
end;


procedure TPositionEditingForm.FENLabeledEditKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_RETURN:
    begin
      if (FSetUserFEN) then
        ActiveControl := CloseButton;
    end;

    Ord('A'), Ord('a'):
    begin
      FENLabeledEdit.SelectAll;
    end;
  end;

end;


procedure TPositionEditingForm.FENLabeledEditDblClick(Sender: TObject);
begin
  if (not m_bFENChanged) then
    FENLabeledEdit.SelectAll;
end;


procedure TPositionEditingForm.FSetPosition;
var
  bRes: boolean;
begin
  if (Assigned(m_PositionEditable)) then
  begin
    bRes := m_PositionEditable.SetPosition(m_ChessRulesEngine.GetPosition);
    Assert(bRes);
  end;
end;


function TPositionEditingForm.FSetUserFEN: boolean;
begin
  Result := TRUE;

  if (Assigned(m_PositionEditable)) then
  begin
    Result := m_ChessRulesEngine.SetPosition(FGetFEN);
    if (Result) then
      FSetPosition;
  end;

  if (not Result) then
    MessageDlg(MSG_FEN_WRONG, mtError, [mbOK], 0);
end;


procedure TPositionEditingForm.FENLabeledEditExit(Sender: TObject);
begin
  if (m_bFENChanged) then
    FSetUserFEN;
  m_bFENChanged := FALSE;
end;


procedure TPositionEditingForm.FENLabeledEditChange(Sender: TObject);
begin
  if (m_bFENUpdating) then
    exit;
  m_bFENChanged := TRUE;
end;


function TPositionEditingForm.FGetFEN: string;
begin
  Result := Trim(FENLabeledEdit.Text);
end;


procedure TPositionEditingForm.FSetFEN(const strValue: string);
begin
  m_bFENUpdating := TRUE;
  try
    FENLabeledEdit.Text := strValue;
    m_bFENChanged := FALSE;

    if (not m_ChessRulesEngine.SetPosition(strValue)) then
      exit;

    PositionColor := m_ChessRulesEngine.Position.color;
    CastlingCapability := m_ChessRulesEngine.Position.castling;
    EnPassant := m_ChessRulesEngine.Position.en_passant;
    MoveNumber := m_ChessRulesEngine.GetFENMoveNumber;

  finally
    m_bFENUpdating := FALSE;
  end;
end;


function TPositionEditingForm.FGetPositionColor: TFigureColor;
begin
  if (ColorRadioGroup.ItemIndex = 0) then
    Result := fcWhite
  else
    Result := fcBlack;
end;


procedure TPositionEditingForm.FSetPositionColor(Value: TFigureColor);
begin
  if (Value = fcWhite) then
    ColorRadioGroup.ItemIndex := 0
  else
    ColorRadioGroup.ItemIndex := 1;
end;


function TPositionEditingForm.FGetCastlingCapability: TCastlingCapability;
begin
  Result := [];

  case CCWhiteComboBox.ItemIndex of
    0:
      Result := Result + [WhiteKingSide, WhiteQueenSide];
    1: // 0-0
      Include(Result, WhiteKingSide);
    2: // 0-0-0
      Include(Result, WhiteQueenSide);
    3:
      ;
  end;

  case CCBlackComboBox.ItemIndex of
    0:
      Result := Result + [BlackKingSide, BlackQueenSide];
    1: // 0-0
      Include(Result, BlackKingSide);
    2: // 0-0-0
      Include(Result, BlackQueenSide);
    3:
      ;
  end;
  
end;


procedure TPositionEditingForm.FSetCastlingCapability(const Value: TCastlingCapability);
begin
  if ((WhiteKingSide in Value) and (WhiteQueenSide in Value)) then
    CCWhiteComboBox.ItemIndex := 0 // <both>
  else if (WhiteKingSide in Value) then
    CCWhiteComboBox.ItemIndex := 1 // 0-0
  else if (WhiteQueenSide in Value) then
    CCWhiteComboBox.ItemIndex := 2 // 0-0-0
  else
    CCWhiteComboBox.ItemIndex := 3; // <no>

  if ((BlackKingSide in Value) and (BlackQueenSide in Value)) then
    CCBlackComboBox.ItemIndex := 0 // <both>
  else if (BlackKingSide in Value) then
    CCBlackComboBox.ItemIndex := 1 // 0-0
  else if (BlackQueenSide in Value) then
    CCBlackComboBox.ItemIndex := 2 // 0-0-0
  else
    CCBlackComboBox.ItemIndex := 3; // <no>
end;


function TPositionEditingForm.FGetEnPassant: integer;
begin
  Result := EPFileComboBox.ItemIndex;
  Assert(Result in [0..8]);
end;


procedure TPositionEditingForm.FSetEnPassant(iValue: integer);
begin
  Assert(iValue in [0..8]);
  EPFileComboBox.ItemIndex := iValue;
end;


function TPositionEditingForm.FGetMoveNumber: integer;
begin
  Result := MoveNUpDown.Position;
end;


procedure TPositionEditingForm.FSetMoveNumber(iValue: integer);
begin
  Assert(iValue >= 1);
  MoveNUpDown.Position := iValue;
end;


procedure TPositionEditingForm.EmptyButtonClick(Sender: TObject);
begin
  FSetFEN(EMPTY_FEN);
  FSetPosition;
end;


procedure TPositionEditingForm.InitialButtonClick(Sender: TObject);
begin
  FSetFEN(INITIAL_FEN);
  FSetPosition;
end;


procedure TPositionEditingForm.MoveNEditChange(Sender: TObject);
begin
  if (m_bFENUpdating) then
    exit;

  MoveNEdit.Text := IntToStr(MoveNumber);

  m_ChessRulesEngine.MovesOffset := MoveNumber - 1;
  FSetPosition;
end;


procedure TPositionEditingForm.ColorRadioGroupClick(Sender: TObject);
begin
  if (m_bFENUpdating) then
    exit;

  m_ChessRulesEngine.Position.color := PositionColor;
  FSetPosition;
end;


procedure TPositionEditingForm.CCComboBoxChange(Sender: TObject);
begin
  if (m_bFENUpdating) then
    exit;

  m_ChessRulesEngine.Position.castling := CastlingCapability;
  FSetPosition;
end;


procedure TPositionEditingForm.EPFileComboBoxChange(Sender: TObject);
begin
  if (m_bFENUpdating) then
    exit;

  m_ChessRulesEngine.Position.en_passant := EnPassant;
  FSetPosition;
end;


procedure TPositionEditingForm.FSetSelectedPiece(Value: TFigure);
begin
  if (Value = m_SelectedPiece) then
    exit;

  FErasePieceSelection(m_SelectedPiece);
  FDrawPieceSelection(Value);

  m_SelectedPiece := Value;

  FSetEditPiece;
end;

////////////////////////////////////////////////////////////////////////////////
// TLabeledEdit

procedure TLabeledEdit.CNKeyDown(var Message: TWMKeyDown);
begin
  TWinControlHlp.CNKeyDown(self, Message);
end;

////////////////////////////////////////////////////////////////////////////////
// TLabeledEdit

procedure TEdit.CNKeyDown(var Message: TWMKeyDown);
begin
  TWinControlHlp.CNKeyDown(self, Message);
end;

////////////////////////////////////////////////////////////////////////////////
// TWinControlHlp

class procedure TWinControlHlp.CNKeyDown(WinControl: TWinControl;
  var Message: TWMKeyDown);
var
  WCHlp: TWinControlHlp;
  Mask: Integer;
begin
  WCHlp := TWinControlHlp(WinControl);

  with Message do
  begin
    Result := 1;
    WCHlp.UpdateUIState(Message.CharCode);
//    if IsMenuKey(Message) then Exit;
    if not (csDesigning in WCHlp.ComponentState) then
    begin
      if (WCHlp.Perform(CM_CHILDKEY, CharCode, Integer(Self)) <> 0) then
        Exit;
      Mask := 0;
      case CharCode of
        VK_TAB:
          Mask := DLGC_WANTTAB;
        VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN:
          Mask := DLGC_WANTARROWS;
        VK_RETURN, VK_EXECUTE, VK_ESCAPE, VK_CANCEL:
          Mask := DLGC_WANTALLKEYS;
      end;
      if (Mask <> 0) and
         (WCHlp.Perform(CM_WANTSPECIALKEY, CharCode, 0) = 0) and
         (WCHlp.Perform(WM_GETDLGCODE, 0, 0) and Mask = 0) and
         (GetParentForm(WCHlp).Perform(CM_DIALOGKEY, CharCode, KeyData) <> 0) then
        Exit;
    end;
    Result := 0;
  end;
  
end;

end.
