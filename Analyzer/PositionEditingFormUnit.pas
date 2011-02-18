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
    procedure FormCreate(Sender: TObject);
    procedure PieceImageClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FENLabeledEditKeyPress(Sender: TObject; var Key: Char);
    procedure FENLabeledEditExit(Sender: TObject);
    procedure FENLabeledEditChange(Sender: TObject);
    procedure EmptyButtonClick(Sender: TObject);
    procedure InitialButtonClick(Sender: TObject);
    procedure MoveNEditChange(Sender: TObject);
  private
    m_PositionEditable: IPositionEditable;
    m_SelectedPiece: TFigure;
    m_bFENChanged: boolean;
    function FGetPieceImage(Piece: TFigure): TImage;
    procedure FLoadPieces;
    procedure FDrawPieceSelection(Piece: TFigure);
    procedure FErasePieceSelection(Piece: TFigure);
    procedure FSetEditPiece;
    function FSetPosition: boolean;
    procedure FStopEditing;
    function FGetFEN: string;
    procedure FSetFEN(const strValue: string);
  public
    property SelectedPiece: TFigure read m_SelectedPiece;
    property PositionEditable: IPositionEditable
      read m_PositionEditable write m_PositionEditable;
    property FEN: string read FGetFEN write FSetFEN;
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


procedure TPositionEditingForm.PieceImageClick(Sender: TObject);
var
  NewSelectedPiece: TFigure;
begin
  NewSelectedPiece := TFigure((Sender as TImage).Tag);
  if (NewSelectedPiece = m_SelectedPiece) then
    exit;

  FErasePieceSelection(m_SelectedPiece);
  FDrawPieceSelection(NewSelectedPiece);

  m_SelectedPiece := NewSelectedPiece;

  FSetEditPiece;
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


procedure TPositionEditingForm.FENLabeledEditKeyPress(Sender: TObject;
  var Key: Char);
begin
  if (Ord(Key) = VK_RETURN) then
  begin
    if (FSetPosition) then
      ActiveControl := CloseButton;
  end;
end;


function TPositionEditingForm.FSetPosition: boolean;
begin
  if (Assigned(m_PositionEditable)) then
    Result := m_PositionEditable.SetPosition(FGetFEN)
  else
    Result := TRUE;

  if (Result) then
    exit;

  MessageDlg(MSG_FEN_WRONG, mtError, [mbOK], 0);
end;


procedure TPositionEditingForm.FENLabeledEditExit(Sender: TObject);
begin
  if (m_bFENChanged) then
    FSetPosition;
  m_bFENChanged := FALSE;
end;


procedure TPositionEditingForm.FENLabeledEditChange(Sender: TObject);
begin
  m_bFENChanged := TRUE;
end;


function TPositionEditingForm.FGetFEN: string;
begin
  Result := Trim(FENLabeledEdit.Text);
end;


procedure TPositionEditingForm.FSetFEN(const strValue: string);
begin
  FENLabeledEdit.Text := strValue;
  m_bFENChanged := FALSE;

  // TODO: parse FEN and fill other fields
end;


procedure TPositionEditingForm.EmptyButtonClick(Sender: TObject);
begin
  FSetFEN(EMPTY_CHESS_POSITION);
  FSetPosition;
end;


procedure TPositionEditingForm.InitialButtonClick(Sender: TObject);
begin
  FSetFEN(INITIAL_CHESS_POSITION);
  FSetPosition;
end;


procedure TPositionEditingForm.MoveNEditChange(Sender: TObject);
begin
  MoveNEdit.Text := IntToStr(MoveNUpDown.Position);
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
