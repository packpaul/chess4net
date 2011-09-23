////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit MoveListFormUnit;

interface

uses
  Classes, Windows, Controls, Grids, Buttons,
  //
  PlysProviderIntfUnit, FloatingFormsUnit;

type
  TStringGrid = class(Grids.TStringGrid)
  private
    m_HiddenInplaceEditData: TStrings;
    m_bPlyLineSelection: boolean;
    class procedure FPlyIndexToGridPos(iPlyIndex: integer; bWhiteStarts: boolean;
      out iCol, iRow: integer);
    class function FGridPosToPlyIndex(iCol, iRow: integer;
      bWhiteStarts: boolean): integer;
    function FGetPlysProvider: IPlysProvider;
    procedure FSetPlyLineSelection(bValue: boolean);
    procedure FOnGetPickListItems(ACol, ARow: Integer; Items: TStrings);
    function FGetTrainingMode: boolean;
    property PlysProvider: IPlysProvider read FGetPlysProvider;
    property PlyLineSelection: boolean read m_bPlyLineSelection write FSetPlyLineSelection;
    property TrainingMode: boolean read FGetTrainingMode;
  protected
    function CreateEditor: TInplaceEdit; override;
    function GetEditStyle(ACol, ARow: Longint): TEditStyle; override;
    function CanEditModify: Boolean; override;
    function CanEditShow: Boolean; override;
    procedure SetEditText(ACol, ARow: Longint; const Value: string); override;
    function GetEditText(ACol, ARow: Longint): string; override;
  public
    destructor Destroy; override;
  end;

  
  TMoveListForm = class(TChildFloatingForm)
    MovesStringGrid: TStringGrid;
    BackSpeedButton: TSpeedButton;
    ForthSpeedButton: TSpeedButton;
    SelectLineSpeedButton: TSpeedButton;
    InitialSpeedButton: TSpeedButton;
    ReturnFromLineSpeedButton: TSpeedButton;
    DeleteLineSpeedButton: TSpeedButton;
    EndSpeedButton: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MovesStringGridDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure MovesStringGridSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: String);
    procedure MovesStringGridSelectCell(Sender: TObject; ACol,
      ARow: Integer; var CanSelect: Boolean);
    procedure MovesStringGridMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    m_PlysProvider: IPlysProvider;
    m_iPlyIndexRow, m_iPlyIndexCol: integer;
    m_lwInvalidationID: LongWord;
    m_bCellFirstSelected: boolean;
    m_bRefreshing: boolean;
    m_bTrainingMode: boolean;

    procedure FSetPlysProvider(APlysProvider: IPlysProvider);
    function FGetMovesCount: integer;
    procedure FRefresh;
    procedure FSetTrainingMode(bValue: boolean);
  public
    procedure SelectLine;
    procedure Refresh;
    property TrainingMode: boolean read m_bTrainingMode write FSetTrainingMode;
    property PlysProvider: IPlysProvider read m_PlysProvider write FSetPlysProvider;
  end;

implementation

{$R *.dfm}

uses
  Math, SysUtils, Graphics,
  //
  AnalyseChessBoardUnit, PlysTreeUnit;

type
  TInplaceEditList = class(Grids.TInplaceEditList)
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DoDropDownKeys(var Key: Word; Shift: TShiftState); override;
    procedure CloseUp(Accept: Boolean); override;
  end;

const
  SEVERAL_PLYS_COLOR = TColor($D0E0F0);
  UNKNOWN_PLY = '?';

////////////////////////////////////////////////////////////////////////////////
// TMoveListForm

procedure TMoveListForm.FormCreate(Sender: TObject);
begin
  with MovesStringGrid do
  begin
    Cells[0, 0] := '#';
    Cells[1, 0] := 'White';
    Cells[2, 0] := 'Black';
  end;
end;


procedure TMoveListForm.FormDeactivate(Sender: TObject);
begin
  MovesStringGrid.PlyLineSelection := FALSE;
end;


procedure TMoveListForm.FormDestroy(Sender: TObject);
begin
  m_PlysProvider := nil;
end;


procedure TMoveListForm.FSetPlysProvider(APlysProvider: IPlysProvider);
begin
  if (not Assigned(self)) then
    exit;
  m_PlysProvider := APlysProvider;
  Refresh;
end;


procedure TMoveListForm.Refresh;
begin
  m_bRefreshing := TRUE;
  try
    FRefresh;
  finally
    m_bRefreshing := FALSE;
  end;
end;


procedure TMoveListForm.FRefresh;
var
  i: integer;
  iCol, iRow: integer;
  iMoveNumber: integer;
begin
  MovesStringGrid.PlyLineSelection := FALSE;
  MovesStringGrid.RowCount := Max(2, FGetMovesCount + 1);

  if (not Assigned(m_PlysProvider)) then
    exit;

  TStringGrid.FPlyIndexToGridPos(
    m_PlysProvider.CurrentPlyIndex, m_PlysProvider.WhiteStarts, 
    m_iPlyIndexCol, m_iPlyIndexRow);

  MovesStringGrid.FocusCell(m_iPlyIndexCol, m_iPlyIndexRow, TRUE);

  if (m_lwInvalidationID = m_PlysProvider.InvalidationID) then
    exit
  else
    m_lwInvalidationID := m_PlysProvider.InvalidationID;

  for i := 0 to 2 do
    MovesStringGrid.Cells[i, 1] := '';

  if (m_PlysProvider.PlysCount = 0) then
    exit;

  for i := 1 to m_PlysProvider.PlysCount do
  begin
    TStringGrid.FPlyIndexToGridPos(i, m_PlysProvider.WhiteStarts, iCol, iRow);

    iMoveNumber := TPlysTree.ConvertPlyToMove(
      m_PlysProvider.PlysOffset + i, m_PlysProvider.WhiteStarts);
    MovesStringGrid.Cells[0, iRow] := Format('%d.', [iMoveNumber]);

    MovesStringGrid.Cells[iCol, iRow] := m_PlysProvider.Plys[i];
  end;

  if (iCol < 2) then
    MovesStringGrid.Cells[2, iRow] := '';
end;


function TMoveListForm.FGetMovesCount: integer;
begin
  if (Assigned(m_PlysProvider)) then
    Result := TPlysTree.ConvertPlyToMove(m_PlysProvider.PlysCount,
      m_PlysProvider.WhiteStarts)
  else
    Result := 0;
end;


procedure TMoveListForm.MovesStringGridDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);

  function NGetData: string;
  begin
    Result := MovesStringGrid.Cells[ACol, ARow];

    if (m_bTrainingMode and (ACol > 0) and (Result <> '')) then
      Result := UNKNOWN_PLY;

    if (not Assigned(m_PlysProvider)) then
      exit;

    if (m_PlysProvider.HasSeveralPlysForPlyIndex(TStringGrid.FGridPosToPlyIndex(
        ACol, ARow, m_PlysProvider.WhiteStarts))) then
      Result := Result + '...';
  end;

  procedure NStylishCell;
  var
    iPly: integer;
    PlyStatuses: TPlyStatuses;
  begin
    iPly := TStringGrid.FGridPosToPlyIndex(ACol, ARow, m_PlysProvider.WhiteStarts);

    if ((m_iPlyIndexCol = ACol) and (m_iPlyIndexRow = ARow)) then
    begin
      MovesStringGrid.Canvas.Brush.Color := clHighlight;
      MovesStringGrid.Canvas.Font.Color := clHighlightText;
    end
    else if (m_PlysProvider.HasSeveralPlysForPlyIndex(iPly)) then
      MovesStringGrid.Canvas.Brush.Color := SEVERAL_PLYS_COLOR;

    MovesStringGrid.Canvas.FillRect(Rect);    

    PlyStatuses := m_PlysProvider.GetPlyStatus(iPly);
    if (psMainLine in PlyStatuses) then
    begin
      MovesStringGrid.Canvas.Font.Style := [fsBold];
    end
    else if (psUserLine in PlyStatuses) then
    begin
//      MovesStringGrid.Canvas.Font.Style := [fsItalic];
    end;
  end;

  procedure NDrawCell(const strCell: string; Alignment: TAlignment;
    iIndentX: integer = 0);
  var
    ASize: TSize;
    iX: integer;
  begin
    with MovesStringGrid do
    begin
      ASize := Canvas.TextExtent(strCell);
      case Alignment of
        taLeftJustify:
          iX := Rect.Left + 2;
        taRightJustify:
          iX := Rect.Right - ASize.cx;
      else //  taCenter:
          iX := (Rect.Left + Rect.Right - ASize.cx) div 2
      end;

      inc(iX, iIndentX);

      Canvas.TextRect(Rect, iX, Rect.Top + 2, strCell);
    end;
  end;

var
  str: string;
begin // .MovesStringGridDrawCell
  if (gdFixed in State) then
    exit;

  str := NGetData;
  if (str = '') then
    exit;   

  if (ACol = 0) then
    NDrawCell(str, taRightJustify, -2)
  else
  begin
    NStylishCell;
    NDrawCell(str, taCenter);
  end;
end;


procedure TMoveListForm.MovesStringGridSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: String);
var
  iPly: integer;
  strOldPly: string;
begin
  MovesStringGrid.PlyLineSelection := FALSE;

  if (not Assigned(m_PlysProvider)) then
    exit;

  iPly := TStringGrid.FGridPosToPlyIndex(ACol, ARow, m_PlysProvider.WhiteStarts);

  strOldPly := m_PlysProvider.Plys[iPly];
  if (Value = strOldPly) then
    exit;

  if (not m_PlysProvider.SetPlyForPlyIndex(iPly, Value)) then
    MovesStringGrid.Cells[ACol, ARow] := strOldPly;
end;


procedure TMoveListForm.MovesStringGridSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  if ((m_iPlyIndexRow = ARow) and (m_iPlyIndexCol = ACol)) then
  begin
    CanSelect := TRUE;
    exit;
  end;

  CanSelect := FALSE;

  if (not ((ACol > 0) and (ARow > 0))) then
    exit;

  if (MovesStringGrid.Cells[ACol, ARow] = '') then
    exit;

  if (Assigned(m_PlysProvider) and (not m_bRefreshing)) then
  begin
    m_PlysProvider.CurrentPlyIndex := TStringGrid.FGridPosToPlyIndex(ACol, ARow,
      m_PlysProvider.WhiteStarts);
  end;

  m_bCellFirstSelected := TRUE;
end;


procedure TMoveListForm.MovesStringGridMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  iRow, iCol: integer;
begin
  if (m_bCellFirstSelected) then
  begin
    m_bCellFirstSelected := FALSE;
    exit;
  end;

  MovesStringGrid.MouseToCell(X, Y, iCol, iRow);
  if ((iCol <> m_iPlyIndexCol) or (iRow <> m_iPlyIndexRow)) then
    exit;

  if (not Assigned(m_PlysProvider)) then
    exit;

  if (m_PlysProvider.HasSeveralPlysForPlyIndex(m_PlysProvider.CurrentPlyIndex)) then
    SelectLine;
end;


procedure TMoveListForm.SelectLine;
begin
  if (not Showing) then
    exit;

  if (not MovesStringGrid.PlyLineSelection) then
  begin
    MovesStringGrid.PlyLineSelection := TRUE;
  end
  else
  begin
    if (Assigned(MovesStringGrid.InplaceEditor)) then
    begin
      (MovesStringGrid.InplaceEditor as TInplaceEditList).CloseUp(TRUE);
      MovesStringGrid.PlyLineSelection := FALSE;
    end;
  end;
end;


procedure TMoveListForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:
    begin
      MovesStringGrid.PlyLineSelection := FALSE;
      Key := 0;
    end;
  end;
end;


procedure TMoveListForm.FSetTrainingMode(bValue: boolean);
begin
  if ((not Assigned(self)) or (m_bTrainingMode = bValue)) then
    exit;

  m_bTrainingMode := bValue;

  MovesStringGrid.PlyLineSelection := FALSE;

  MovesStringGrid.Invalidate;
end;

////////////////////////////////////////////////////////////////////////////////
// TStringGrid

destructor TStringGrid.Destroy;
begin
  m_HiddenInplaceEditData.Free;

  if (Assigned(InplaceEditor)) then
  begin
    (InplaceEditor as TInplaceEditList).OnGetPickListitems := nil;
  end;

  inherited;
end;

function TStringGrid.CreateEditor: TInplaceEdit;
begin
  Result := TInplaceEditList.Create(self);
  with (Result as TInplaceEditList) do
  begin
    OnGetPickListitems := FOnGetPickListItems;
    ReadOnly := TRUE;
    AutoSelect := FALSE;
  end;
end;


function TStringGrid.GetEditStyle(ACol, ARow: Longint): TEditStyle;
begin
  Result := esPickList;
end;


function TStringGrid.CanEditModify: Boolean;
begin
  Result := FALSE;
end;


function TStringGrid.CanEditShow: Boolean;
begin
  Result := m_bPlyLineSelection;
  if (not Result) then
    exit;

  Result := ((Col > 0) and (Cells[Col, Row] <> ''));
  if (not Result) then
    exit;

  Result := (PlysProvider.HasSeveralPlysForPlyIndex(FGridPosToPlyIndex(Col, Row,
    PlysProvider.WhiteStarts)));
end;


class procedure TStringGrid.FPlyIndexToGridPos(
  iPlyIndex: integer; bWhiteStarts: boolean; out iCol, iRow: integer);
begin
  if (iPlyIndex > 0) then
  begin
    if (not bWhiteStarts) then
      inc(iPlyIndex);
    iRow := ((iPlyIndex - 1) div 2) + 1;
    iCol := ((iPlyIndex - 1) mod 2) + 1;
  end
  else
  begin
    iRow := 1;
    iCol := 0;
  end;
end;


class function TStringGrid.FGridPosToPlyIndex(iCol, iRow: integer;
  bWhiteStarts: boolean): integer;
begin
  if (iCol = 0) then
    Result := 0
  else
    Result := 2 * (iRow - 1) + iCol - IfThen(bWhiteStarts, 0, 1);
end;


function TStringGrid.FGetPlysProvider: IPlysProvider;
begin
  Result := (Owner as TMoveListForm).PlysProvider;
end;


function TStringGrid.FGetTrainingMode: boolean;
begin
  Result := (Owner as TMoveListForm).TrainingMode;
end;


procedure TStringGrid.FSetPlyLineSelection(bValue: boolean);
begin
  if (m_bPlyLineSelection = bValue) then
    exit;

  m_bPlyLineSelection := bValue;

  if (bValue) then
  begin
    ShowEditor;
    if (Assigned(InplaceEditor)) then
    begin
      with InplaceEditor as TInplaceEditList do
      begin
        if (not (self.Owner as TMoveListForm).Active) then
          DropDown;
        DropDown;
      end;
    end;
  end
  else
    HideEditor;

  m_bPlyLineSelection := EditorMode;
end;


procedure TStringGrid.SetEditText(ACol, ARow: Longint; const Value: string);
var
  iInplaceEditItemIndex: integer;
  strHiddenValue: string;
begin
  if (Assigned(InplaceEditor)) then
    iInplaceEditItemIndex := (InplaceEditor as TInplaceEditList).PickList.ItemIndex
  else
    iInplaceEditItemIndex := -1;

  if (iInplaceEditItemIndex >= 0) then
    strHiddenValue := m_HiddenInplaceEditData[iInplaceEditItemIndex]
  else
    strHiddenValue := Value;

  inherited SetEditText(ACol, ARow, strHiddenValue);
end;


function TStringGrid.GetEditText(ACol, ARow: Longint): string;
begin
  Result := inherited GetEditText(ACol, ARow);
{
  if ((ACol > 0) and TrainingMode) then
    Result := UNKNOWN_PLY
  else
    Result := inherited GetEditText(ACol, ARow);
}
end;


procedure TStringGrid.FOnGetPickListItems(ACol, ARow: Integer; Items: TStrings);
var
  i: integer;
begin
  if (not Assigned(m_HiddenInplaceEditData)) then
    m_HiddenInplaceEditData := TStringList.Create;

  if (Assigned(PlysProvider)) then
  begin
    PlysProvider.GetPlysForPlyIndex(TStringGrid.FGridPosToPlyIndex(ACol, ARow,
      PlysProvider.WhiteStarts), m_HiddenInplaceEditData)
  end
  else
    m_HiddenInplaceEditData.Clear;

  Items.Assign(m_HiddenInplaceEditData);
{
  if (TrainingMode) then
  begin
    for i := 0 to Items.Count - 1 do
      Items[i] := UNKNOWN_PLY;
  end;
}  
end;

////////////////////////////////////////////////////////////////////////////////
// TInplaceEditList

procedure TInplaceEditList.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := Params.Style or ES_CENTER;
end;


procedure TInplaceEditList.DoDropDownKeys(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:
      ;
  else
    inherited;
  end;
end;


procedure TInplaceEditList.CloseUp(Accept: Boolean);
begin
  if (Accept) then
    Text := '';

  inherited;
end;

end.
