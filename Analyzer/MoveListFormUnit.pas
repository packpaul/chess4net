unit MoveListFormUnit;

interface

uses
  Forms, Classes, Windows, Controls, Grids, Buttons;

type
  IPlysProvider = interface
    function GetPlysCount: integer;
    function GetPly(iIndex: integer): string;

    function GetCurrentPlyIndex: integer;
    procedure SetCurrentPlyIndex(iValue: integer);

    function GetPlysCountForPlyIndex(iPlyIndex: integer): integer;
    procedure GetPlysForPlyIndex(iPlyIndex: integer; var List: TStrings);
    function SetPlyForPlyIndex(iPlyIndex: integer; const strPly: string): boolean;

    function GetInvalidationID: LongWord;

    property PlysCount: integer read GetPlysCount;
    property Plys[iIndex: integer]: string read GetPly;
    property CurrentPlyIndex: integer read GetCurrentPlyIndex
                                      write SetCurrentPlyIndex;
    property InvalidationID: LongWord read GetInvalidationID;
// TODO:    property WhiteStarts: boolean;
  end;


  TStringGrid = class(Grids.TStringGrid)
  private
    m_bPlyLineSelection: boolean;
    class procedure FPlyIndexToGridPos(iPlyIndex: integer; out iCol, iRow: integer);
    class function FGridPosToPlyIndex(iCol, iRow: integer): integer;
    function FGetPlysProvider: IPlysProvider;
    procedure FSetPlyLineSelection(bValue: boolean);
    property PlysProvider: IPlysProvider read FGetPlysProvider;
    property PlyLineSelection: boolean read m_bPlyLineSelection write FSetPlyLineSelection;
  protected
    function CreateEditor: TInplaceEdit; override;
    function GetEditStyle(ACol, ARow: Longint): TEditStyle; override;
    function CanEditModify: Boolean; override;
    function CanEditShow: Boolean; override;
  public
    destructor Destroy; override;
  end;

  
  TMoveListForm = class(TForm)
    MovesStringGrid: TStringGrid;
    BackSpeedButton: TSpeedButton;
    ForthSpeedButton: TSpeedButton;
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
  private
    m_PlysProvider: IPlysProvider;
    m_iPlyIndexRow, m_iPlyIndexCol: integer;
    m_lwInvalidationID: LongWord;

    procedure FOnGetPickListItems(ACol, ARow: Integer; Items: TStrings);
    procedure FSetPlysProvider(APlysProvider: IPlysProvider);
    function FGetMovesCount: integer;
  public
    procedure Refresh;
    property PlysProvider: IPlysProvider read m_PlysProvider write FSetPlysProvider;
  end;

implementation

{$R *.dfm}

uses
  Math, SysUtils, Graphics,
  //
  AnalyseChessBoardUnit;

type
  TInplaceEditList = class(Grids.TInplaceEditList)
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  end;

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


procedure TMoveListForm.FOnGetPickListItems(ACol, ARow: Integer; Items: TStrings);
begin
  if (Assigned(m_PlysProvider)) then
    m_PlysProvider.GetPlysForPlyIndex(TStringGrid.FGridPosToPlyIndex(ACol, ARow), Items)
  else
    Items.Clear;
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
  m_PlysProvider := APlysProvider;
  Refresh;
end;


procedure TMoveListForm.Refresh;
var
  i: integer;
  iCol, iRow: integer;
begin
  MovesStringGrid.PlyLineSelection := FALSE;
  MovesStringGrid.RowCount := Max(2, FGetMovesCount + 1);

  if (not Assigned(m_PlysProvider)) then
    exit;

  TStringGrid.FPlyIndexToGridPos(m_PlysProvider.CurrentPlyIndex,
    m_iPlyIndexCol, m_iPlyIndexRow);

  MovesStringGrid.FocusCell(m_iPlyIndexCol, m_iPlyIndexRow, TRUE);

  if (m_lwInvalidationID = m_PlysProvider.InvalidationID) then
    exit
  else
    m_lwInvalidationID := m_PlysProvider.InvalidationID;

  for i := 0 to 2 do
    MovesStringGrid.Cells[i, 1] := '';

  iRow := 1;
  iCol := 1;

  for i := 1 to m_PlysProvider.PlysCount do
  begin
    MovesStringGrid.Cells[0, iRow] := Format('%d.', [(i + 1) div 2]);
    MovesStringGrid.Cells[iCol, iRow] := m_PlysProvider.Plys[i];

    if (iCol = 2) then
    begin
      iCol := 1;
      inc(iRow);
    end
    else
      inc(iCol);
  end;

  if (iRow < MovesStringGrid.RowCount) then
    MovesStringGrid.Cells[iCol, iRow] := '';
end;


function TMoveListForm.FGetMovesCount: integer;
begin
  if (Assigned(m_PlysProvider)) then
    Result := (m_PlysProvider.PlysCount + 1) div 2
  else
    Result := 0;
end;


procedure TMoveListForm.MovesStringGridDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);

  function NGetData: string;
  var
    iPly: integer;
  begin
    Result := MovesStringGrid.Cells[ACol, ARow];

    if (not Assigned(m_PlysProvider)) then
      exit;

    iPly := TStringGrid.FGridPosToPlyIndex(ACol, ARow);
    if (m_PlysProvider.GetPlysCountForPlyIndex(iPly) > 1) then
      Result := Result + '...';
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

  procedure NColorCell;
  begin
    if ((m_iPlyIndexCol = ACol) and (m_iPlyIndexRow = ARow)) then
    begin
      MovesStringGrid.Canvas.Brush.Color := clHighlight;
      MovesStringGrid.Canvas.Font.Color := clHighlightText;
      MovesStringGrid.Canvas.FillRect(Rect);
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
    NColorCell;
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

  iPly := TStringGrid.FGridPosToPlyIndex(ACol, ARow);

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

  if (Assigned(m_PlysProvider)) then
    m_PlysProvider.CurrentPlyIndex := TStringGrid.FGridPosToPlyIndex(ACol, ARow);
end;


procedure TMoveListForm.MovesStringGridMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  iRow, iCol: integer;
begin
  MovesStringGrid.MouseToCell(X, Y, iCol, iRow);
  if ((iCol <> m_iPlyIndexCol) or (iRow <> m_iPlyIndexRow)) then
    exit;

  if (not Assigned(m_PlysProvider)) then
    exit;

  if (m_PlysProvider.GetPlysCountForPlyIndex(m_PlysProvider.CurrentPlyIndex) > 1) then
    MovesStringGrid.PlyLineSelection := TRUE;
end;

////////////////////////////////////////////////////////////////////////////////
// TStringGrid

destructor TStringGrid.Destroy;
begin
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
    OnGetPickListitems := (self.Owner as TMoveListForm).FOnGetPickListItems;
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

  Result := (PlysProvider.GetPlysCountForPlyIndex(FGridPosToPlyIndex(Col, Row)) > 1);
end;


class procedure TStringGrid.FPlyIndexToGridPos(iPlyIndex: integer; out iCol, iRow: integer);
begin
  if (iPlyIndex > 0) then
  begin
    iRow := ((iPlyIndex - 1) div 2) + 1;
    iCol := ((iPlyIndex - 1) mod 2) + 1;
  end
  else
  begin
    iRow := 1;
    iCol := 0;
  end;
end;


class function TStringGrid.FGridPosToPlyIndex(iCol, iRow: integer): integer;
begin
  if (iCol = 0) then
    Result := 0
  else
    Result := 2 * (iRow - 1) + iCol;
end;


function TStringGrid.FGetPlysProvider: IPlysProvider;
begin
  Result := (self.Owner as TMoveListForm).PlysProvider;
end;


procedure TStringGrid.FSetPlyLineSelection(bValue: boolean);
begin
  if (m_bPlyLineSelection = bValue) then
    exit;

  m_bPlyLineSelection := bValue;

  if (bValue) then
    ShowEditor
  else
    HideEditor;

  m_bPlyLineSelection := EditorMode;
end;

////////////////////////////////////////////////////////////////////////////////
// TInplaceEditList

procedure TInplaceEditList.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := Params.Style or ES_CENTER;
end;

end.
