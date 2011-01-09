unit MoveListFormUnit;

interface

uses
  Forms, Classes, Windows, Controls, Grids, Buttons;

type
  TStringGrid = class(Grids.TStringGrid)
  protected
    function CreateEditor: TInplaceEdit; override;
    function GetEditStyle(ACol, ARow: Longint): TEditStyle; override;
    function CanEditModify: Boolean; override;
    function CanEditShow: Boolean; override;
  public
    destructor Destroy; override;
  end;

  IPlysProvider = interface
    function GetPlysCount: integer;
    function GetPly(iIndex: integer): string;
    function GetInvalidationID: LongWord;
    function GetCurrentPlyIndex: integer;
    procedure SetCurrentPlyIndex(iValue: integer);
//    procedure ForwardPly;
//    procedure BackwardsPly;
    property PlysCount: integer read GetPlysCount;
    property Plys[iIndex: integer]: string read GetPly;
    property CurrentPlyIndex: integer read GetCurrentPlyIndex
                                      write SetCurrentPlyIndex;
    property InvalidationID: LongWord read GetInvalidationID;
// TODO:    property WhiteStarts: boolean;
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
  with Items do
  begin
    Clear;
    Add('one');
    Add('two');
    Add('three');
  end;
end;


procedure TMoveListForm.FormDeactivate(Sender: TObject);
begin
  MovesStringGrid.HideEditor;
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
  MovesStringGrid.RowCount := Max(2, FGetMovesCount + 1);

  if (not Assigned(m_PlysProvider)) then
    exit;

  if (m_PlysProvider.CurrentPlyIndex > 0) then
  begin
    m_iPlyIndexRow := ((m_PlysProvider.CurrentPlyIndex - 1) div 2) + 1;
    m_iPlyIndexCol := ((m_PlysProvider.CurrentPlyIndex - 1) mod 2) + 1;
  end
  else
  begin
    m_iPlyIndexRow := 1;
    m_iPlyIndexCol := 0;
  end;

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

  str := MovesStringGrid.Cells[ACol, ARow];

  if (str = '') then
    exit;   

  if (ACol = 0) then
    NDrawCell(str, taRightJustify, -2)
  else
  begin
    if ((m_iPlyIndexCol = ACol) and (m_iPlyIndexRow = ARow)) then
    begin
      MovesStringGrid.Canvas.Brush.Color := clHighlight;
      MovesStringGrid.Canvas.Font.Color := clHighlightText;
      MovesStringGrid.Canvas.FillRect(Rect);
    end;
    NDrawCell(str, taCenter);
  end;
end;


procedure TMoveListForm.MovesStringGridSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: String);
begin
  // TODO:
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
  Result := ((Col > 0) and (Cells[Col, Row] <> ''));
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

  m_PlysProvider.CurrentPlyIndex := 2 * (ARow - 1) + (ACol - 1) + 1;
end;

////////////////////////////////////////////////////////////////////////////////
// TInplaceEditList

procedure TInplaceEditList.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := Params.Style or ES_CENTER;
end;

end.
