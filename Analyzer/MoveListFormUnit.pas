unit MoveListFormUnit;

interface

uses
  Forms, Classes, Controls, Grids, Buttons;

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
//    procedure ForwardPly;
//    procedure BackwardsPly;
    property PlysCount: integer read GetPlysCount;
    property Plys[iIndex: integer]: string read GetPly;
  end;


  TMoveListForm = class(TForm)
    MovesStringGrid: TStringGrid;
    BackSpeedButton: TSpeedButton;
    ForthSpeedButton: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    m_PlysProvider: IPlysProvider;
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
  Windows, Math, SysUtils,
  //
  AnalyseChessBoardUnit;

type
  TInplaceEditList = class(Grids.TInplaceEditList)
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

  for i := 0 to 2 do
    MovesStringGrid.Cells[i, 1] := '';

  if (not Assigned(m_PlysProvider)) then
    exit;

  iRow := 1;
  iCol := 1;

  for i := 0 to m_PlysProvider.PlysCount - 1 do
  begin
    MovesStringGrid.Cells[0, iRow] := Format('%d.', [i div 2 + 1]);
    MovesStringGrid.Cells[iCol, iRow] := m_PlysProvider.Plys[i];

    if (iCol = 2) then
    begin
      iCol := 1;
      inc(iRow);
    end
    else
      inc(iCol);
  end;
end;


function TMoveListForm.FGetMovesCount: integer;
begin
  if (Assigned(m_PlysProvider)) then
    Result := (m_PlysProvider.PlysCount + 1) div 2
  else
    Result := 0;
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

end.
