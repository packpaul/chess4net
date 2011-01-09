unit PlysTreeUnit;

interface

uses
  Classes;

type
  TPlysTree = class(TStringList)
  private
    function FGetPosition(iIndex: integer): string;
  public
    destructor Destroy; override;

    function Add(const strMove, strPos: string): integer; reintroduce;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;

    property Position[iIndex: integer]: string read FGetPosition;
  end;

implementation

type
  TPosString = class
  private
    m_strPos: string;
  public
    constructor Create(const strPos: string);
    property Pos: string read m_strPos;
  end;

////////////////////////////////////////////////////////////////////////////////
// TPlysTree

destructor TPlysTree.Destroy;
begin
  Clear;
  inherited;
end;


procedure TPlysTree.Clear;
var
  i: integer;
  Obj: TObject;
begin
  for i := 0 to Count - 1 do
  begin
    Obj := Objects[i];
    Objects[i] := nil;
    Obj.Free;
  end;

  inherited Clear;
end;


procedure TPlysTree.Delete(Index: Integer);
var
  Obj: TObject;
begin
  Obj := Objects[Index];
  Objects[Index] := nil;
  Obj.Free;

  inherited;
end;


function TPlysTree.Add(const strMove, strPos: string): integer;
begin
  Result := inherited AddObject(strMove, TPosString.Create(strPos));
end;


function TPlysTree.FGetPosition(iIndex: integer): string;
begin
  Result := (Objects[iIndex] as TPosString).Pos; 
end;

////////////////////////////////////////////////////////////////////////////////
// TPosString

constructor TPosString.Create(const strPos: string);
begin
  m_strPos := strPos;
  inherited Create;
end;

end.
