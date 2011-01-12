unit PlysTreeUnit;

interface

uses
  Classes;

type
  TPlysTree = class
  private
    m_strItems: TStringList;
    function FGetPosition(iIndex: integer): string;
    function FGetItem(iIndex: integer): string;
    function FGetCount: integer;
  public
    constructor Create;
    destructor Destroy; override;

    function Add(const strPos: string; strMove: string = ''): integer;
    procedure Clear;
    procedure Delete(iIndex: Integer);

    property Items[iIndex: integer]: string read FGetItem; default;
    property Position[iIndex: integer]: string read FGetPosition;
    
    property Count: integer read FGetCount;
  end;

implementation

type
  TPlysTreeNode = class
  private
    m_strPos: string;
  public
    constructor Create(const strPos: string);
    property Pos: string read m_strPos;
  end;

////////////////////////////////////////////////////////////////////////////////
// TPlysTree

constructor TPlysTree.Create;
begin
  m_strItems := TStringList.Create;
end;

destructor TPlysTree.Destroy;
begin
  Clear;
  m_strItems.Free;
  inherited;
end;


procedure TPlysTree.Clear;
begin
  Delete(0);
end;


procedure TPlysTree.Delete(iIndex: Integer);
var
  i: integer;
  Obj: TObject;
begin
  for i := m_strItems.Count - 1 downto iIndex do
  begin
    Obj := m_strItems.Objects[i];
    m_strItems.Objects[iIndex] := nil;
    Obj.Free;

    m_strItems.Delete(i);
  end;
end;


function TPlysTree.Add(const strPos: string; strMove: string = ''): integer;
begin
  Result := m_strItems.AddObject(strMove, TPlysTreeNode.Create(strPos));
end;


function TPlysTree.FGetPosition(iIndex: integer): string;
begin
  Result := (m_strItems.Objects[iIndex] as TPlysTreeNode).Pos;
end;


function TPlysTree.FGetItem(iIndex: integer): string;
begin
  Result := m_strItems[iIndex];
end;


function TPlysTree.FGetCount: integer;
begin
  Result := m_strItems.Count;
end;

////////////////////////////////////////////////////////////////////////////////
// TPlysTreeNode

constructor TPlysTreeNode.Create(const strPos: string);
begin
  inherited Create;
  m_strPos := strPos;  
end;

end.
