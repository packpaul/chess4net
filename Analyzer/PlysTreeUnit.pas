unit PlysTreeUnit;

interface

uses
  Classes;

type
  TPlysTreeNode = class
  private
    m_strPly: string;
    m_strPos: string;

    m_arrNextNodes: array of TPlysTreeNode;
    m_iNextNodeOfLineIndex: integer;

    constructor Create(const strPos, strPly: string);

    function FGetNextNodeOfLine: TPlysTreeNode;
    procedure FDeleteNextNodeOfLine;
    procedure FDeleteNextNodes;
    procedure FAddLineNode(Node: TPlysTreeNode);

    function FEquals(Node: TPlysTreeNode): boolean;

    property Ply: string read m_strPly;
    property Pos: string read m_strPos;

  public
    destructor Destroy; override;
  end;

  TPlysTree = class
  private
    m_FirstNode: TPlysTreeNode;
    function FGetPosition(iIndex: integer): string;
    function FGetPly(iIndex: integer): string;
    function FGetCount: integer;
    function FGetNodeOfDepth(iPlyDepth: integer): TPlysTreeNode;
  public
    destructor Destroy; override;

    function Add(iPlyIndex: integer; const strPos: string; const strMove: string = ''): integer; overload;
    function Add(const strPos: string; strMove: string = ''): integer; overload;

    procedure Clear;
    procedure Delete(iIndex: Integer);

    property Plys[iIndex: integer]: string read FGetPly; default;
    property Position[iIndex: integer]: string read FGetPosition;

    property Count: integer read FGetCount;
  end;

implementation

uses
  SysUtils;

////////////////////////////////////////////////////////////////////////////////
// TPlysTree

destructor TPlysTree.Destroy;
begin
  Clear;
  inherited;
end;


procedure TPlysTree.Clear;
begin
  Delete(0);
end;


procedure TPlysTree.Delete(iIndex: Integer);
var
  Node: TPlysTreeNode;
begin
  Assert(iIndex >= 0);

  if (iIndex = 0) then
  begin
    FreeAndNil(m_FirstNode);
    exit;
  end;

  Node := FGetNodeOfDepth(iIndex - 1);
  if (Assigned(Node)) then
    Node.FDeleteNextNodeOfLine;
end;


function TPlysTree.FGetNodeOfDepth(iPlyDepth: integer): TPlysTreeNode;
var
  i: integer;
begin
  Result := m_FirstNode;
  for i := 0 to iPlyDepth - 1 do
  begin
    if (not Assigned(Result)) then
      exit;
    Result := Result.FGetNextNodeOfLine;
  end;
end;


function TPlysTree.Add(iPlyIndex: integer; const strPos: string; const strMove: string = ''): integer;
var
  Node, NextNode: TPlysTreeNode;
begin
  Assert(iPlyIndex >= 0);

  Result := 0;

  if (iPlyIndex = 0) then
  begin
    Clear;
    m_FirstNode := TPlysTreeNode.Create(strPos, strMove);
    exit;
  end;

  NextNode := m_FirstNode;
  repeat
    inc(Result);
    Node := NextNode;
    NextNode := Node.FGetNextNodeOfLine;
  until ((Result >= iPlyIndex) or (not Assigned(NextNode)));

  Node.FAddLineNode(TPlysTreeNode.Create(strPos, strMove));
end;


function TPlysTree.Add(const strPos: string; strMove: string = ''): integer;
begin
  Result := Add(Count, strPos, strMove);
end;


function TPlysTree.FGetPosition(iIndex: integer): string;
var
  Node: TPlysTreeNode;
begin
  Node := FGetNodeOfDepth(iIndex);
  if (Assigned(Node)) then
    Result := Node.Pos
  else
    Result := '';
end;


function TPlysTree.FGetPly(iIndex: integer): string;
var
  Node: TPlysTreeNode;
begin
  Node := FGetNodeOfDepth(iIndex);
  if (Assigned(Node)) then
    Result := Node.Ply
  else
    Result := '';
end;


function TPlysTree.FGetCount: integer;
var
  NextNode: TPlysTreeNode;
begin
  Result := 0;

  NextNode := m_FirstNode;
  while (Assigned(NextNode)) do
  begin
    inc(Result);
    NextNode := NextNode.FGetNextNodeOfLine;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPlysTreeNode

constructor TPlysTreeNode.Create(const strPos, strPly: string);
begin
  inherited Create;

  m_strPos := strPos;
  m_strPly := strPly;

  m_iNextNodeOfLineIndex := -1;
end;


destructor TPlysTreeNode.Destroy;
begin
  FDeleteNextNodes;
  inherited;
end;


procedure TPlysTreeNode.FDeleteNextNodes;
var
  i: integer;
begin
  for i := Low(m_arrNextNodes) to High(m_arrNextNodes) do
    FreeAndNil(m_arrNextNodes[i]);
  SetLength(m_arrNextNodes, 0);
  m_iNextNodeOfLineIndex := -1;
end;


function TPlysTreeNode.FGetNextNodeOfLine: TPlysTreeNode;
begin
  if (m_iNextNodeOfLineIndex >= Low(m_arrNextNodes)) then
    Result := m_arrNextNodes[m_iNextNodeOfLineIndex]
  else
    Result := nil;
end;


procedure TPlysTreeNode.FDeleteNextNodeOfLine;
var
  i: integer;
begin
  if (m_iNextNodeOfLineIndex < 0) then
    exit;

  FreeAndNil(m_arrNextNodes[m_iNextNodeOfLineIndex]);

  for i := Low(m_arrNextNodes) to High(m_arrNextNodes) do
  begin
    if (Assigned(m_arrNextNodes[i])) then
    begin
      m_iNextNodeOfLineIndex := i;
      exit;
    end;
  end;

  m_iNextNodeOfLineIndex := -1;
end;


procedure TPlysTreeNode.FAddLineNode(Node: TPlysTreeNode);
var
  i: integer;
begin
  for i := Low(m_arrNextNodes) to High(m_arrNextNodes) do
  begin
    if (m_arrNextNodes[i].FEquals(Node)) then
    begin
      m_iNextNodeOfLineIndex := i;
      Node.Free;
      exit;
    end;
  end;

  for i := Low(m_arrNextNodes) to High(m_arrNextNodes) do
  begin
    if (not Assigned(m_arrNextNodes[i])) then
    begin
      m_arrNextNodes[i] := Node;
      m_iNextNodeOfLineIndex := i;
      exit;
    end;
  end;

  SetLength(m_arrNextNodes, Length(m_arrNextNodes) + 1);
  m_iNextNodeOfLineIndex := High(m_arrNextNodes);
  m_arrNextNodes[m_iNextNodeOfLineIndex] := Node;
end;


function TPlysTreeNode.FEquals(Node: TPlysTreeNode): boolean;
begin
  Result := ((m_strPly = Node.Ply) and (m_strPos = Node.Pos));
end;

end.
