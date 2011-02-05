unit PlysTreeUnit;

interface

uses
  Classes,
  //
  PlysProviderIntfUnit;

type
  TPlysTreeNode = class
  private
    m_strPly: string;
    m_strPos: string;
    m_PlyStatuses: TPlyStatuses;

    m_arrNextNodes: array of TPlysTreeNode;
    m_iNextNodeOfLineIndex: integer;

    constructor Create(const strPos, strPly: string; APlyStatuses: TPlyStatuses); overload;
    class function Create(const Source: TPlysTreeNode): TPlysTreeNode; overload;

    function FGetNextNodeOfLine: TPlysTreeNode;
    procedure FDeleteNextNodeOfLine;
    procedure FDeleteNextNodes;
    procedure FAddLineNode(Node: TPlysTreeNode);
    function FGetNextNodesCount: integer;
    procedure FGetNextNodesList(var List: TStrings);
    function FSetNextNodeOfLineToPly(const strPly: string): boolean;

    function FEquals(Node: TPlysTreeNode): boolean;

    procedure FRemapMainLine;

    property Ply: string read m_strPly;
    property Pos: string read m_strPos;
    property PlyStatuses: TPlyStatuses read m_PlyStatuses write m_PlyStatuses;

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
    function FGetWhiteStarts: boolean;
    procedure FDelete(iIndex: Integer);
    
  public
    destructor Destroy; override;

    function Add(iPlyIndex: integer; const strPos: string;
      const strMove: string = ''; APlyStatuses: TPlyStatuses = []): integer; overload;
    function Add(const strPos: string; strMove: string = '';
      APlyStatuses: TPlyStatuses = []): integer; overload;

    procedure Clear;
    function Delete(iIndex: Integer): boolean;

    function GetPlysCountForPlyIndex(iIndex: integer): integer;
    procedure GetPlysForPlyIndex(iIndex: integer; var List: TStrings);
    function SetPlyForPlyIndex(iIndex: integer; const strPly: string): boolean;
    function GetPlyStatus(iIndex: integer): TPlyStatuses;

    procedure Assign(const Source: TPlysTree);

    class function ConvertPlyToMove(iPly: integer; bWhiteStarts: boolean): integer;
    class function IsWhiteToMove(iPly: integer; bWhiteStarts: boolean): boolean;

    property Plys[iIndex: integer]: string read FGetPly; default;
    property Position[iIndex: integer]: string read FGetPosition;

    property Count: integer read FGetCount;
    property WhiteStarts: boolean read FGetWhiteStarts;
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
  FDelete(0);
end;


procedure TPlysTree.FDelete(iIndex: Integer);
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
  if (iPlyDepth < 0) then
  begin
    Result := nil;
    exit;
  end;

  Result := m_FirstNode;    
        
  for i := 0 to iPlyDepth - 1 do
  begin
    if (not Assigned(Result)) then
      exit;
    Result := Result.FGetNextNodeOfLine;
  end;
end;


function TPlysTree.Add(iPlyIndex: integer; const strPos: string;
  const strMove: string = ''; APlyStatuses: TPlyStatuses = []): integer;
var
  Node, NextNode: TPlysTreeNode;
begin
  Assert(iPlyIndex >= 0);

  Result := 0;

  if (iPlyIndex = 0) then
  begin
    Clear;
    m_FirstNode := TPlysTreeNode.Create(strPos, strMove, APlyStatuses);
    m_FirstNode.PlyStatuses := m_FirstNode.PlyStatuses + [psMainLine];
    exit;
  end;

  NextNode := m_FirstNode;
  repeat
    inc(Result);
    Node := NextNode;
    NextNode := Node.FGetNextNodeOfLine;
  until ((Result >= iPlyIndex) or (not Assigned(NextNode)));

  Node.FAddLineNode(TPlysTreeNode.Create(strPos, strMove, APlyStatuses));
end;


function TPlysTree.Add(const strPos: string; strMove: string = '';
  APlyStatuses: TPlyStatuses = []): integer;
begin
  Result := Add(Count, strPos, strMove, APlyStatuses);
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


function TPlysTree.GetPlysCountForPlyIndex(iIndex: integer): integer;
var
  Node: TPlysTreeNode;
begin
  Node := FGetNodeOfDepth(iIndex - 1);
  if (Assigned(Node)) then
    Result := Node.FGetNextNodesCount
  else
    Result := 0;
end;


procedure TPlysTree.GetPlysForPlyIndex(iIndex: integer; var List: TStrings);
var
  Node: TPlysTreeNode;
begin
  if (not Assigned(List)) then
    exit;

  List.Clear;

  Node := FGetNodeOfDepth(iIndex - 1);
  if (Assigned(Node)) then
    Node.FGetNextNodesList(List);
end;


function TPlysTree.GetPlyStatus(iIndex: integer): TPlyStatuses;
var
  Node: TPlysTreeNode;
begin
  Node := FGetNodeOfDepth(iIndex);
  if (Assigned(Node)) then
    Result := Node.PlyStatuses
  else
    Result := [];
end;


function TPlysTree.SetPlyForPlyIndex(iIndex: integer; const strPly: string): boolean;
var
  Node: TPlysTreeNode;
begin
  Result := FALSE;

  Node := FGetNodeOfDepth(iIndex - 1);
  if (Assigned(Node)) then
    Result := Node.FSetNextNodeOfLineToPly(strPly);
end;


procedure TPlysTree.Assign(const Source: TPlysTree);
begin
  Assert(Assigned(Source));

  Clear;
  m_FirstNode := TPlysTreeNode.Create(Source.m_FirstNode);
end;


function TPlysTree.FGetWhiteStarts: boolean;
begin
  Result := TRUE;
end;


class function TPlysTree.ConvertPlyToMove(iPly: integer; bWhiteStarts: boolean): integer;
begin
  if (not bWhiteStarts) then
    inc(iPly);
  Result := (iPly + 1) div 2;
end;


class function TPlysTree.IsWhiteToMove(iPly: integer; bWhiteStarts: boolean): boolean;
begin
  if (not bWhiteStarts) then
    inc(iPly);
  Result := Odd(iPly);
end;


function TPlysTree.Delete(iIndex: Integer): boolean;
var
  bRemapMainLine: boolean;
  Node: TPlysTreeNode;
begin
  Result := ((iIndex >= 0) and (iIndex < Count));

  bRemapMainLine := (psMainLine in GetPlyStatus(iIndex));

  FDelete(iIndex);

  if (bRemapMainLine) then
  begin
    Node := FGetNodeOfDepth(iIndex - 1);
    if (Assigned(Node)) then
      Node.FRemapMainLine;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPlysTreeNode

constructor TPlysTreeNode.Create(const strPos, strPly: string; APlyStatuses: TPlyStatuses);
begin
  inherited Create;

  m_strPos := strPos;
  m_strPly := strPly;
  m_PlyStatuses := APlyStatuses;

  m_iNextNodeOfLineIndex := -1;
end;


class function TPlysTreeNode.Create(const Source: TPlysTreeNode): TPlysTreeNode;
var
  i: integer;
begin
  if (not Assigned(Source)) then
  begin
    Result := nil;
    exit;
  end;

  Result := inherited Create;

  with Result do
  begin
    m_strPos := Source.m_strPos;
    m_strPly := Source.m_strPly;
    m_PlyStatuses := Source.m_PlyStatuses;

    SetLength(m_arrNextNodes, Length(Source.m_arrNextNodes));
    for i := Low(Source.m_arrNextNodes) to High(Source.m_arrNextNodes) do
      m_arrNextNodes[i] := TPlysTreeNode.Create(Source.m_arrNextNodes[i]);

    m_iNextNodeOfLineIndex := Source.m_iNextNodeOfLineIndex;
  end;
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
    if (Assigned(m_arrNextNodes[i]) and m_arrNextNodes[i].FEquals(Node)) then
    begin
      m_iNextNodeOfLineIndex := i;
      Node.Free;
      exit;
    end;
  end;

  if ((psMainLine in PlyStatuses) and (FGetNextNodesCount = 0)) then
    Node.PlyStatuses := Node.PlyStatuses + [psMainLine];

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


function TPlysTreeNode.FGetNextNodesCount: integer;
var
  i: integer;
begin
  Result := 0;
  for i := Low(m_arrNextNodes) to High(m_arrNextNodes) do
  begin
    if (Assigned(m_arrNextNodes[i])) then
      inc(Result);
  end;
end;


procedure TPlysTreeNode.FGetNextNodesList(var List: TStrings);
var
  i: integer;
  Node: TPlysTreeNode;
begin
  List.Clear;

  for i := Low(m_arrNextNodes) to High(m_arrNextNodes) do
  begin
    Node := m_arrNextNodes[i];
    if (Assigned(Node)) then
      List.Append(Node.Ply);
  end;
end;


function TPlysTreeNode.FSetNextNodeOfLineToPly(const strPly: string): boolean;
var
  i: integer;
  Node: TPlysTreeNode;
begin
  Result := FALSE;

  for i := Low(m_arrNextNodes) to High(m_arrNextNodes) do
  begin
    Node := m_arrNextNodes[i];
    if (not Assigned(Node)) then
      continue;

    Result := ((Node.Ply = strPly) and (i <> m_iNextNodeOfLineIndex));
    if (Result) then
    begin
      m_iNextNodeOfLineIndex := i;
      exit;
    end;
  end;
end;


procedure TPlysTreeNode.FRemapMainLine;
var
  i: integer;
  NextNode: TPlysTreeNode;
begin
  Include(m_PlyStatuses, psMainLine);

  for i := Low(m_arrNextNodes) to High(m_arrNextNodes) do
  begin
    NextNode := m_arrNextNodes[i];
    if (Assigned(NextNode)) then
    begin
      if (i > Low(m_arrNextNodes)) then
      begin
        m_arrNextNodes[i] := nil;
        m_arrNextNodes[Low(m_arrNextNodes)] := NextNode;
        if (m_iNextNodeOfLineIndex = i) then
          m_iNextNodeOfLineIndex := Low(m_arrNextNodes);
      end;
      NextNode.FRemapMainLine;
      exit;
    end;
  end; // for i
  
end;

end.
