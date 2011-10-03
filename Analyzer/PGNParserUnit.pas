////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit PGNParserUnit;

interface

uses
  TntClasses, Classes,
  //
  ChessRulesEngine, PlysTreeUnit;

type
  TPGNParser = class
  private
    m_Data: TTntStrings;
    m_iDataLine: integer;

    m_bParseResult: boolean;

    m_strWhite: string;
    m_strBlack: string;

    m_Tree: TPlysTree;

    m_bInC4NFormat: boolean;

    m_strStartPosition: string;

    function FGetLine: WideString;
    function FGetNextLine: WideString;
    function FIsEndOfData: boolean;

    procedure FParseTags;
    procedure FParseGame;

    procedure FCheckAgainstC4NVersionSupport(iVersion: integer);

    property StartPosition: string read m_strStartPosition;

  public
    constructor Create;
    destructor Destroy; override;

    function Parse(const wstrData: WideString): boolean; overload;
    function Parse(const AData: TTntStrings): boolean; overload;

    property Tree: TPlysTree read m_Tree;
    property InC4NFormat: boolean read m_bInC4NFormat;
  end;


  IPGNDataCursor = interface
    function GetLine: WideString;
    function GetNextLine: WideString;
    function IsEndOfData: boolean;
  end;

  
  TPGNTagParser = class
  private
    m_DataCursor: IPGNDataCursor;

    m_Tags: TTntStrings;

    m_wstrWhite: WideString;
    m_wstrBlack: WideString;
    m_iC4N: integer;
    m_strFEN: string;

    function FParse: boolean;
    function FProcessLine(const wstr: WideString): boolean;
    procedure FSetC4N(const strValue: string);

  public
    constructor Create;
    destructor Destroy; override;

    function Parse(ADataCursor: IPGNDataCursor): boolean;
    class function IsTag(wstr: WideString): boolean;
    class function IsSameTagLabel(const wstrTag1, wstrTag2: WideString): boolean;

    property Tags: TTntStrings read m_Tags;

    property White: WideString read m_wstrWhite;
    property Black: WideString read m_wstrBlack;
    property C4N: integer read m_iC4N;
    property FEN: string read m_strFEN;
  end;

implementation

uses
  SysUtils, TntSysUtils, StrUtils, Contnrs;

type
  EPGNParser = class(Exception)
  public
    constructor Create;
  end;

  TGameParser = class
  private
    m_wstrlData: TTntStringList;
    m_iDataLine: integer;

    m_ChessRulesEngine: TChessRulesEngine;

    m_Tree: TPlysTree;
    m_bInC4NFormat: boolean;
    m_strStartPosition: string;

    m_strBkpMove: string;
    m_iPly: integer;
    m_bAddPos: boolean;
    m_MovePlyStack: TStack;

    m_bLastInvalidMove: boolean;

    procedure FParse;
    procedure FParseNextToken;
    function FHasTokens: boolean;
    function FParseComment(const wstrToken: WideString; out wstrComment: WideString): boolean;

    function FStartLine(const strToken: string): boolean;
    procedure FTakeBackLine;

    function FNextToken: WideString;
    procedure FAppendDataLeft(const wstr: WideString);

  public
    constructor Create(APGNParser: TPGNParser);
    destructor Destroy; override;

    procedure Parse(const GameData: TTntStrings);

    property InC4NFormat: boolean read m_bInC4NFormat;
  end;


  PMovePly = ^TMovePly;
  TMovePly = object
  private
    m_bNewed: boolean;
  public
    move: string;
    ply: integer;
    addPos: boolean; // ??. genOpening

    function Create: PMovePly;
    procedure Free;
  end;


  TPGNDataCursorAdaptor = class(TInterfacedObject, IPGNDataCursor)
  private
    m_PGNParser: TPGNParser;
  public
    constructor Create(const APGNParser: TPGNParser);
    function GetLine: WideString;
    function GetNextLine: WideString;
    function IsEndOfData: boolean;
  end;

////////////////////////////////////////////////////////////////////////////////
// TPGNParser

constructor TPGNParser.Create;
begin
  inherited Create;
  m_Tree := TPlysTree.Create;
end;


destructor TPGNParser.Destroy;
begin
  m_Tree.Free;
  inherited;
end;


function TPGNParser.Parse(const wstrData: WideString): boolean;
var
  wstrlData: TTntStringList;
begin
  wstrlData := TTntStringList.Create;
  try
    wstrlData.Text := wstrData;
    Result := Parse(wstrlData);
  finally
    wstrlData.Free;
  end;
end;


function TPGNParser.Parse(const AData: TTntStrings): boolean;
begin
  Result := FALSE;

  if (not (Assigned(AData))) then
    exit;

  try
    m_Data := AData;

    m_iDataLine := 0;

    m_strStartPosition := '';

    m_bParseResult := TRUE;
    try
      m_bInC4NFormat := FALSE;
      FParseTags;
      FParseGame;
    except
      on EPGNParser do
        m_bParseResult := FALSE;
    end;

  finally
    m_Data := nil;
  end;

  Result := m_bParseResult;
end;


function TPGNParser.FGetLine: WideString;
begin
  if (m_iDataLine < m_Data.Count) then
    Result := m_Data[m_iDataLine]
  else
    Result := '';
end;


function TPGNParser.FGetNextLine: WideString;
begin
  inc(m_iDataLine);
  
  if (m_iDataLine < (m_Data.Count)) then
    Result := m_Data[m_iDataLine]
  else
    Result := '';
end;


function TPGNParser.FIsEndOfData: boolean;
begin
  Result := (m_iDataLine >= m_Data.Count);
end;


procedure TPGNParser.FParseTags;
begin // .FParseTags
  if (not m_bParseResult) then
    exit;

  with TPGNTagParser.Create do
  try
    m_bParseResult := Parse(TPGNDataCursorAdaptor.Create(self));
    if (not m_bParseResult) then
      exit;

    self.m_strWhite := White;
    self.m_strBlack := Black;
    self.FCheckAgainstC4NVersionSupport(C4N);
    self.m_strStartPosition := FEN;

  finally
    Free;
  end;

end;


procedure TPGNParser.FCheckAgainstC4NVersionSupport(iVersion: integer);
begin
  m_bInC4NFormat := (iVersion in [1, 2]);
end;

(*
procedure TPGNParser.FParseGame;
var
  wstr: WideString;
  wstrlGameData: TTntStringList;
begin
  if ((not m_bParseResult) or FIsEndOfData) then
    exit;

  wstrlGameData := TTntStringList.Create;
  try
    wstr := FGetLine;
    repeat
      if (TPGNTagParser.IsTag(TrimRight(wstr))) then
        break;

      if (wstr <> '') then
        wstrlGameData.Append(wstr);

      wstr := FGetNextLine;
    until (FIsEndOfData);

    m_bParseResult := (wstrlGameData.Count > 0);
    if (not m_bParseResult) then
      exit;

    with TGameParser.Create(self) do
    try
      Parse(wstrlGameData);

      self.m_bInC4NFormat := InC4NFormat;

    finally
      Free;
    end;

  finally
    wstrlGameData.Free;
  end;

end;
*)

procedure TPGNParser.FParseGame;
var
  wstr: WideString;
  wstrlGameData: TTntStringList;
begin
  if (not m_bParseResult) then
    exit;

  wstrlGameData := TTntStringList.Create;
  try
    wstr := FGetLine;
    while (not FIsEndOfData) do
    begin
      if (TPGNTagParser.IsTag(TrimRight(wstr))) then
        break;

      if (wstr <> '') then
        wstrlGameData.Append(wstr);

      wstr := FGetNextLine;
    end;
{
    // TODO: C4N = 3 handling
    m_bParseResult := (wstrlGameData.Count > 0);
    if (not m_bParseResult) then
      exit;
}
    with TGameParser.Create(self) do
    try
      Parse(wstrlGameData);

      self.m_bInC4NFormat := InC4NFormat;

    finally
      Free;
    end;

  finally
    wstrlGameData.Free;
  end;

end;

////////////////////////////////////////////////////////////////////////////////
// EPGNParser

constructor EPGNParser.Create;
begin
  inherited Create('');
end;

////////////////////////////////////////////////////////////////////////////////
// TGameParser

constructor TGameParser.Create(APGNParser: TPGNParser);
begin
  inherited Create;

  m_Tree := APGNParser.Tree;
  m_bInC4NFormat := APGNParser.InC4NFormat;
  m_strStartPosition := APGNParser.StartPosition;

  m_wstrlData := TTntStringList.Create;

  m_ChessRulesEngine := TChessRulesEngine.Create;
  m_ChessRulesEngine.MoveNotationFormat := mnfCh4NEx;
  m_ChessRulesEngine.FENFormat := TRUE;
end;


destructor TGameParser.Destroy;
begin
  m_ChessRulesEngine.Free;
  m_wstrlData.Free;
  
  inherited;
end;


procedure TGameParser.Parse(const GameData: TTntStrings);
begin
  m_iDataLine := 0;
  m_wstrlData.Assign(GameData);

  FParse;
end;


procedure TGameParser.FParse;
begin
  m_ChessRulesEngine.InitNewGame;
  if (m_strStartPosition <> '') then
  begin
    if (not m_ChessRulesEngine.SetPosition(m_strStartPosition)) then
      raise EPGNParser.Create;
  end;

  m_Tree.Clear;
  
  m_Tree.WhiteStarts := (m_ChessRulesEngine.Position.color = fcWhite);
  m_Tree.Add(m_ChessRulesEngine.GetPosition);
  m_Tree.PlysOffset := 2 * m_ChessRulesEngine.MovesOffset;

  m_MovePlyStack := TStack.Create;
  try
    m_iPly := m_Tree.PlysOffset;
    m_strBkpMove := '';
    m_bLastInvalidMove := TRUE;
    m_bAddPos := TRUE;

    while (FHasTokens) do
      FParseNextToken;
{
    // TODO: C4N = 3 handling
    if (m_MovePlyStack.Count <> 0) then
      raise EPGNParser.Create;
}

  finally
    if (m_MovePlyStack.Count > 0) then
      PMovePly(m_MovePlyStack.Pop).Free;
    FreeAndNil(m_MovePlyStack);
  end;

end;


function TGameParser.FHasTokens: boolean;
begin
  Result := (m_iDataLine < m_wstrlData.Count);
end;


procedure TGameParser.FParseNextToken;
var
  wstrToken, wstr: WideString;
  strOriginalMove: string;
  i: integer;
  n: integer;
  posMove: TPosMove;
  bTakebackLineFlag: boolean;
  wstrComment: WideString;
begin
  wstrToken := FNextToken;

  wstr := TrimRight(wstrToken);

  if ((wstr = '') or (wstr = '*') or (wstr = '1-0') or (wstr = '0-1') or (wstr = '1/2-1/2')) then
    exit;

  if (wstr[1] = '{') then
  begin
    if (FParseComment(wstrToken, wstrComment)) then
    begin
      m_Tree.Comments[m_ChessRulesEngine.NPlysDone] := wstrComment;
      exit;
    end;
  end;

  if (wstr[1] = '(') then
  begin
    if (FStartLine(wstr)) then
      exit;
  end;

  if (wstr = ')') then
  begin
    bTakebackLineFlag := TRUE;
    wstr := '';
  end
  else
  begin
    bTakebackLineFlag := FALSE;
    while ((wstr <> '') and (wstr[length(wstr)] = ')')) do
    begin
      FAppendDataLeft(') ');
      wstr := LeftStr(wstr, length(wstr) - 1);
    end;
  end;

  if (wstr = '...') then
    exit;

  for i := length(wstr) downto 1 do
  begin
    if (wstr[i] = '.') then
    begin
      FAppendDataLeft(RightStr(wstr, length(wstr) - i) + ' ');
      wstr := LeftStr(wstr, i);
      break;
    end;
  end;

  if (RightStr(wstr, 2) = '..') then // 21... => 21.
    wstr := LeftStr(wstr, length(wstr) - 2);

  if (wstr <> '') and
     (not ((wstr[length(wstr)] = '.') and TryStrToInt(LeftStr(wstr, length(wstr) - 1), n) and
           (n = (m_iPly shr 1) + 1))) and
     (wstr[1] <> '$') then
  begin
    strOriginalMove := wstr;

    wstr := StringReplace(wstr, 'O-O-O', '0-0-0', []);
    wstr := StringReplace(wstr, 'O-O', '0-0', []);
    wstr := StringReplace(wstr, 'x', '', []);
    wstr := StringReplace(wstr, '=', '', []);
//      s := StringReplace(s, '+', '', []);
//      s := StringReplace(s, '#', '', []);

    posMove.pos := m_ChessRulesEngine.Position^;
    if (m_ChessRulesEngine.DoMove(wstr)) then
    begin
      m_Tree.Add(m_ChessRulesEngine.NPlysDone, m_ChessRulesEngine.GetPosition,
        m_ChessRulesEngine.lastMoveStr);

      m_bInC4NFormat := (m_bInC4NFormat and (strOriginalMove = m_ChessRulesEngine.lastMoveStr));

      m_strBkpMove := wstr;

      inc(m_iPly);

      m_bLastInvalidMove := FALSE;

//      writeln(n_ply, 'p. ' + s + #9 + ChessBoard.GetPosition); // DEBUG:
    end
    else
    begin
//      writeln(s, ' n_pos: ', n_pos); // DEBUG:
      if (m_bLastInvalidMove) then
        raise EPGNParser.Create;

      m_bLastInvalidMove := TRUE;
    end; // if (ChessRulesEngine.DoMove(s))

    m_strBkpMove := wstr;
  end; { if (s <> '') ...}

  if (bTakebackLineFlag) then
    FTakeBackLine;
end;


procedure TGameParser.FAppendDataLeft(const wstr: WideString);
begin
  m_wstrlData[m_iDataLine] := wstr + m_wstrlData[m_iDataLine];
end;


function TGameParser.FNextToken: WideString;
var
  wstr: WideString;
  iPos: integer;
begin
  Result := '';

  repeat
    if (not FHasTokens) then
      exit;

    wstr := TrimLeft(m_wstrlData[m_iDataLine]);

    if (wstr = '') then
      inc(m_iDataLine);
  until (wstr <> '');

  iPos := Pos(' ', wstr);

  if (iPos = 0) then
  begin
    Result := wstr;
    m_wstrlData[m_iDataLine] := '';
    exit;
  end;

  repeat
    inc(iPos);
  until ((iPos > Length(wstr)) or (wstr[iPos] <> ' '));

  Result := Copy(wstr, 1, iPos - 1);
  m_wstrlData[m_iDataLine] := Copy(wstr, iPos, MaxInt);
end;


function TGameParser.FParseComment(const wstrToken: WideString; out wstrComment: WideString): boolean;
var
  wstr: WideString;
  iPos: integer;
  bEndOfComment: boolean;
  bNeedTrimFlag: boolean;
begin
  wstrComment := '';
  Result := TRUE;

  Assert((wstrToken <> '') and (wstrToken[1] = '{'));

  wstr := Copy(wstrToken, 2, MaxInt);

  bEndOfComment := FALSE;
  bNeedTrimFlag := FALSE;

  while (FHasTokens) do
  begin
    if (TrimRight(wstr) = '|') then
    begin
      if (bNeedTrimFlag) then
      begin
        bNeedTrimFlag := FALSE;
        wstrComment := TrimRight(wstrComment);
      end;
      wstrComment := wstrComment + sLineBreak + RightStr(wstr, Length(wstr) - 2);

      wstr := FNextToken;
      continue;
    end;

    bNeedTrimFlag := TRUE;

    iPos := 0;
    repeat
      inc(iPos);
      iPos := PosEx('}', wstr, iPos);
      if (iPos > 0) then
      begin
        if ((iPos < Length(wstr)) and (wstr[iPos + 1] = '}')) then
          Delete(wstr, iPos, 1)
        else
        begin
          bEndOfComment := TRUE;
          FAppendDataLeft(Copy(wstr, iPos + 1, MaxInt));
          wstr := Copy(wstr, 1, iPos - 1);
        end;
      end;
    until (iPos = 0);

    wstr := Tnt_WideStringReplace(wstr, '||', '|', [rfReplaceAll], TRUE);
    wstrComment := wstrComment + wstr;

    if (bEndOfComment) then
      exit;

    wstr := FNextToken;
  end;

  raise EPGNParser.Create;
end;


function TGameParser.FStartLine(const strToken: string): boolean;
var
  movePly: PMovePly;
begin
  Result := TRUE;

  movePly := nil; // To supress warning

  if ((length(strToken) > 1) and (strToken[2] = '{')) then
  begin
    FAppendDataLeft('( {');
    exit;
  end;

  movePly := movePly.Create;

  movePly.move := m_strBkpMove;
  movePly.ply := m_iPly;
  movePly.addPos := m_bAddPos;

  if (not m_bLastInvalidMove) then
  begin
    movePly.ply := m_iPly;
    m_ChessRulesEngine.TakeBack;
    dec(m_iPly);
  end
  else
    movePly.ply := m_iPly + 1;

  m_MovePlyStack.Push(movePly);

  if (RightStr(strToken, length(strToken) - 1) = '') then
    exit;

  Result := FALSE;
end;


procedure TGameParser.FTakeBackLine;
var
  movePly: PMovePly;
begin
  movePly := PMovePly(m_MovePlyStack.Pop);
  try
    while (movePly.ply <= m_iPly) do
    begin
      m_ChessRulesEngine.TakeBack;
      dec(m_iPly);
    end;

    if m_ChessRulesEngine.DoMove(movePly.move) then
    begin
      m_Tree.SetPlyForPlyIndex(m_ChessRulesEngine.NPlysDone - m_Tree.PlysOffset,
        m_ChessRulesEngine.lastMoveStr);
      inc(m_iPly);
    end
    else
    begin
      m_bLastInvalidMove := TRUE;
      m_bAddPos := movePly.addPos; // Opening
    end;

    m_strBkpMove := movePly.move;

  finally
    movePly.Free;
  end;

end;

////////////////////////////////////////////////////////////////////////////////
// TPGNTagParser

constructor TPGNTagParser.Create;
begin
  inherited Create;

  m_Tags := TTntStringList.Create;
end;


destructor TPGNTagParser.Destroy;
begin
  m_Tags.Free;

  inherited;
end;


class function TPGNTagParser.IsTag(wstr: WideString): boolean;
begin
  wstr := TrimRight(wstr);
  Result := ((wstr <> '') and ((wstr[1] = '[') and (wstr[length(wstr)] = ']')));
end;


class function TPGNTagParser.IsSameTagLabel(const wstrTag1, wstrTag2: WideString): boolean;
var
  iPos1, iPos2: integer;
  wstrTag1Label, wstrTag2Label: WideString;
begin
  iPos1 := Pos(' ', wstrTag1);
  iPos2 := Pos(' ', wstrTag2);

  Result := (iPos1 = iPos2);
  if (not Result) then
    exit;

  wstrTag1Label := Copy(wstrTag1, 2, iPos1 - 2);
  wstrTag2Label := Copy(wstrTag2, 2, iPos2 - 2);

  Result := (wstrTag1Label = wstrTag2Label);
end;


function TPGNTagParser.Parse(ADataCursor: IPGNDataCursor): boolean;
begin
  Assert(Assigned(ADataCursor));
  
  m_DataCursor := ADataCursor;
  try
    Result := FParse;
  finally
    m_DataCursor := nil;
  end;
end;


function TPGNTagParser.FParse: boolean;
var
  wstr: WideString;
begin
  Result := FALSE;

  m_Tags.Clear;

  if (m_DataCursor.IsEndOfData) then
    exit;

  Result := TRUE;

  wstr := m_DataCursor.GetLine;
  repeat
    wstr := TrimRight(wstr);
    if (wstr <> '') then
    begin
      if (not FProcessLine(wstr)) then
        exit;
      m_Tags.Append(wstr);
    end;

    wstr := m_DataCursor.GetNextLine;

  until (m_DataCursor.IsEndOfData);

end;


function TPGNTagParser.FProcessLine(const wstr: WideString): boolean;
const
  WHITE_PREFIX = '[White "';
  BLACK_PREFIX = '[Black "';
  C4N_PREFIX = '[C4N "';
  FEN_PREFIX = '[FEN "';
  POSTFIX = '"]';
begin
  Result := FALSE;

  if (not TPGNTagParser.IsTag(wstr)) then
    exit;

  if ((LeftStr(wstr, length(WHITE_PREFIX)) = WHITE_PREFIX) and
      (RightStr(wstr, length(POSTFIX)) = POSTFIX)) then
  begin
    m_wstrWhite := Copy(wstr, length(WHITE_PREFIX) + 1,
      length(wstr) - length(WHITE_PREFIX) - length(POSTFIX));
  end
  else if ((LeftStr(wstr, length(BLACK_PREFIX)) = BLACK_PREFIX) and
      (RightStr(wstr, length(POSTFIX)) = POSTFIX)) then
  begin
    m_wstrBlack := Copy(wstr, length(BLACK_PREFIX) + 1,
      length(wstr) - length(BLACK_PREFIX) - length(POSTFIX));
  end
  else if ((LeftStr(wstr, length(C4N_PREFIX)) = C4N_PREFIX) and
      (RightStr(wstr, length(POSTFIX)) = POSTFIX)) then
  begin
    FSetC4N(Copy(wstr, length(C4N_PREFIX) + 1,
      length(wstr) - length(C4N_PREFIX) - length(POSTFIX)));
  end
  else if ((LeftStr(wstr, length(FEN_PREFIX)) = FEN_PREFIX) and
      (RightStr(wstr, length(POSTFIX)) = POSTFIX)) then
  begin
    m_strFEN := Copy(wstr, length(FEN_PREFIX) + 1,
      length(wstr) - length(FEN_PREFIX) - length(POSTFIX));
  end;

  Result := TRUE;
end;


procedure TPGNTagParser.FSetC4N(const strValue: string);
begin
  m_iC4N := StrToIntDef(strValue, 0);
  if (m_iC4N < 0) then
    m_iC4N := 0;
end;

////////////////////////////////////////////////////////////////////////////////
// TPGNDataCursorAdaptor

constructor TPGNDataCursorAdaptor.Create(const APGNParser: TPGNParser);
begin
  inherited Create;
  m_PGNParser := APGNParser;
end;


function TPGNDataCursorAdaptor.GetLine: WideString;
begin
  Result := m_PGNParser.FGetLine;
end;


function TPGNDataCursorAdaptor.GetNextLine: WideString;
begin
  Result := m_PGNParser.FGetNextLine;
end;


function TPGNDataCursorAdaptor.IsEndOfData: boolean;
begin
  Result := m_PGNParser.FIsEndOfData;
end;

////////////////////////////////////////////////////////////////////////////////
// TMovePly

function TMovePly.Create: PMovePly;
begin
  New(Result);
  Result.m_bNewed := TRUE;
end;


procedure TMovePly.Free;
begin
  if (not Assigned(@self)) then
    exit;

  move := '';

  if (m_bNewed) then
    Dispose(@self);
end;

end.
