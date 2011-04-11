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

    function  FIsTag(const wstr: WideString): boolean;
    procedure FParseTags;
    procedure FParseGame;

    procedure FCheckAgainstC4NVersionSupport(const strVersion: string);
    procedure FSetupStartPosition(const strPosition: string);

    property StartPosition: string read m_strStartPosition;

  public
    constructor Create;
    destructor Destroy; override;

    function Parse(const AData: TTntStrings): boolean;

    property Tree: TPlysTree read m_Tree;
    property InC4NFormat: boolean read m_bInC4NFormat;
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
    m_PosMoveStack, m_MovePlyStack: TStack;

    m_bLastInvalidMove: boolean;

    procedure FParse;
    procedure FParseNextToken;
    function FHasNoTokens: boolean;
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
  TMovePly = record
    move: string;
    ply: integer;
    addPos: boolean; // ??. genOpening
    posMoveCount: integer;
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

  function NProcessLine(const wstr: WideString): boolean;
  const
    WHITE_PREFIX = '[White "';
    BLACK_PREFIX = '[Black "';
    C4N_PREFIX = '[C4N "';
    FEN_PREFIX = '[FEN "';
    POSTFIX = '"]';
  begin
    Result := FALSE;

    if (not FIsTag(wstr)) then
      exit;

    if ((LeftStr(wstr, length(WHITE_PREFIX)) = WHITE_PREFIX) and
        (RightStr(wstr, length(POSTFIX)) = POSTFIX)) then
    begin
      m_strWhite := Copy(wstr, length(WHITE_PREFIX) + 1,
        length(wstr) - length(WHITE_PREFIX) - length(POSTFIX));
    end
    else if ((LeftStr(wstr, length(BLACK_PREFIX)) = BLACK_PREFIX) and
        (RightStr(wstr, length(POSTFIX)) = POSTFIX)) then
    begin
      m_strBlack := Copy(wstr, length(BLACK_PREFIX) + 1,
        length(wstr) - length(BLACK_PREFIX) - length(POSTFIX));
    end
    else if ((LeftStr(wstr, length(C4N_PREFIX)) = C4N_PREFIX) and
        (RightStr(wstr, length(POSTFIX)) = POSTFIX)) then
    begin
      FCheckAgainstC4NVersionSupport(Copy(wstr, length(C4N_PREFIX) + 1,
        length(wstr) - length(C4N_PREFIX) - length(POSTFIX)));
    end
    else if ((LeftStr(wstr, length(FEN_PREFIX)) = FEN_PREFIX) and
        (RightStr(wstr, length(POSTFIX)) = POSTFIX)) then
    begin
      FSetupStartPosition(Copy(wstr, length(FEN_PREFIX) + 1,
        length(wstr) - length(FEN_PREFIX) - length(POSTFIX)));
    end;

    Result := TRUE;
  end;

var
  wstr: WideString;
begin // .FParseTags
  m_bParseResult := (m_bParseResult and (not FIsEndOfData));
  if (not m_bParseResult) then
    exit;

  wstr := FGetLine;
  repeat
    wstr := TrimRight(wstr);
    if (wstr <> '') then
    begin
      if (not NProcessLine(wstr)) then
        exit;
    end;

    wstr := FGetNextLine;

  until (FIsEndOfData);
end;


procedure TPGNParser.FSetupStartPosition(const strPosition: string);
begin
  m_strStartPosition := strPosition;
end;


function  TPGNParser.FIsTag(const wstr: WideString): boolean;
begin
  Result := ((wstr <> '') and ((wstr[1] = '[') and (wstr[length(wstr)] = ']')));
end;


procedure TPGNParser.FCheckAgainstC4NVersionSupport(const strVersion: string);
var
  iC4NFormatVer: integer;
begin
  iC4NFormatVer := StrToIntDef(strVersion, 0);
  m_bInC4NFormat := (iC4NFormatVer in [1, 2]);
end;


procedure TPGNParser.FParseGame;
var
  wstr: WideString;
  wstrlGameData: TTntStringList;
begin
  m_bParseResult := (m_bParseResult and (not FIsEndOfData));
  if (not m_bParseResult) then
    exit;

  wstrlGameData := TTntStringList.Create;
  try
    wstr := FGetLine;
    repeat
      if (FIsTag(TrimRight(wstr))) then
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
var
  moveEsts: TList;
  i: integer;
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

  moveEsts := nil;
  m_PosMoveStack := nil;
  m_MovePlyStack := TStack.Create;
  try
    moveEsts := TList.Create;
    m_PosMoveStack := TStack.Create;

    m_iPly := m_Tree.PlysOffset;
    m_strBkpMove := '';
    m_bLastInvalidMove := TRUE;
    m_bAddPos := TRUE;

    repeat
      FParseNextToken;
    until FHasNoTokens;

    if (m_MovePlyStack.Count <> 0) then
      raise EPGNParser.Create;

  finally
    for i := 0 to moveEsts.Count - 1 do
      Dispose(moveEsts[i]);
    moveEsts.Free;

    while (m_PosMoveStack.Count > 0) do
      Dispose(m_PosMoveStack.Pop);
    m_PosMoveStack.Free;

    m_MovePlyStack.Free;
  end;

end;


function TGameParser.FHasNoTokens: boolean;
begin
  Result := (m_iDataLine >= m_wstrlData.Count);
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
  bTokenFound: boolean;
begin
  Result := '';

  repeat
    if (FHasNoTokens) then
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

  while (not FHasNoTokens) do
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

  Result := FALSE;

  raise EPGNParser.Create;
end;


function TGameParser.FStartLine(const strToken: string): boolean;
var
  movePly: PMovePly;
begin
  Result := TRUE;

  if ((length(strToken) > 1) and (strToken[2] = '{')) then
  begin
    FAppendDataLeft('( {');
    exit;
  end;

  New(movePly);
  movePly.move := m_strBkpMove;
  movePly.ply := m_iPly;
  movePly.addPos := m_bAddPos;
  movePly.posMoveCount := m_PosMoveStack.Count;

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
  n: integer;
begin
  movePly := PMovePly(m_MovePlyStack.Pop);
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

  n := movePly.posMoveCount;
  while (n < m_PosMoveStack.Count) do // Deletion of subline stack
    Dispose(m_PosMoveStack.Pop);

  m_strBkpMove := movePly.move;

  movePly.move := '';
  Dispose(movePly);
end;


end.
