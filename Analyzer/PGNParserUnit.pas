unit PGNParserUnit;

interface

uses
  Classes,
  //
  ChessRulesEngine, PlysTreeUnit;

type
  TPGNParser = class
  private
    m_Data: TStrings;
    m_iDataLine: integer;
    m_bParseResult: boolean;

    m_strWhite: string;
    m_strBlack: string;

    m_ChessRulesEngine: TChessRulesEngine;
    m_Tree: TPlysTree;

    m_bInC4NFormat: boolean;

    m_strStartPosition: string;

    function FGetLine: string;
    function FGetNextLine: string;
    function FIsEndOfData: boolean;

    function  FIsTag(const str: string): boolean;
    procedure FParseTags;
    procedure FParseGame;
    procedure FParseGameStr(const strGame: string; bIncludeVariants: boolean);

    procedure FCheckAgainstC4NVersionSupport(const strVersion: string);
    procedure FSetupStartPosition(const strPosition: string);

  public
    constructor Create;
    destructor Destroy; override;

    function Parse(const AData: TStrings): boolean;

    property Tree: TPlysTree read m_Tree;
    property InC4NFormat: boolean read m_bInC4NFormat;
  end;

implementation

uses
  SysUtils, StrUtils, Contnrs;

type
  EPGNParser = class(Exception)
  public
    constructor Create;
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


function TPGNParser.Parse(const AData: TStrings): boolean;
begin
  Result := FALSE;

  if (not (Assigned(AData))) then
    exit;

  m_ChessRulesEngine := TChessRulesEngine.Create;
  try
    m_ChessRulesEngine.MoveNotationFormat := mnfCh4NEx;
    m_ChessRulesEngine.FENFormat := TRUE; 

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
    FreeAndNil(m_ChessRulesEngine);
  end;

  Result := m_bParseResult;
end;


function TPGNParser.FGetLine: string;
begin
  if (m_iDataLine < m_Data.Count) then
    Result := m_Data[m_iDataLine]
  else
    Result := '';
end;


function TPGNParser.FGetNextLine: string;
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

  function NProcessLine(const s: string): boolean;
  const
    WHITE_PREFIX = '[White "';
    BLACK_PREFIX = '[Black "';
    C4N_PREFIX = '[C4N "';
    FEN_PREFIX = '[FEN "';
    POSTFIX = '"]';
  begin
    Result := FALSE;

    if (not FIsTag(s)) then
      exit;

    if ((LeftStr(s, length(WHITE_PREFIX)) = WHITE_PREFIX) and
        (RightStr(s, length(POSTFIX)) = POSTFIX)) then
    begin
      m_strWhite := Copy(s, length(WHITE_PREFIX) + 1,
        length(s) - length(WHITE_PREFIX) - length(POSTFIX));
    end
    else if ((LeftStr(s, length(BLACK_PREFIX)) = BLACK_PREFIX) and
        (RightStr(s, length(POSTFIX)) = POSTFIX)) then
    begin
      m_strBlack := Copy(s, length(BLACK_PREFIX) + 1,
        length(s) - length(BLACK_PREFIX) - length(POSTFIX));
    end
    else if ((LeftStr(s, length(C4N_PREFIX)) = C4N_PREFIX) and
        (RightStr(s, length(POSTFIX)) = POSTFIX)) then
    begin
      FCheckAgainstC4NVersionSupport(Copy(s, length(C4N_PREFIX) + 1,
        length(s) - length(C4N_PREFIX) - length(POSTFIX)));
    end
    else if ((LeftStr(s, length(FEN_PREFIX)) = FEN_PREFIX) and
        (RightStr(s, length(POSTFIX)) = POSTFIX)) then
    begin
      FSetupStartPosition(Copy(s, length(FEN_PREFIX) + 1,
        length(s) - length(FEN_PREFIX) - length(POSTFIX)));
    end;

    Result := TRUE;
  end;

var
  s: string;
begin // .FParseHeader
  m_bParseResult := (m_bParseResult and (not FIsEndOfData));
  if (not m_bParseResult) then
    exit;

  s := FGetLine;
  repeat
    s := TrimRight(s);
    if (s <> '') then
    begin
      if (not NProcessLine(s)) then
        exit;
    end;

    s := FGetNextLine;

  until (FIsEndOfData);
end;


procedure TPGNParser.FSetupStartPosition(const strPosition: string);
begin
  m_strStartPosition := strPosition;
end;


function  TPGNParser.FIsTag(const str: string): boolean;
begin
  Result := ((str <> '') and ((str[1] = '[') and (str[length(str)] = ']')));
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
  s: string;
  ss: string;
begin
  m_bParseResult := (m_bParseResult and (not FIsEndOfData));
  if (not m_bParseResult) then
    exit;

  ss := '';

  s := FGetLine;
  repeat
    s := TrimRight(s);
    if (FIsTag(s)) then
      break;

    ss := ss + ' ' + s;

    s := FGetNextLine;
  until (FIsEndOfData);

  m_bParseResult := (ss <> '');
  if (not m_bParseResult) then
    exit;

  FParseGameStr(ss, TRUE);
end;


procedure TPGNParser.FParseGameStr(const strGame: string; bIncludeVariants: boolean);
var
  n_ply: integer;
  bkpMove: string;
  movePlyStack, posMoveStack: TStack;
  moveEsts: TList;
  lastInvalidMove: boolean;
  addPos: boolean;
//  addSimplePosMove: boolean;
//  SimplePosMove: TPosMove;

  procedure NProceedInner(var str: string);
{
    procedure NProcessExtendedOpeningLine(const posMove: TPosMove);
    var
      p_posMove: PPosMove;
    begin
      if (addPos) then
      begin
        // Adding previous positions, which hadn't been added to DB before
        while (posMoveStack.Count > 0) do
        begin
          PosBase.Add(PPosMove(posMoveStack.Peek)^);
          Dispose(posMoveStack.Pop);
        end;
        addSimplePosMove := FALSE;
      end
      else
      begin
        New(p_posMove);
        p_posMove^ := posMove;
        posMoveStack.Push(p_posMove);
        if ((genOpening = openExtendedPlus) and (not addSimplePosMove)) then
        begin
          addSimplePosMove := TRUE;
          SimplePosMove := posMove;
        end;
      end;
    end;
}
{
    procedure NProcessOpeningLine(const posMove: TPosMove);
    var
      i: integer;
    begin
      if (Assigned(RefPosBase)) then
        addPos := RefPosBase.Find(posMove.pos, moveEsts)
      else
        addPos := PosBase.Find(posMove.pos, moveEsts);

      if (not addPos) then
        exit;

      i := moveEsts.Count - 1;
      while (i >= 0) do
      begin
        with PMoveEst(moveEsts[i]).move, posMove do
          addPos := (i0 = move.i0) and (j0 = move.j0) and (i = move.i) and
                    (j = move.j) and (prom_fig = move.prom_fig);
        if (addPos) then
        begin
          if (useStatPrunning) then
            addPos := ((PMoveEst(moveEsts[i]).estimate and $FFFF) >= 2);
          if (addPos) then
            break;
        end;
        dec(i);
      end;
      if (i < 0) then
        addPos := FALSE;
    end;
}
    function NCutOutComments(const s: string): boolean;
    var
      i: integer;
    begin
      Result := TRUE;

      str := s + ' ' + str;
      for i := 1 to length(str) do
      begin
        if (str[i] = '}') then
        begin
          str := RightStr(str, length(str) - i);
          exit;
        end;
//       writeln(' n_pos: ', n_pos);
      end;
//      Assert(FALSE);
      raise EPGNParser.Create;

      Result := FALSE;
    end; // \NCutOutComments

    type
      PMovePly = ^TMovePly;
      TMovePly = record
        move: string;
        ply: integer;
        addPos: boolean; // ??. genOpening
        posMoveCount: integer;
      end;

    function NStartLine(const s: string): boolean;
    var
      n: integer;
      i: integer;
      movePly: PMovePly;
    begin
      Result := TRUE;

      if ((length(s) > 1) and (s[2] = '{')) then
      begin
        str := '( {' + str;
        exit;
      end;

      if (not bIncludeVariants) then
      begin
        n := 1;
        i := 1;
        repeat
          case str[i] of
            '{':
            begin
              repeat
                inc(i);
              until (i > length(str)) or (str[i] = '}');
            end;

            '(':
              inc(n);

            ')':
            begin
              dec(n);
              if (n = 0) then
              begin
                str := RightStr(str, length(str) - i);
                exit;
              end;
            end;

          end; { case }

          inc(i);

        until (i > length(str));

  //      Assert(FALSE);
        raise EPGNParser.Create;

      end; // if (not incVariants)

      New(movePly);
      movePly.move := bkpMove;
      movePly.ply := n_ply;
      movePly.addPos := addPos;
      movePly.posMoveCount := posMoveStack.Count;

      if (not lastInvalidMove) then
      begin
        movePly.ply := n_ply;
        m_ChessRulesEngine.TakeBack;
        dec(n_ply);
      end
      else
        movePly.ply := n_ply + 1;

      movePlyStack.Push(movePly);

      if (RightStr(s, length(s) - 1) = '') then
        exit;

      Result := FALSE;
    end; // \NStartLine

    function NextWord: string;
    var
      l : integer;
    begin
      str := TrimLeft(str);
      l := pos(' ', str);
      if (l = 0) then
      begin
        Result := str;
        str := '';
      end
      else
      begin
        Result := LeftStr(str, l - 1);
        str := RightStr(str, length(str) - l);
      end;
    end; // \NextWord

    procedure NTakeBackLine;
    var
      movePly: PMovePly;
      n: integer;
    begin
      movePly := PMovePly(movePlyStack.Pop);
      while (movePly.ply <= n_ply) do
      begin
        m_ChessRulesEngine.TakeBack;
        dec(n_ply);
      end;

      if m_ChessRulesEngine.DoMove(movePly.move) then
      begin
        m_Tree.SetPlyForPlyIndex(m_ChessRulesEngine.NPlysDone - m_Tree.PlysOffset,
          m_ChessRulesEngine.lastMoveStr);
        inc(n_ply);
//        addPos := (genOpening = openNo) or PosBase.Find(ChessRulesEngine.Position^); // Opening
      end
      else
      begin
        lastInvalidMove := TRUE;
        addPos := movePly.addPos; // Opening
      end;

      n := movePly.posMoveCount;
      while (n < posMoveStack.Count) do // Deletion of subline stack
        Dispose(posMoveStack.Pop);

      bkpMove := movePly.move;

      movePly.move := '';
      Dispose(movePly);
    end; // \NTakeBackLine

  var
    s, strOriginalMove: string;
    i: integer;
    n: integer;
//    movePly: PMovePly;
    posMove: TPosMove;
//    p_posMove: PPosMove;
    bTakebackLineFlag: boolean;
  begin // \NProceedInner
    s := NextWord;

    if (s = '') or (s = '*') or (s = '1-0') or (s = '0-1') or (s = '1/2-1/2') then
      exit;

    if (s[1] = '{') then // Cuts out comments
    begin
      if (NCutOutComments(s)) then
        exit;
    end;

    if (s[1] = '(') then
    begin
      if (NStartLine(s)) then
        exit;
    end;

    if (s = ')') then
    begin
      bTakebackLineFlag := TRUE;
      s := '';
    end
    else
    begin
      bTakebackLineFlag := FALSE;
      while ((s <> '') and (s[length(s)] = ')')) do
      begin
        str := ') ' + str;
        s := LeftStr(s, length(s) - 1);
      end;
    end;

    if (s = '...') then
      exit;

    for i := length(s) downto 1 do
    begin
      if (s[i] = '.') then
      begin
        str := RightStr(s, length(s) - i) + ' ' + str;
        s := LeftStr(s, i);
        break;
      end;
    end;

    if (RightStr(s, 2) = '..') then // 21... => 21.
      s := LeftStr(s, length(s) - 2);

    if (s <> '') and
       (not ((s[length(s)] = '.') and TryStrToInt(LeftStr(s, length(s) - 1), n) and
             (n = (n_ply shr 1) + 1))) and
       (s[1] <> '$') then
    begin
      strOriginalMove := s;

      s := StringReplace(s, 'O-O-O', '0-0-0', []);
      s := StringReplace(s, 'O-O', '0-0', []);
      s := StringReplace(s, 'x', '', []);
      s := StringReplace(s, '=', '', []);
//      s := StringReplace(s, '+', '', []);
//      s := StringReplace(s, '#', '', []);

      posMove.pos := m_ChessRulesEngine.Position^;
      if (m_ChessRulesEngine.DoMove(s)) then
      begin
        m_Tree.Add(m_ChessRulesEngine.NPlysDone, m_ChessRulesEngine.GetPosition,
          m_ChessRulesEngine.lastMoveStr);

        m_bInC4NFormat := (m_bInC4NFormat and (strOriginalMove = m_ChessRulesEngine.lastMoveStr));

        bkpMove := s;

        inc(n_ply);
//        inc(n_pos);

        lastInvalidMove := FALSE;

//        writeln(n_ply, 'p. ' + s + #9 + ChessBoard.GetPosition); // DEBUG:
      end
      else
      begin
//        writeln(s, ' n_pos: ', n_pos); // DEBUG:
//        Assert(not lastInvalidMove);
        if (lastInvalidMove) then
          raise EPGNParser.Create;

        lastInvalidMove := TRUE;
      end; // if (ChessRulesEngine.DoMove(s))

      bkpMove := s;
    end; { if (s <> '') ...}

    if (bTakebackLineFlag) then
      NTakeBackLine;

  end; // \NProceedInner

var
  str: string;
  i: integer;
begin // .FParseGameStr
//  inc(n_game);

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
  posMoveStack := nil;
  movePlyStack := TStack.Create;
  try
    moveEsts := TList.Create;
    posMoveStack := TStack.Create;

    n_ply := m_Tree.PlysOffset;
    bkpMove := '';
    lastInvalidMove := TRUE;
    addPos := TRUE;
//    addSimplePosMove := FALSE;

    str := strGame;
    repeat
      NProceedInner(str);
    until (str = '');

//    Assert(movePlyStack.Count = 0);
      if (movePlyStack.Count <> 0) then
        raise EPGNParser.Create;

  finally
    for i := 0 to moveEsts.Count - 1 do
      Dispose(moveEsts[i]);
    moveEsts.Free;

    while (posMoveStack.Count > 0) do
      Dispose(posMoveStack.Pop);
    posMoveStack.Free;

    movePlyStack.Free;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// EPGNParser

constructor EPGNParser.Create;
begin
  inherited Create('');
end;

end.
