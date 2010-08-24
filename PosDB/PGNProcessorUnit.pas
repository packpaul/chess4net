unit PGNProcessorUnit;

interface

uses
  Classes,
  //
  ChessRulesEngine, PosBaseUnit;

type
  TFigureColors = set of TFigureColor;
  TOpening = (openNo, openNormal, openExtended, openExtendedPlus);

  TPGNProcessor = class
  private
    ChessRulesEngine: TChessRulesEngine;
    PosBase, RefPosBase: TPosBase;
    incVariants, useUniquePos, useStatPrunning: boolean;
    genOpening: TOpening;
    procColor: TFigureColors;
    useNumPlys: integer;
    n_pos: integer;
    n_game: integer;

    constructor FCreate;
    procedure FProceedGameStr(const strGame: string);
    procedure FReestimate(moveEsts: TList; nRec: integer);
    procedure FProceedPGN(const basename: string; variants, chngest: boolean; uniquePos: boolean;
      const color: TFigureColors; numPlys: integer; const player_name: string;
      opening: TOpening; statPrunning: boolean; refBaseName: string);

  public
    constructor Create;
    destructor Destroy; override;
    class procedure Proceed(const basename: string; variants, chngest: boolean; uniquePos: boolean;
                            const color: TFigureColors; numPlys: integer; const player_name: string;
                            opening: TOpening; statPrunning: boolean; refBaseName: string);
  end;

implementation

uses
  SysUtils, StrUtils, Contnrs;

var
  g_PGNProcessor: TPGNProcessor = nil;

procedure Reestimate(moveEsts: TList; nRec: integer);
begin
  if (Assigned(g_PGNProcessor)) then
    g_PGNProcessor.FReestimate(moveEsts, nRec); 
end;

////////////////////////////////////////////////////////////////////////////////
// TPGNProcessor

constructor TPGNProcessor.Create;
begin
  raise Exception.Create('TPGNProcessor cannot be instantiated directly!');
end;


constructor TPGNProcessor.FCreate;
begin
  inherited Create;
  ChessRulesEngine := TChessRulesEngine.Create(nil);

  g_PGNProcessor := self;
end;


destructor TPGNProcessor.Destroy;
begin
  g_PGNProcessor := nil;
  ChessRulesEngine.Free; 
  inherited;
end;


class procedure TPGNProcessor.Proceed(const basename: string; variants, chngest: boolean; uniquePos: boolean;
  const color: TFigureColors; numPlys: integer; const player_name: string;
  opening: TOpening; statPrunning: boolean; refBaseName: string);
begin
  with TPGNProcessor.FCreate do
  try
    FProceedPGN(basename, variants, chngest, uniquePos, color, numPlys, player_name,
      opening, statPrunning, refBaseName);
  finally
    Free;
  end;
end;


procedure TPGNProcessor.FProceedPGN(const basename: string; variants, chngest: boolean; uniquePos: boolean;
  const color: TFigureColors; numPlys: integer; const player_name: string;
  opening: TOpening; statPrunning: boolean; refBaseName: string);
var
  s, ss: string;
  playerExists: boolean;
begin
  incVariants := variants;
  genOpening := opening;
  useStatPrunning := statPrunning;
  useNumPlys := numPlys;

  if (chngest) then
  begin
    useUniquePos := uniquePos;
    PosBase := TPosBase.Create(basename, Reestimate);
  end
  else
    PosBase := TPosBase.Create(basename);

  if (refBaseName <> '') then
    RefPosBase := TPosBase.Create(refBaseName);

  ss := '';
  procColor := color;
  playerExists := (player_name = '');
  repeat
    readln(input, s);

    if (s <> '') and ((s[1] <> '[') or (s[length(s)] <> ']')) then
      begin
        ss := ss + ' ' + s;
        if not eof(input) then
          continue;
      end;
    if ss <> '' then
      begin
        if (procColor <> []) and playerExists then
          FProceedGameStr(ss);
        ss := '';
        procColor := color;
        playerExists := (player_name = '');
      end;
    if player_name <> '' then
      begin
        if s = ('[White "' + player_name + '"]') then
          begin
            procColor := color * [fcWhite];
            playerExists := TRUE;
          end
        else
        if s = ('[Black "' + player_name + '"]') then
          begin
            procColor := color * [fcBlack];
            playerExists := TRUE;
          end;
      end;
  until eof(input);

  RefPosBase.Free;
  PosBase.Free;
  writeln('Games viewed: ', n_game);
  writeln('Positions viewed: ', n_pos);
end;


procedure TPGNProcessor.FProceedGameStr(const strGame: string);
var
  n_ply: integer;
  bkpMove: string;
  movePlyStack, posMoveStack: TStack;
  moveEsts: TList;
  lastInvalidMove: boolean;
  addPos: boolean;
  addSimplePosMove: boolean;
  SimplePosMove: TPosMove;

  procedure NProceedInner(var str: string);

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
      Assert(FALSE);

      Result := FALSE;
    end; // \NCutOutComments

    type
      PMovePly = ^TMovePly;
      TMovePly = record
        move: string;
        ply: integer;
        addPos: boolean; // см. genOpening
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

      if (not incVariants) then
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

        Assert(FALSE);
      end; // if (not incVariants)

      New(movePly);
      movePly.move := bkpMove;
      movePly.ply := n_ply;
      movePly.addPos := addPos;
      movePly.posMoveCount := posMoveStack.Count;

      if (not lastInvalidMove) then
      begin
        movePly.ply := n_ply;
        self.ChessRulesEngine.TakeBack;
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
        ChessRulesEngine.TakeBack;
        dec(n_ply);
      end;

      if ChessRulesEngine.DoMove(movePly.move) then
      begin
        inc(n_ply);
        addPos := (genOpening = openNo) or PosBase.Find(ChessRulesEngine.Position^); // Opening
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
    s: string;
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
      s := StringReplace(s, 'O-O-O', '0-0-0', []);
      s := StringReplace(s, 'O-O', '0-0', []);
      s := StringReplace(s, 'x', '', []);
      s := StringReplace(s, '+', '', []);
      s := StringReplace(s, '#', '', []);
      s := StringReplace(s, '=', '', []);

      posMove.pos := ChessRulesEngine.Position^;
      if (ChessRulesEngine.DoMove(s)) then
      begin
        bkpMove := s;

        inc(n_ply);
        inc(n_pos);

        if (posMove.pos.color in procColor) then
        begin
          posMove.move := ChessRulesEngine.lastMove^;

          if (genOpening <> openNo) then
            NProcessOpeningLine(posMove);

          addPos := (addPos and ((useNumPlys = 0) or (n_ply <= useNumPlys)));

          if (addPos) then
            PosBase.Add(posMove);

          if (genOpening in [openExtended, openExtendedPlus]) then
            NProcessExtendedOpeningLine(posMove);

        end; { if posMove.pos.color ... }

        lastInvalidMove := FALSE;

//        writeln(n_ply, 'p. ' + s + #9 + ChessBoard.GetPosition); // DEBUG:
      end
      else
      begin
//        writeln(s, ' n_pos: ', n_pos); // DEBUG:
        Assert(not lastInvalidMove);
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
begin // TPGNProcessor.FProceedGameStr
  inc(n_game);

  ChessRulesEngine.InitNewGame;

  moveEsts := nil;
  posMoveStack := nil;
  movePlyStack := TStack.Create;
  try
    moveEsts := TList.Create;
    posMoveStack := TStack.Create;

    n_ply := 0;
    bkpMove := '';
    lastInvalidMove := TRUE;
    addPos := TRUE;
    addSimplePosMove := FALSE;

    str := strGame;
    repeat
      NProceedInner(str);
    until (str = '');

    if (addSimplePosMove) then
      PosBase.Add(SimplePosMove);

    Assert(movePlyStack.Count = 0);

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


procedure TPGNProcessor.FReestimate(moveEsts: TList; nRec: integer);
var
  est : LongWord;
begin
  // Re-estimation is done here:
  // Re-estimation for a DB with GMs games
  est := LongWord(moveEsts[nRec]);
  if ((est and $FFFF) < $FFFF) then
    est := est + 1;
  // For statistical estimation: if position in bounds of one game comes more than one time -> don't change estimation
  if (useUniquePos) then
  begin
    if ((est shr 16) >= n_game) then // exclude repitition of "position + moves" in one game
      exit;
    est := (n_game shl 16) or (est and $FFFF);
  end
  else
    est := est and $FFFF;

  moveEsts[nRec] := Pointer(est);
end;

end.
