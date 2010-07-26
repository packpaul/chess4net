unit PGNProcessorUnit;

interface

uses
  Classes,
  //
  ChessRulesEngine, PosBaseUnit;

type
  TFigureColors = set of TFigureColor;
  TOpening = (openNo, openNormal, openExtended);

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
    procedure FProceedGameStr(str: string);
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
  if chngest then
    begin
      useUniquePos := uniquePos;
      PosBase := TPosBase.Create(basename, Reestimate);
    end
  else
    PosBase := TPosBase.Create(basename);
  if refBaseName <> '' then
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


procedure TPGNProcessor.FProceedGameStr(str: string);

  function NextWord : string;
  var
    l : integer;
  begin
    str := TrimLeft(str);
    l := pos(' ', str);
    if l = 0 then
      begin
        Result := str;
        str := '';
      end
    else
      begin
        Result := LeftStr(str, l-1);
        str := RightStr(str, length(str) - l);
      end;
  end;

type
  PMovePly = ^TMovePly;
  TMovePly = record
    move: string;
    ply: integer;
    addPos: boolean; // см. genOpening
    posMoveCount: integer;
  end;

var
  s: string;
  i, n, n_ply: integer;
  bkpMove: string;
  movePly: PMovePly;
  posMove: TPosMove;
  p_posMove: PPosMove;
  movePlyStack, posMoveStack: TStack;
  moveEsts: TList;
  takebackLine, lastInvalidMove: boolean;
  addPos: boolean;
label
  here;
begin
  ChessRulesEngine.InitNewGame;
  n_ply := 0;
  bkpMove := '';
  movePlyStack := TStack.Create;
  moveEsts := TList.Create;
  posMoveStack := TStack.Create;
  lastInvalidMove := TRUE;
  addPos := TRUE;
  inc(n_game);

  repeat
here:
    s := NextWord;
    if (s = '') or (s = '*') or (s = '1-0') or (s = '0-1') or (s = '1/2-1/2') then
      break;

    if s[1] = '{' then // вырезает комментарии
      begin
        str := s + ' ' + str;
        for i := 1 to length(str) do
          if str[i] = '}' then
            begin
              str := RightStr(str, length(str) - i);
              goto here;
            end;
//       writeln(' n_pos: ', n_pos);
         Assert(FALSE);
       end;

    if s[1] = '(' then // начало варианта
      begin
        if (length(s) > 1) and (s[2] = '{') then
          begin
            str := '( {' + str;
            goto here;
          end;
        if not incVariants then
          begin
            n := 1;
            i := 1;
            repeat
              case str[i] of
                '{':
                  repeat
                    inc(i);
                  until (i > length(str)) or (str[i] = '}');
                '(':
                  inc(n);
                ')':
                  begin
                    dec(n);
                    if n = 0 then
                      begin
                        str := RightStr(str, length(str) - i);
                        goto here;
                      end;
                  end;
              end; { case }
              inc(i);
            until i > length(str);
            Assert(FALSE);
          end;
        new(movePly);
        movePly.move := bkpMove;
        movePly.ply := n_ply;
        movePly.addPos := addPos;
        movePly.posMoveCount := posMoveStack.Count;
        if not lastInvalidMove then
          begin
            movePly.ply := n_ply;
            self.ChessRulesEngine.TakeBack;
            dec(n_ply);
          end
        else
          movePly.ply := n_ply + 1;
        movePlyStack.Push(movePly);

        s := RightStr(s, length(s)-1);
        if s = '' then
          goto here;
      end;

    if s = ')' then
    begin
      takebackLine := TRUE;
      s := '';
    end
    else
      begin
        takebackLine := FALSE;
        while (s <> '') and (s[length(s)] = ')') do
          begin
            str := ') ' + str;
            s := LeftStr(s, length(s)-1);
          end;
      end;

    for i := length(s) downto 1 do
      if s[i] = '.' then
        begin
          str := RightStr(s, length(s) - i) + ' ' + str;
          s := LeftStr(s, i);
          break;
        end;

    if RightStr(s, 2) = '..' then // 21... => 21.
      s := LeftStr(s, length(s)-2);
    if (s <> '') and
       (not ((s[length(s)] = '.') and TryStrToInt(LeftStr(s, length(s)-1), n) and
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
      if ChessRulesEngine.DoMove(s) then
      begin
        bkpMove := s;
        inc(n_ply);
        inc(n_pos);

        if posMove.pos.color in procColor then
          begin
            posMove.move := ChessRulesEngine.lastMove^;

            if genOpening <> openNo then
            begin
              if Assigned(RefPosBase) then
                addPos := RefPosBase.Find(posMove.pos, moveEsts)
              else
                addPos := PosBase.Find(posMove.pos, moveEsts);
              if addPos then
                begin
                  i := moveEsts.Count - 1;
                  while (i >= 0) do
                    begin
                      with PMoveEst(moveEsts[i]).move, posMove do
                        addPos := (i0 = move.i0) and (j0 = move.j0) and (i = move.i) and
                                  (j = move.j) and (prom_fig = move.prom_fig);
                      if addPos then
                        begin
                          if useStatPrunning then
                            addPos := ((PMoveEst(moveEsts[i]).estimate and $FFFF) > 1);
                          if addPos then
                            break;
                        end;
                      dec(i);
                    end;
                  if i < 0 then
                    addPos := FALSE;
                end;
            end; { if genOpening <> openNo }

            addPos := addPos and ((useNumPlys = 0) or (n_ply <= useNumPlys));

            if addPos then
              begin
                PosBase.Add(posMove);
                if genOpening = openExtended then // добавление предыдущих позиций, до этого не вошедших в базу
                  while (posMoveStack.Count > 0) do
                    begin
                      PosBase.Add(PPosMove(posMoveStack.Peek)^);
                      dispose(posMoveStack.Pop);
                    end;
              end
            else
              begin
                if genOpening = openExtended then
                  begin
                    new(p_posMove);
                    p_posMove^ := posMove;
                    posMoveStack.Push(p_posMove);
                  end;
              end; { if addPos ... }
          end; { if posMove.pos.color ... }

        lastInvalidMove := FALSE;

//        writeln(n_ply, 'p. ' + s + #9 + ChessBoard.GetPosition);
      end
      else
      begin
//        writeln(s, ' n_pos: ', n_pos);
        Assert(not lastInvalidMove);
        lastInvalidMove := TRUE;
      end;
      bkpMove := s;
    end; { if not ... }

    if takebackLine then
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
            addPos := (genOpening = openNo) or PosBase.Find(ChessRulesEngine.Position^); // дебют
          end
        else
          begin
            lastInvalidMove := TRUE;
            addPos := movePly.addPos; // дебют
          end;

        n := movePly.posMoveCount;
        while (n < posMoveStack.Count) do // удаление стека подварианта
          dispose(posMoveStack.Pop);

        bkpMove := movePly.move;

        dispose(movePly);
      end;
  until str = '';

  for i := 1 to moveEsts.Count - 1 do
    dispose(moveEsts[i]);
  moveEsts.Free;  
  while (posMoveStack.Count > 0) do
    dispose(posMoveStack.Pop);
  Assert(movePlyStack.Count = 0);
  movePlyStack.Free;
end;


procedure TPGNProcessor.FReestimate(moveEsts: TList; nRec: integer);
var
  est : LongWord;
begin
  // переоценка выполн€етс€ здесь:
  // переоценка дл€ базы партий гроссмейстеров
  est := (LongWord(moveEsts[nRec]) + 1);
  // дл€ статистической оценки: если позици€ в рамках одной игры возникает > 1 раз -> оценку не мен€ть
  if useUniquePos then
    begin
      if (est shr 16) >= n_game then // предотвращение включени€ повтор€ющихс€ в одной игре позиций+ходов
        exit;
      est := (n_game shl 16) or (est and $FFFF);
    end
  else
    est := est and $FFFF;
  moveEsts[nRec] := Pointer(est);
end;

end.
