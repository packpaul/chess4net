////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit PosBaseChessBoardLayerUnit;

interface

uses
  Classes,
  //
  ChessBoardUnit, PosBaseUnit, MoveTreeBaseUnit, ChessRulesEngine;


type
  TGameResult = (grWin, grWinTime, grDraw, grLost, grLostTime);

  // Layer extended with Position DB
  TPosBaseChessBoardLayer = class(TChessBoardLayerBase)
  private
    m_bTrainingMode: boolean;
    m_lstMovePrior: TList;
    m_bUseUserBase: boolean;
    m_PosBase, m_ExtPosBase: TPosBase;
    m_ExtMoveTreeBase: TMoveTreeBase;
    m_strPosBaseName, m_strExtPosBaseName: string;

    procedure FSetTrainingMode(bValue: boolean);
    procedure FSetUseUserBase(bValue: boolean);

    procedure FClearMovePriorList;
    procedure FReadFromBase(out lstMovePrior: TList); overload;
    procedure FReadFromBase; overload;
    procedure FWriteGameToBase;
    procedure FRecreateExtBases;
    procedure FFreeExtBases;
    procedure FFindInExtTreeBase(const pos: TChessPosition; var moveEsts: TList);

    property MovePrior: TList read m_lstMovePrior;

  protected
    procedure RDraw; override;
    procedure ROnAfterMoveDone; override;
    procedure ROnAfterSetPosition; override;
    procedure ROnAfterModeSet(const OldValue, NewValue: TMode); override;
    procedure ROnResetMoveList; override;
    procedure ROnInitPosition; override;
  public
    constructor Create(const strPosBaseName: string = '');
    destructor Destroy; override;
    procedure SetExternalBase(const strExtPosBaseName: string);
    procedure WriteGameToBase(AGameResult: TGameResult);
    procedure UnsetExternalBase;
    property TrainingMode: boolean read m_bTrainingMode write FSetTrainingMode;
    property UseUserBase: boolean read m_bUseUserBase write FSetUseUserBase;
  end;

implementation

uses
  Graphics, SysUtils, Windows,
  //
  ChessBoardHeaderUnit;

type
  TPrior = (mpNo, mpHigh, mpMid, mpLow);

  PMovePrior = ^TMovePrior;
  TMovePrior = record
    move: TMoveAbs;
    prior: TPrior;
  end;

  TPosBaseOperator = class(TThread)
  private
    m_Operation: (opRead, opWrite);
    m_Layer: TPosBaseChessBoardLayer;
    m_lstMovePrior: TList;
    constructor FCreateRead(ALayer: TPosBaseChessBoardLayer;
      vbFreeOnTerminate: boolean = TRUE);
    constructor FCreateWrite(ALayer: TPosBaseChessBoardLayer);
    procedure FDoUpdateLayer;
  protected
    procedure Execute; override;
  public
    class function CreateRead(ALayer: TPosBaseChessBoardLayer;
      vbFreeOnTerminate: boolean = TRUE): TPosBaseOperator;
    class function CreateWrite(ALayer: TPosBaseChessBoardLayer): TPosBaseOperator;
    procedure WaitFor;
  end;

var
  gameResult: TGameResult; // Not threadsafe
  gameID: word; // It's used for writing unique positions (not threadsafe)

const
  NUM_PRIORITIES = 3; // Maximal number of priorities
{$IFDEF RESTRICT_TRAINING_DB}
  MAX_PLY_TO_BASE = 60;
{$ELSE}
  MAX_PLY_TO_BASE = -1; // The whole game is saved to the DB
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////
// TPosBaseChessBoardLayer

constructor TPosBaseChessBoardLayer.Create(const strPosBaseName: string = '');
begin
  inherited Create;

  m_bUseUserBase := TRUE;
  m_strPosBaseName := strPosBaseName;

  m_lstMovePrior := TList.Create;
end;


destructor TPosBaseChessBoardLayer.Destroy;
begin
  FClearMovePriorList;
  m_lstMovePrior.Free;

  TrainingMode := FALSE;
  
  inherited;
end;


procedure TPosBaseChessBoardLayer.RDraw;
const
  ARROW_END_LENGTH = 10; // в пиксел€х
  ARROW_END_ANGLE = 15 * (Pi / 180); // угол концов стрелки
  ARROW_INDENT = 7;

  HIGH_ARROW_COLOR = clRed;
  HIGH_ARROW_WIDTH = 2;
  MID_ARROW_COLOR = clGreen; // clTeal;
  MID_ARROW_WIDTH = 2;
  LOW_ARROW_COLOR = clSkyBlue;
  LOW_ARROW_WIDTH = 2;
  NO_ARROW_COLOR = clBlue;
  NO_ARROW_WIDTH = 1;

var
  i, x0, y0, x, y: integer;
  xa, ya, ca, sa: double;
  move: TMoveAbs;
begin
  if (not (Assigned(ChessBoard) and Assigned(Canvas))) then
    exit;

  if (not (m_bTrainingMode and (ChessBoard.Mode in [mGame, mAnalyse]) and
      (ChessBoard.PlayerColor = ChessBoard.PositionColor))) then
    exit;

  Canvas.Pen.Style := psSolid;

  for i := 0 to m_lstMovePrior.Count - 1 do
  begin
    case PMovePrior(m_lstMovePrior[i]).prior of
      mpNo:
        begin
          Canvas.Pen.Color := NO_ARROW_COLOR;
          Canvas.Pen.Width := NO_ARROW_WIDTH;
        end;
      mpHigh:
        begin
          Canvas.Pen.Color := HIGH_ARROW_COLOR;
          Canvas.Pen.Width := HIGH_ARROW_WIDTH;
        end;
      mpMid:
        begin
          Canvas.Pen.Color := MID_ARROW_COLOR;
          Canvas.Pen.Width := MID_ARROW_WIDTH;
        end;
      mpLow:
        begin
          Canvas.Pen.Color := LOW_ARROW_COLOR;
          Canvas.Pen.Width := LOW_ARROW_WIDTH;
        end;
    end;

    move := PMovePrior(m_lstMovePrior[i]).move;

    if (not ChessBoard.Flipped) then
    begin
      x0 := CHB_X + SquareSize * (move.i0 - 1) + (SquareSize div 2);
      y0 := CHB_Y + SquareSize * (8 - move.j0) + (SquareSize div 2);
      x := CHB_X + SquareSize * (move.i - 1) + (SquareSize div 2);
      y := CHB_Y + SquareSize * (8 - move.j) + (SquareSize div 2);
    end
    else
    begin
      x0 := CHB_X + SquareSize * (8 - move.i0) + (SquareSize div 2);
      y0 := CHB_Y + SquareSize * (move.j0 - 1) + (SquareSize div 2);
      x := CHB_X + SquareSize * (8 - move.i) + (SquareSize div 2);
      y := CHB_Y + SquareSize * (move.j - 1) + (SquareSize div 2);
    end;

    // Draw an arrow
    ca := (x - x0) / sqrt(sqr(x - x0) + sqr(y - y0));
    sa := (y - y0) / sqrt(sqr(x - x0) + sqr(y - y0));
    x0 := x0 + Round(ARROW_INDENT * ca);
    y0 := y0 + Round(ARROW_INDENT * sa);
    x := x - Round(ARROW_INDENT * ca);
    y := y - Round(ARROW_INDENT * sa);

    Canvas.MoveTo(x0, y0);
    Canvas.LineTo(x, y);

    xa := x + (-ARROW_END_LENGTH * cos(ARROW_END_ANGLE)) * ca -
              (ARROW_END_LENGTH * sin(ARROW_END_ANGLE)) * sa;
    ya := y + (-ARROW_END_LENGTH * cos(ARROW_END_ANGLE)) * sa +
              (ARROW_END_LENGTH * sin(ARROW_END_ANGLE)) * ca;

    Canvas.LineTo(Round(xa), Round(ya));

    xa := x + (-ARROW_END_LENGTH * cos(ARROW_END_ANGLE)) * ca -
              (-ARROW_END_LENGTH * sin(ARROW_END_ANGLE)) * sa;
    ya := y + (-ARROW_END_LENGTH * cos(ARROW_END_ANGLE)) * sa +
              (-ARROW_END_LENGTH * sin(ARROW_END_ANGLE)) * ca;

    Canvas.MoveTo(x, y);
    Canvas.LineTo(Round(xa), Round(ya));
  end;

end;


procedure Reestimate(lstMoveEsts: TList; viRec: integer);
var
  est: SmallInt;
  id: word;
begin
  id := LongWord(lstMoveEsts[viRec]) shr 16;
  if id = gameID then
    exit; // позици€ дублируетс€ в рамках одной партии

  est := SmallInt(lstMoveEsts[viRec]);
  case gameResult of
    grWin:     inc(est, 2);
    grWinTime: inc(est);
    grDraw:       ;
    grLost:     dec(est, 2);
    grLostTime: dec(est);
  end;
  lstMoveEsts[viRec] := Pointer((gameID shl 16) or Word(est));
end;


procedure TPosBaseChessBoardLayer.FSetTrainingMode(bValue: boolean);
begin
  if (m_bTrainingMode = bValue) then
    exit;

  m_bTrainingMode := bValue;

  try
    if (m_bTrainingMode) then
    begin
      if (m_strPosBaseName <> '') then
        m_PosBase := TPosBase.Create(m_strPosBaseName, Reestimate);
      if (m_strExtPosBaseName <> '') then
        FRecreateExtBases;
      with TPosBaseOperator.CreateRead(self, FALSE) do
      try
        WaitFor;
      finally
        Free;
      end;
    end
    else
    begin
      FFreeExtBases;
      FreeAndNil(m_PosBase);
    end;

    RDoUpdate;

  except
    on Exception do
    begin
      FFreeExtBases;
      FreeAndNil(m_PosBase);

      m_bTrainingMode := FALSE;
    end;
  end;

end;


procedure TPosBaseChessBoardLayer.FRecreateExtBases;
begin
  FFreeExtBases;

  if (TMoveTreeBase.Exists(m_strExtPosBaseName)) then
    m_ExtMoveTreeBase := TMoveTreeBase.Create(m_strExtPosBaseName);

  if (TPosBase.Exists(m_strExtPosBaseName)) then
    m_ExtPosBase := TPosBase.Create(m_strExtPosBaseName, m_ExtMoveTreeBase);
end;


procedure TPosBaseChessBoardLayer.FFreeExtBases;
begin
  FreeAndNil(m_ExtPosBase);
  FreeAndNil(m_ExtMoveTreeBase);
end;


procedure TPosBaseChessBoardLayer.FFindInExtTreeBase(const pos: TChessPosition; var moveEsts: TList);
var
  Moves: TMoveAbsArr;
  i: integer;
  pme: PMoveEst;
begin
  if (not Assigned(m_ExtMoveTreeBase)) then
    exit;

  m_ExtMoveTreeBase.Find(Position^, Moves);
  for i := Low(Moves) to High(Moves) do
  begin
    pme := new (PMoveEst);
    pme.move := Moves[i];
    pme.estimate := 0;
    moveEsts.Add(pme);
  end;

end;



procedure TPosBaseChessBoardLayer.FSetUseUserBase(bValue: boolean);
begin
  if (m_bUseUserBase = bValue) then
    exit;
  m_bUseUserBase := bValue;
  TPosBaseOperator.CreateRead(self, TRUE);
end;


procedure TPosBaseChessBoardLayer.ROnAfterMoveDone;
begin
  if (m_bTrainingMode) then
  begin
    if (Assigned(ChessBoard) and
        (ChessBoard.PlayerColor = ChessBoard.PositionColor)) then
      TPosBaseOperator.CreateRead(self) // Read from PosBase and update
  end;
end;


procedure TPosBaseChessBoardLayer.ROnAfterSetPosition;
begin
  if (m_bTrainingMode) then
  begin
    with TPosBaseOperator.CreateRead(self, FALSE) do // Read from DB and update
    try
      WaitFor;
    finally
      Free;
    end;
  end;
end;


procedure TPosBaseChessBoardLayer.SetExternalBase(const strExtPosBaseName: string);
begin
  if (m_strExtPosBaseName = strExtPosBaseName) then
    exit;

  m_strExtPosBaseName := strExtPosBaseName;

  if (m_bTrainingMode) then
  begin
    FRecreateExtBases;

    TPosBaseOperator.CreateRead(self, TRUE);
  end;
end;


procedure TPosBaseChessBoardLayer.WriteGameToBase(AGameResult: TGameResult);
begin
  if (m_bTrainingMode) then
  begin
    gameResult := AGameResult;
    TPosBaseOperator.CreateWrite(self);
  end;
end;


procedure TPosBaseChessBoardLayer.UnsetExternalBase;
begin
  FFreeExtBases;
end;


procedure TPosBaseChessBoardLayer.ROnAfterModeSet(const OldValue, NewValue: TMode);
begin
  if (OldValue = mEdit) then
    ROnAfterSetPosition; // Read from PosBase
end;


procedure TPosBaseChessBoardLayer.ROnResetMoveList;
begin
  if (ChessBoard.Mode = mEdit) then
    FClearMovePriorList;
end;


procedure TPosBaseChessBoardLayer.ROnInitPosition;
begin
  if (Assigned(m_ExtMoveTreeBase)) then
    m_ExtMoveTreeBase.PosCache.Clear;
end;


procedure TPosBaseChessBoardLayer.FClearMovePriorList;
var
  i: integer;
begin
  for i := 0 to m_lstMovePrior.Count - 1 do
    Dispose(m_lstMovePrior[i]);
  m_lstMovePrior.Clear;
end;


function EstSignedComape(item1, item2: pointer): integer;
begin
  Result := SmallInt(PMoveEst(item2).estimate and $FFFF) - SmallInt(PMoveEst(item1).estimate and $FFFF);
end;


function EstUnsignedComape(item1, item2: pointer): integer;
begin
  Result := Word(PMoveEst(item2).estimate and $FFFF) - Word(PMoveEst(item1).estimate and $FFFF);
end;


function MoveLengthComape(item1, item2: pointer): integer;
var
  MoveLength1: integer;
  MoveLength2: integer;
begin
  with PMovePrior(item1).move do
    MoveLength1 := abs(i - i0) + abs(j - j0);

  with PMovePrior(item2).move do
    MoveLength2 := abs(i - i0) + abs(j - j0);

  Result := MoveLength2 - MoveLength1;
end;


procedure TPosBaseChessBoardLayer.FReadFromBase(out lstMovePrior: TList);

  procedure NClasterUsrMoves(var rlstMove: TList);
  var
    i, j, num_clast, i_min, j_min, curr_assoc: integer;
    modus_min: double;
    clastWeights: array of record
      grav: double;
      assoc: integer;
    end;
    mp: PMovePrior;
    p: TPrior;
  begin
    if (rlstMove.Count = 0) then
      exit;

    rlstMove.Sort(EstSignedComape);
    SetLength(clastWeights, rlstMove.Count);

    num_clast := rlstMove.Count;
    for i := 0 to num_clast - 1 do
    begin
      clastWeights[i].assoc := i + 1;
      clastWeights[i].grav := SmallInt(PMoveEst(rlstMove[i]).estimate and $FFFF);
    end;

    repeat
      i_min := 0;
      j_min := 0;
      modus_min := $7FFF; // $7FFF - макс. значение дл€ оценки
      curr_assoc := 0; // текущий просматриваемый кластер

      for i := 0 to length(clastWeights) - 2 do
      begin
        if (curr_assoc = clastWeights[i].assoc) then
          continue;

        curr_assoc := clastWeights[i].assoc;
        for j := i + 1 to length(clastWeights) - 1 do
          if ((clastWeights[j].assoc <> clastWeights[j - 1].assoc) and
              (curr_assoc <> clastWeights[j].assoc) and
              (abs(clastWeights[i].grav - clastWeights[j].grav) <= modus_min)) then
          begin
            i_min := i;
            j_min := j;
            modus_min := abs(clastWeights[i].grav - clastWeights[j].grav);
          end;
       end;

       if ((num_clast > Ord(High(TPrior))) or (modus_min = 0.0)) then
       begin
         for i := High(clastWeights) downto j_min do
           if clastWeights[i].assoc = clastWeights[j_min].assoc then
             clastWeights[i].assoc := clastWeights[i_min].assoc;
         clastWeights[i_min].grav := (clastWeights[i_min].grav + clastWeights[j_min].grav) / 2;
       end;

       dec(num_clast);
    until (num_clast <= Ord(High(TPrior))) and ((modus_min <> 0.0) or (num_clast < 1));

    p := mpHigh;
    for i := 0 to rlstMove.Count - 1 do
    begin
      new(mp);
      if ((i > 0) and (clastWeights[i].assoc > clastWeights[i - 1].assoc)) then
        p := Succ(p);
      mp.move := PMoveEst(rlstMove[i]).move;
      mp.prior := p;
      Dispose(rlstMove[i]);
      rlstMove[i] := mp;
    end;
  end;

  procedure NClasterExtMoves(var rlstMove: TList);
  var
    i, j, num_clast, i_min, j_min, curr_assoc: integer;
    iEst: integer;
    modus_min: double;
    clastWeights: array of record
      grav: double;
      assoc: integer;
      iIndex: integer;
    end;
    mp: PMovePrior;
    p: TPrior;
  begin
    if (rlstMove.Count = 0) then
      exit;

    rlstMove.Sort(EstUnsignedComape);

    SetLength(clastWeights, rlstMove.Count);
    num_clast := 0;
    for i := 0 to rlstMove.Count - 1 do
    begin
      iEst := Word(PMoveEst(rlstMove[i]).estimate and $FFFF);
      if (iEst = 0) then
      begin
        new(mp);
        mp.move := PMoveEst(rlstMove[i]).move;
        mp.prior := mpNo;
        Dispose(rlstMove[i]);
        rlstMove[i] := mp;
      end
      else
      begin
        clastWeights[i].assoc := i + 1;
        clastWeights[i].grav := iEst;
        clastWeights[i].iIndex := i;
        inc(num_clast);
      end;
    end;
    SetLength(clastWeights, num_clast);

    repeat
      i_min := 0;
      j_min := 0;
      modus_min := $FFFF; // $FFFF - max. value for evaluation
      curr_assoc := 0; // current cluster being viewed

      for i := Low(clastWeights) to Pred(High(clastWeights)) do
      begin
        if (curr_assoc = clastWeights[i].assoc) then
          continue;

        curr_assoc := clastWeights[i].assoc;

        for j := Succ(i) to High(clastWeights) do
        begin
          if ((clastWeights[j].assoc <> clastWeights[Pred(j)].assoc) and
              (curr_assoc <> clastWeights[j].assoc) and
              (abs(clastWeights[i].grav - clastWeights[j].grav) <= modus_min)) then
          begin
            i_min := i;
            j_min := j;
            modus_min := abs(clastWeights[i].grav - clastWeights[j].grav);
          end;
        end;
      end;

      if ((num_clast > Ord(High(TPrior))) or (modus_min = 0.0)) then
      begin
        for i := High(clastWeights) downto j_min do
        begin
          if clastWeights[i].assoc = clastWeights[j_min].assoc then
             clastWeights[i].assoc := clastWeights[i_min].assoc;
        end;
        clastWeights[i_min].grav := (clastWeights[i_min].grav + clastWeights[j_min].grav) / 2;
      end;

      dec(num_clast);
      
    until (num_clast <= Ord(High(TPrior))) and ((modus_min <> 0.0) or (num_clast < 1));

    p := mpHigh;
    for i := Low(clastWeights) to High(clastWeights) do
    begin
      new(mp);
      if ((i > Low(clastWeights)) and (clastWeights[i].assoc > clastWeights[Pred(i)].assoc)) then
        p := Succ(p);
      j := clastWeights[i].iIndex;
      mp.move := PMoveEst(rlstMove[j]).move;
      mp.prior := p;
      Dispose(rlstMove[j]);
      rlstMove[j] := mp;
    end;

  end;

  procedure NClasterTreeMoves(var rlstMove: TList);
  var
    i: integer;
    mp: PMovePrior;
  begin
    for i := 0 to rlstMove.Count - 1 do
    begin
      new(mp);
      mp.move := PMoveEst(rlstMove[i]).move;
      mp.prior := mpNo;
      Dispose(rlstMove[i]);
      rlstMove[i] := mp;
    end;
  end;

  procedure NMergeMoves(var lstMove: TList); overload;
  var
    i, j, n: integer;
  const
    PRIOR_CALC: array[TPrior, TPrior] of TPrior =
      ((mpNo, mpNo, mpNo, mpNo),        // UsrPrior = mpNo - ?, т.к. ещЄ нигде не исп.
       (mpHigh, mpHigh, mpHigh, mpMid), // UsrPrior = mpHigh
       (mpMid, mpMid, mpMid, mpMid),    // UsrPrior = mpMid
       (mpLow, mpMid, mpLow, mpLow));   // UsrPrior = mpLow
  begin
    n := lstMovePrior.Count;

    for i := 0 to lstMove.Count - 1 do
    begin
      j := n - 1;
      while (j >= 0) do
      begin
        if (PMovePrior(lstMove[i]).move.Equals(PMovePrior(lstMovePrior[j]).move)) then
          begin
            PMovePrior(lstMovePrior[j]).prior :=
              PRIOR_CALC[PMovePrior(lstMovePrior[j]).prior,
                         PMovePrior(lstMove[i]).prior];
            Dispose(lstMove[i]);
            break;
          end;
        dec(j);
      end;
      if (j < 0) then
        lstMovePrior.Add(lstMove[i]);
    end; // for
  end;

var
  lstUsrMove, lstExtMove, lstExtMove2: TList;

  procedure NMergeMoves; overload;
  begin
    NMergeMoves(lstUsrMove);
    NMergeMoves(lstExtMove);
    NMergeMoves(lstExtMove2);
  end;

begin // .FReadFromBase
  lstMovePrior := TList.Create;

  if (not Assigned(Position)) then
    exit;

  lstExtMove := nil;
  lstExtMove2 := nil;
  lstUsrMove := TList.Create;
  try
    lstExtMove := TList.Create;
    lstExtMove2 := TList.Create;

    if (m_bUseUserBase or (not Assigned(m_ExtPosBase))) then
    begin
      if (Assigned(m_PosBase)) then
        m_PosBase.Find(Position^, lstUsrMove);
    end;
    if (Assigned(m_ExtPosBase)) then
      m_ExtPosBase.Find(Position^, lstExtMove);

    FFindInExtTreeBase(Position^, lstExtMove2);

    // TODO: Handle wrong DB

    NClasterUsrMoves(lstUsrMove);
    NClasterExtMoves(lstExtMove);
    NClasterTreeMoves(lstExtMove2);

    NMergeMoves;

    lstMovePrior.Sort(MoveLengthComape);

  finally
    lstExtMove2.Free;
    lstExtMove.Free;
    lstUsrMove.Free;
  end;
  
end;


procedure TPosBaseChessBoardLayer.FReadFromBase;
var
  lstMovePrior: TList;
begin
  lstMovePrior := nil;

  FClearMovePriorList;
  try
    FReadFromBase(lstMovePrior);
    m_lstMovePrior.Assign(lstMovePrior);
  finally
    lstMovePrior.Free;
  end;
end;


procedure TPosBaseChessBoardLayer.FWriteGameToBase;
var
  ply: integer;
begin
  if (not (Assigned(m_PosBase) and Assigned(PositionsList))) then
    exit;

  gameID := Random($FFFF) + 1;

  if (ChessBoard.PlayerColor = RGetColorStarts) then
    ply := 0
  else
    ply := 1;

  while ((ply < PositionsList.Count) and ((MAX_PLY_TO_BASE < 0) or (ply <= MAX_PLY_TO_BASE))) do
  begin
    m_PosBase.Add(PPosMove(PositionsList[ply])^);
    inc(ply, 2);
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TPosBaseOperator

constructor TPosBaseOperator.FCreateRead(ALayer: TPosBaseChessBoardLayer;
  vbFreeOnTerminate: boolean = TRUE);
begin
  m_Operation := opRead;
  m_Layer := ALayer;

  inherited Create(TRUE);
  Priority := tpNormal;
  FreeOnTerminate := vbFreeOnTerminate;
  Resume;
end;


constructor TPosBaseOperator.FCreateWrite(ALayer: TPosBaseChessBoardLayer);
begin
  m_Layer := ALayer;
  m_Operation := opWrite;

  inherited Create(TRUE);

  Priority := tpNormal;
  FreeOnTerminate := TRUE;

  Resume;
end;


class function TPosBaseOperator.CreateRead(ALayer: TPosBaseChessBoardLayer;
  vbFreeOnTerminate: boolean = TRUE): TPosBaseOperator;
begin
  Result := nil;

  if (Assigned(ALayer.ChessBoard) and (ALayer.ChessBoard.Mode <> mEdit)) then
  begin
    ALayer.FClearMovePriorList;
    Result := TPosBaseOperator.FCreateRead(ALayer, vbFreeOnTerminate);
  end;
end;


class function TPosBaseOperator.CreateWrite(ALayer: TPosBaseChessBoardLayer): TPosBaseOperator;
begin
  Result := nil;

  if (Assigned(ALayer.ChessBoard) and (ALayer.ChessBoard.Mode <> mEdit)) then
    Result := TPosBaseOperator.FCreateWrite(ALayer);
end;


procedure TPosBaseOperator.Execute;
begin
  case m_Operation of
    opRead:
    begin
      try
        m_Layer.FReadFromBase(m_lstMovePrior);
        Synchronize(FDoUpdateLayer);
      finally
        FreeAndNil(m_lstMovePrior);
      end;
    end;
    opWrite:
      m_Layer.FWriteGameToBase;
  end;
end;


procedure TPosBaseOperator.FDoUpdateLayer;
begin
  m_Layer.MovePrior.Assign(m_lstMovePrior);
  m_Layer.RDoUpdate;
end;


procedure TPosBaseOperator.WaitFor;
begin
  if (not Assigned(self)) then
    exit;
  inherited WaitFor;
end;

initialization
  Randomize;

end.
