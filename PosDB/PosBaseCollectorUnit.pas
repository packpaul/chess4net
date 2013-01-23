////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit PosBaseCollectorUnit;

interface

uses
  Contnrs, Classes, SysUtils,
  //
  NonRefInterfacedObjectUnit, PGNTraverserUnit, PosBaseUnit, ChessRulesEngine,
  MoveTreeBaseUnit;

type
  TOpening = (openNo, openNormal, openExtended, openExtendedPlus);

  EPosBaseCollector = class(Exception);

  TPosBaseCollector = class(TNonRefInterfacedObject, IPGNTraverserVisitable)
  private
    m_PosMovesCollected: TObjectList;

    m_ExtendablePosMoves: TStack;
    m_Contexts: TStack;

    m_PosBase, m_RefPosBase: TPosBase;
    m_MoveTreeBase: TMoveTreeBase;

    m_strPosBaseName: string;
    m_ProceedColors: TFigureColors;
    m_ProceedColorsInner: TFigureColors;
    m_strPlayerName: string;
    m_bChangeEstimation: boolean;
    m_bUseUniquePositions: boolean;
    m_bUseStatisticalPrunning: boolean;
    m_iUseNumberOfPlys: integer;
    m_bUniqueGames: boolean;

    m_bAddPosMove: boolean;
    m_bAddPos: boolean;

    m_bAddSimplePosMove: boolean;
    m_SimplePosMove: TPosMove;

    m_bAddedToMoveTreeBase: boolean;

    m_GenOpening: TOpening;
    m_bExcludeLoopsInExtensions: boolean;

    m_bLineStartedFromPreviousPosition: boolean;
    m_lastPosMove: TPosMove;
    m_lastResultingPos: TChessPosition;
    m_iGameNumber: integer;

    // For tests
    m_PosBaseForTest: TPosBase;
    m_bSomeGamesSkippedForTest: boolean;

    constructor FCreate(const ARefPosBase: TPosBase);

    procedure FCreatePosBase;
    procedure FDestroyPosBase;

    function FGetMoveTreeBase: TMoveTreeBase;
    procedure FSetMoveTreeBase(ABase: TMoveTreeBase);

    procedure FClearPosMoves;
    procedure FClearContexts;

    procedure FProcessExtendedOpeningLine(const posMove: TPosMove);
    procedure FExcludeLoopsInExtensions;
    procedure FProcessOpeningLine(const posMove: TPosMove);
    procedure FReestimate(moveEsts: TList; nRec: integer);

    procedure FCollectPosMove(const APosMove: TPosMove);
    procedure FCollectPos(const APos: TChessPosition);

    procedure FAddCollectedPosMovesToBase;

    procedure FOnMoveTreeBaseAdded(Sender: TObject);

  protected
    constructor CreateForTest(const ADataBase: TPosBase; const ARefPosBase: TPosBase = nil);
    property SomeGamesSkippedForTest: boolean read m_bSomeGamesSkippedForTest;

  public
    constructor Create(const strPosBaseName: string; const ARefPosBase: TPosBase = nil);
    destructor Destroy; override;

    procedure Start(const Visitor: IPGNTraverserVisitor);
    procedure DoPosMove(iPlyNumber: integer; const APosMove: TPosMove; const AResultingPos: TChessPosition);
    procedure StartLine(bFromPreviousPos: boolean);
    procedure EndLine;
    procedure Finish;

    property PosBaseName: string read m_strPosBaseName;
    property MoveTreeBase: TMoveTreeBase read FGetMoveTreeBase write FSetMoveTreeBase;

    property ProceedColors: TFigureColors read m_ProceedColors write m_ProceedColors;
    property PlayerName: string read m_strPlayerName write m_strPlayerName;
    property ChangeEstimation: boolean read m_bChangeEstimation write m_bChangeEstimation;
    property UseUniquePositions: boolean read m_bUseUniquePositions write m_bUseUniquePositions;
    property GeneratedOpening: TOpening read m_GenOpening write m_GenOpening;
    property ExcludeLoopsInExtensions: boolean read m_bExcludeLoopsInExtensions write m_bExcludeLoopsInExtensions;
    property UseStatisticalPrunning: boolean read m_bUseStatisticalPrunning write m_bUseStatisticalPrunning;
    property UseNumberOfPlys: integer read m_iUseNumberOfPlys write m_iUseNumberOfPlys;
    property UniqueGames: boolean read m_bUniqueGames write m_bUniqueGames;
  end;

  PPosBaseCollectorContext = ^TPosBaseCollectorContext;
  TPosBaseCollectorContext = record
    bAddPos: boolean;
    iPosMovesCount: integer;
    PosMove: TPosMove;
    ResultingPos: TChessPosition;
  end;

procedure Reestimate(moveEsts: TList; nRec: integer);

implementation

uses
  LoggerUnit;

type
  TPosMoveItem = class
  private
    m_PosMove: TPosMove;
    m_bPosOnly: boolean;
    function FGetPos: TChessPosition;
  public
    constructor Create(const APosMove: TPosMove); overload;
    constructor Create(const APos: TChessPosition); overload;
    property PosMove: TPosMove read m_PosMove;
    property Pos: TChessPosition read FGetPos;
    property PosOnly: boolean read m_bPosOnly;
  end;

  TStackEx = class(TStack);

var
  g_PosBaseCollector: TPosBaseCollector = nil;

procedure Reestimate(moveEsts: TList; nRec: integer);
begin
  if (Assigned(g_PosBaseCollector)) then
    g_PosBaseCollector.FReestimate(moveEsts, nRec);
end;

////////////////////////////////////////////////////////////////////////////////
// TPosBaseCollector

constructor TPosBaseCollector.FCreate(const ARefPosBase: TPosBase);
begin
  m_PosMovesCollected := TObjectList.Create;

  m_ProceedColors := [fcWhite, fcBlack];
  m_GenOpening := openNo;
  m_RefPosBase := ARefPosBase;

  m_ExtendablePosMoves := TStack.Create;
  m_Contexts := TStack.Create;

  g_PosBaseCollector := self;
end;


constructor TPosBaseCollector.Create(const strPosBaseName: string;
  const ARefPosBase: TPosBase = nil);
begin
  inherited Create;

  m_strPosBaseName := strPosBaseName;

  FCreate(ARefPosBase);
end;


constructor TPosBaseCollector.CreateForTest(const ADataBase: TPosBase;
  const ARefPosBase: TPosBase = nil);
begin
  inherited Create;

  m_PosBaseForTest := ADataBase;
  
  FCreate(ARefPosBase);
end;


destructor TPosBaseCollector.Destroy;
begin
  g_PosBaseCollector := nil;

  FDestroyPosBase;

  FClearPosMoves;
  m_ExtendablePosMoves.Free;

  FClearContexts;
  m_Contexts.Free;

  m_PosMovesCollected.Free;

  inherited;
end;


function TPosBaseCollector.FGetMoveTreeBase: TMoveTreeBase;
begin
  Result := m_MoveTreeBase;
end;


procedure TPosBaseCollector.FSetMoveTreeBase(ABase: TMoveTreeBase);
begin
  if (Assigned(m_PosBase)) then
    raise EPosBaseCollector.Create('MoveTreeBase cannot after Start() has been invoked!');

  m_MoveTreeBase := ABase;

  if (Assigned(m_MoveTreeBase)) then
    m_MoveTreeBase.OnAdded := FOnMoveTreeBaseAdded;
end;


procedure TPosBaseCollector.FOnMoveTreeBaseAdded(Sender: TObject);
begin
  m_bAddedToMoveTreeBase := TRUE;
end;


procedure TPosBaseCollector.DoPosMove(iPlyNumber: integer; const APosMove: TPosMove;
  const AResultingPos: TChessPosition);
var
  bInMovesRange: boolean;
begin // .DoPosMove
  m_lastPosMove := APosMove;
  m_lastResultingPos := AResultingPos;

  if (not (APosMove.pos.color in m_ProceedColorsInner)) then
    exit;

  if (m_GenOpening <> openNo) then
    FProcessOpeningLine(APosMove);

  bInMovesRange := ((m_iUseNumberOfPlys = 0) or (iPlyNumber <= m_iUseNumberOfPlys));
  m_bAddPosMove := (m_bAddPosMove and bInMovesRange);
  m_bAddPos := (m_bAddPos and bInMovesRange);

  if (m_bAddPosMove) then
    FCollectPosMove(APosMove)
  else if (m_bAddPos) then
    FCollectPos(APosMove.pos);

  if (not bInMovesRange) then
    exit;

  if (m_GenOpening in [openExtended, openExtendedPlus]) then
    FProcessExtendedOpeningLine(APosMove);
end;


procedure TPosBaseCollector.FCollectPosMove(const APosMove: TPosMove);
begin
  m_PosMovesCollected.Add(TPosMoveItem.Create(APosMove));
end;


procedure TPosBaseCollector.FCollectPos(const APos: TChessPosition);
begin
  m_PosMovesCollected.Add(TPosMoveItem.Create(APos));
end;


procedure TPosBaseCollector.FProcessExtendedOpeningLine(const posMove: TPosMove);
var
  p_posMove: PPosMove;
begin
  if (m_bAddPosMove) then
  begin
    m_bAddSimplePosMove := FALSE;
    if (m_ExtendablePosMoves.Count = 0) then
      exit;
    // Adding previous positions, which hadn't been added to DB before
    TLogger.GetInstance.Info(Format('  Adding %d entires to PosBase in X/X+ mode for game #%d',
                                    [m_ExtendablePosMoves.Count, m_iGameNumber]));
    if (m_bExcludeLoopsInExtensions) then
      FExcludeLoopsInExtensions;
    while (m_ExtendablePosMoves.Count > 0) do
    begin
      p_posMove := m_ExtendablePosMoves.Pop;
      if (Assigned(p_posMove)) then
      begin
        FCollectPosMove(p_posMove^);
        Dispose(p_posMove);
      end;
    end;
  end
  else
  begin
    New(p_posMove);
    p_posMove^ := posMove;
    m_ExtendablePosMoves.Push(p_posMove);
    if ((m_GenOpening = openExtendedPlus) and (not m_bAddSimplePosMove)) then
    begin
      m_bAddSimplePosMove := TRUE;
      m_SimplePosMove := posMove;
    end;
  end;
end;


procedure TPosBaseCollector.FExcludeLoopsInExtensions;
var
  ppm: PPosMove;

  function NIsPawnMoveOrCapture: boolean;
  begin
    with ppm^ do
      Result := ((pos.board[move.i, move.j] <> ES) or
                 (pos.board[move.i0, move.j0] in [WP, BP]));
  end;

var
  TopPos: TChessPosition;
  lstPosMoves: TList;

  procedure NExcludeLoopsInARange(iFrom, iTo: integer);
  var
    i, j: integer;
  begin
    while (iFrom <= iTo) do
    begin
      for i := iFrom to iTo do
      begin
        if (not TopPos.Equals(PPosMove(lstPosMoves[i]).pos)) then
          continue;

        TLogger.GetInstance.Info(Format('  Excluding %d entries in XO mode for game #%d',
                                        [iTo - i + 1, m_iGameNumber]));
        for j := i to iTo do
        begin
          Dispose(lstPosMoves[j]);
          lstPosMoves[j] := nil;
        end;
        exit;
      end;
      TopPos := PPosMove(lstPosMoves[iTo]).pos;
      dec(iTo);
    end;
  end;

var
  iTopIndex: integer;
  i: integer;
begin // .FExcludeLoopsInExtensions
  lstPosMoves := TStackEx(m_ExtendablePosMoves).List;

  TopPos := m_lastPosMove.pos;
  iTopIndex := lstPosMoves.Count;

  for i := lstPosMoves.Count - 1 downto 0 do
  begin
    ppm := lstPosMoves[i];
    if (NIsPawnMoveOrCapture) then
    begin
      NExcludeLoopsInARange(i + 1, iTopIndex - 1);
      TopPos := ppm.pos;
      iTopIndex := i;
    end;
  end;

  NExcludeLoopsInARange(0, iTopIndex - 1);
end;


procedure TPosBaseCollector.FProcessOpeningLine(const posMove: TPosMove);
var
  MoveEstimations: TMoveEstList;

  procedure NProcess(const posMove: TPosMove);
  var
    i: integer;
  begin
    if (Assigned(m_RefPosBase)) then
    begin
      m_bAddPosMove := m_RefPosBase.Find(posMove.pos, MoveEstimations);
      m_bAddPos := (m_bAddPosMove and (MoveEstimations.Count >= 2));
    end
    else
      m_bAddPosMove := m_PosBase.Find(posMove.pos, MoveEstimations);

    if (not m_bAddPosMove) then
      exit;

    i := MoveEstimations.Count - 1;
    while (i >= 0) do
    begin
      with MoveEstimations[i].Move, posMove do
      begin
        m_bAddPosMove := ((i0 = move.i0) and (j0 = move.j0) and (i = move.i) and
                      (j = move.j) and (prom_fig = move.prom_fig));
      end;
      if (m_bAddPosMove) then
      begin
        if (m_bUseStatisticalPrunning) then
          m_bAddPosMove := ((MoveEstimations[i].Estimate and $FFFF) >= 2);
        if (m_bAddPosMove) then
          break;
      end;
      dec(i);
    end;

    if (i < 0) then
      m_bAddPosMove := FALSE;
  end;

begin // .FProcessOpeningLine
  MoveEstimations := nil;
  try
    NProcess(posMove);
  finally
    MoveEstimations.Free;
  end;
end;


procedure TPosBaseCollector.StartLine(bFromPreviousPos: boolean);
var
  pContext: PPosBaseCollectorContext;
begin
  new(pContext);
  pContext.bAddPos := m_bAddPosMove;
  pContext.iPosMovesCount := m_ExtendablePosMoves.Count;
  pContext.PosMove := m_lastPosMove;
  pContext.ResultingPos := m_lastResultingPos;

  m_Contexts.Push(pContext);

  m_bLineStartedFromPreviousPosition := bFromPreviousPos;
end;


procedure TPosBaseCollector.EndLine;
var
  pContext: PPosBaseCollectorContext;
begin
  pContext := m_Contexts.Pop;
  try
    m_lastPosMove := pContext.PosMove;
    m_lastResultingPos := pContext.ResultingPos;

    if (m_bLineStartedFromPreviousPosition) then
      m_bAddPosMove := ((m_GenOpening = openNo) or m_PosBase.Find(m_lastResultingPos)) // Opening
    else
      m_bAddPosMove := pContext.bAddPos; // Opening

    while (pContext.iPosMovesCount < m_ExtendablePosMoves.Count) do // Deletion of subline stack
      Dispose(m_ExtendablePosMoves.Pop);

  finally
    Dispose(pContext);
  end;
  
end;


procedure TPosBaseCollector.Finish;
begin
  if (m_bAddSimplePosMove) then
    FCollectPosMove(m_SimplePosMove);

  if ((not m_bUniqueGames) or m_bAddedToMoveTreeBase) then
    FAddCollectedPosMovesToBase
  else
  begin
    m_bSomeGamesSkippedForTest := TRUE;
    TLogger.GetInstance.Info('  Adding to PosBase skipped for game #' + IntToStr(m_iGameNumber));
  end;

  m_PosMovesCollected.Clear;
end;


procedure TPosBaseCollector.FAddCollectedPosMovesToBase;
var
  i: integer;
begin
  for i := 0 to m_PosMovesCollected.Count - 1 do
  begin
    with TPosMoveItem(m_PosMovesCollected[i]) do
    begin
      if (PosOnly) then
        m_PosBase.Add(Pos)
      else
        m_PosBase.Add(PosMove);
    end;
  end;
end;


procedure TPosBaseCollector.Start(const Visitor: IPGNTraverserVisitor);
begin
  m_bAddPosMove := TRUE;
  m_bAddPos := FALSE;
  m_bAddSimplePosMove := FALSE;
  m_bAddedToMoveTreeBase := FALSE;

  if (m_strPlayerName <> '') then
  begin
    if (Visitor.White = m_strPlayerName) then
      m_ProceedColorsInner := m_ProceedColors * [fcWhite]
    else if (Visitor.Black = m_strPlayerName) then
      m_ProceedColorsInner := m_ProceedColors * [fcBlack];
  end
  else
    m_ProceedColorsInner := m_ProceedColors;

  FClearPosMoves;
  FClearContexts;

  inc(m_iGameNumber);

  if (not Assigned(m_PosBase)) then
    FCreatePosBase;

end;


procedure TPosBaseCollector.FCreatePosBase;
begin
  if (Assigned(m_PosBaseForTest)) then
  begin
    m_PosBase := m_PosBaseForTest;
    exit;
  end;

  if (m_bChangeEstimation) then
    m_PosBase := TPosBase.Create(m_strPosBaseName, m_MoveTreeBase, Reestimate)
  else
    m_PosBase := TPosBase.Create(m_strPosBaseName, m_MoveTreeBase);
end;


procedure TPosBaseCollector.FDestroyPosBase;
begin
  if (m_PosBase <> m_PosBaseForTest) then
    FreeAndNil(m_PosBase);
end;


procedure TPosBaseCollector.FClearPosMoves;
begin
  while (m_ExtendablePosMoves.Count > 0) do
    Dispose(m_ExtendablePosMoves.Pop);
end;


procedure TPosBaseCollector.FClearContexts;
begin
  while (m_Contexts.Count > 0) do
    Dispose(m_Contexts.Pop);
end;


procedure TPosBaseCollector.FReestimate(moveEsts: TList; nRec: integer);
var
  est : LongWord;
begin
  // Re-estimation is done here:
  // Re-estimation for a DB with GMs games
  est := LongWord(moveEsts[nRec]);
  if ((est and $FFFF) < $FFFF) then
    est := est + 1;
  // For statistical estimation: if position per one game comes more than one time -> don't change estimation
  if (m_bUseUniquePositions) then
  begin
    if ((est shr 16) = m_iGameNumber) then // exclude repitition of "position + moves" in one game
      exit;
    est := (m_iGameNumber shl 16) or (est and $FFFF);
  end
  else
    est := est and $FFFF;

  moveEsts[nRec] := Pointer(est);
end;

////////////////////////////////////////////////////////////////////////////////
// TPosMoveItem

constructor TPosMoveItem.Create(const APosMove: TPosMove);
begin
  inherited Create;
  m_PosMove := APosMove;
end;


constructor TPosMoveItem.Create(const APos: TChessPosition);
begin
  inherited Create;
  m_PosMove.pos := APos;
  m_bPosOnly := TRUE;
end;


function TPosMoveItem.FGetPos: TChessPosition;
begin
  Result := m_PosMove.pos;
end;

end.
