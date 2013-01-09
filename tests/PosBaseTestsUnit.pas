////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit PosBaseTestsUnit;

interface

uses
  TestFrameworkExUnit,
  //
  PosBaseUnit, MoveTreeBaseUnit;

type
  TTestCaseBase = class(TTestCaseEx)
  private
    m_PosBase: TPosBase;
    m_MoveTreeBase: TMoveTreeBase;
  protected
    property PosBase: TPosBase read m_PosBase;
    property MoveTreeBase: TMoveTreeBase read m_MoveTreeBase;
    procedure SetUp; override;
    procedure TearDown; override;
    procedure RCreatePosBase(out APosBase: TPosBase); virtual;
    procedure RFillBase; virtual; abstract;
  end;

  TMultipleOutletsTestCase = class(TTestCaseBase)
  protected
    procedure RFillBase; override;
  published
    procedure TestFindPositionsWithMultipleOutlets;
  end;

  TEmptyMovesTestCase = class(TTestCaseBase)
  protected
    procedure RCreatePosBase(out APosBase: TPosBase); override;
    procedure RFillBase; override;
  published
    procedure TestFindPositionWithEmptyMoves;
  end;

  TUniqueGamesTestCase = class(TTestCaseBase)
  private
    m_bSomeGamesSkipped: boolean;
  protected
    procedure RCreatePosBase(out APosBase: TPosBase); override;
    procedure RFillBase; override;
  published
    procedure TestUniqueGames;
  end;

  TOpenExtendedTestCase = class(TTestCaseBase)
  protected
    procedure RCreatePosBase(out APosBase: TPosBase); override;
    procedure RFillBase; override;
  published
    procedure TestOpenExtended;
  end;

  TOpenExtendedPlusTestCase = class(TTestCaseBase)
  protected
    procedure RCreatePosBase(out APosBase: TPosBase); override;
    procedure RFillBase; override;
  published
    procedure TestOpenExtendedPlus;
  end;

implementation

uses
  Classes, SysUtils,
  //
  PGNTraverserUnit, MoveTreeCollectorUnit, PosBaseCollectorUnit, ChessRulesEngine;

type
  TMoveTreeBaseEx = class(TMoveTreeBase);
  TPosBaseEx = class(TPosBase);
  TMoveTreeCollectorEx = class(TMoveTreeCollector);
  TPosBaseCollectorEx = class(TPosBaseCollector);

////////////////////////////////////////////////////////////////////////////////
// TTestCaseBase

procedure TTestCaseBase.SetUp;
begin
  m_MoveTreeBase := TMoveTreeBaseEx.CreateForTest;
  RCreatePosBase(m_PosBase);

  RFillBase;

  m_MoveTreeBase.PosCache.Clear;
end;


procedure TTestCaseBase.RCreatePosBase(out APosBase: TPosBase);
begin
  APosBase := TPosBaseEx.CreateForTest(m_MoveTreeBase);
end;


procedure TTestCaseBase.TearDown;
begin
  FreeAndNil(m_PosBase);
  FreeAndNil(m_MoveTreeBase);
end;

////////////////////////////////////////////////////////////////////////////////
// TMultipleOutletsTestCase

procedure TMultipleOutletsTestCase.RFillBase;

  procedure NCreateData(out strlData: TStringList);
  begin
    strlData := TStringList.Create;

    strlData.Append('[C4N "2"]');
    strlData.Append('');
    strlData.Append('1. e4');
    strlData.Append('');
    strlData.Append('[C4N "2"]');
    strlData.Append('');
    strlData.Append('1. e4 e5');
    strlData.Append('');
    strlData.Append('[C4N "2"]');
    strlData.Append('');
    strlData.Append('1. e4 e5 2. d4');
    strlData.Append('');
    strlData.Append('[C4N "2"]');
    strlData.Append('');
    strlData.Append('1. e4 e5 2. d4 d5');
  end;

var
  strlData: TStringList;
  PGNTraverser: TPGNTraverser;
  MoveTreeCollector: TMoveTreeCollector;
  PosBaseCollector: TPosBaseCollector;
begin
  strlData := nil;
  MoveTreeCollector := nil;
  PosBaseCollector := nil;
  PGNTraverser := nil;
  try
    NCreateData(strlData);

    MoveTreeCollector := TMoveTreeCollectorEx.CreateForTest(MoveTreeBase);
    PosBaseCollector := TPosBaseCollectorEx.CreateForTest(PosBase);
    PosBaseCollector.MoveTreeBase := MoveTreeBase;

    PGNTraverser := TPGNTraverser.Create(strlData, [MoveTreeCollector, PosBaseCollector]);

    PGNTraverser.Traverse;
  finally
    PGNTraverser.Free;
    PosBaseCollector.Free;
    MoveTreeCollector.Free;
    strlData.Free;
  end;
  
end;


procedure TMultipleOutletsTestCase.TestFindPositionsWithMultipleOutlets;
var
  pos: TChessPosition;
  moveEsts: TMoveEstList;
begin
  with TChessRulesEngine.Create do
  try
    DoMove('e4');
    DoMove('e5');
    pos := Position^
  finally
    Free;
  end;

  CheckTrue(PosBase.Find(pos, moveEsts));
  moveEsts.Free;
end;

////////////////////////////////////////////////////////////////////////////////
// TMultipleOutletsTestCase

procedure TEmptyMovesTestCase.RCreatePosBase(out APosBase: TPosBase);
begin
  APosBase := TPosBaseEx.CreateForTest(MoveTreeBase, PosBaseCollectorUnit.Reestimate);
end;


procedure TEmptyMovesTestCase.RFillBase;

  procedure NCreateData(out strlData: TStringList);
  begin
    strlData := TStringList.Create;

    strlData.Append('[C4N "2"]');
    strlData.Append('');
    strlData.Append('1. e4 e5 2. Nf3 Nc6');
    strlData.Append('');
    strlData.Append('[C4N "2"]');
    strlData.Append('');
    strlData.Append('1. e4 Nc6 2. Nf3 e5');
  end;

  procedure NCreateRefBase(out APosBase: TPosBase);
  var
    strlData: TStringList;
    PGNTraverser: TPGNTraverser;
    PosBaseCollector: TPosBaseCollector;
  begin
    APosBase := TPosBaseEx.CreateForTest(nil, PosBaseCollectorUnit.Reestimate);

    strlData := nil;
    PGNTraverser := nil;
    PosBaseCollector := nil;
    try
      NCreateData(strlData);
      PosBaseCollector := TPosBaseCollectorEx.CreateForTest(APosBase);
      PosBaseCollector.UseUniquePositions := TRUE;
      PGNTraverser := TPGNTraverser.Create(strlData, PosBaseCollector);
      PGNTraverser.Traverse;
    finally
      PGNTraverser.Free;
      PosBaseCollector.Free;
      strlData.Free;
    end;
  end;

var
  strlData: TStringList;
  PGNTraverser: TPGNTraverser;
  MoveTreeCollector: TMoveTreeCollector;
  PosBaseCollector: TPosBaseCollector;
  RefBase: TPosBase;
begin
  strlData := nil;
  MoveTreeCollector := nil;
  PosBaseCollector := nil;
  PGNTraverser := nil;

  NCreateRefBase(RefBase);
  try
    NCreateData(strlData);

    MoveTreeCollector := TMoveTreeCollectorEx.CreateForTest(MoveTreeBase);

    PosBaseCollector := TPosBaseCollectorEx.CreateForTest(PosBase, RefBase);
    PosBaseCollector.MoveTreeBase := MoveTreeBase;
    PosBaseCollector.UseUniquePositions := TRUE;
    PosBaseCollector.UseStatisticalPrunning := TRUE;
    PosBaseCollector.GeneratedOpening := openExtended;

    PGNTraverser := TPGNTraverser.Create(strlData, [MoveTreeCollector, PosBaseCollector]);
    PGNTraverser.Traverse;

  finally
    PGNTraverser.Free;
    PosBaseCollector.Free;
    MoveTreeCollector.Free;
    strlData.Free;
    RefBase.Free;
  end;
  
end;


procedure TEmptyMovesTestCase.TestFindPositionWithEmptyMoves;
var
  pos0, pos1: TChessPosition;
begin
  with TChessRulesEngine.Create do
  try
    pos0 := Position^;
    DoMove('e4');
    pos1 := Position^;
    DoMove('e5');
    DoMove('Nf3');
    DoMove('Nc6');
  finally
    Free;
  end;

  CheckTrue(PosBase.Find(pos0), 'pos0');
  CheckTrue(PosBase.Find(pos1), 'pos1');
end;

////////////////////////////////////////////////////////////////////////////////
// TUniqueGamesTestCase

procedure TUniqueGamesTestCase.RCreatePosBase(out APosBase: TPosBase);
begin
  APosBase := TPosBaseEx.CreateForTest(MoveTreeBase, PosBaseCollectorUnit.Reestimate);
end;


procedure TUniqueGamesTestCase.RFillBase;

  procedure NCreateData(out strlData: TStringList);
  begin
    strlData := TStringList.Create;

    strlData.Append('[C4N "2"]');
    strlData.Append('');
    strlData.Append('1. e4 e5 2. Nf3 Nc6');
    strlData.Append('');
    strlData.Append('[C4N "2"]');
    strlData.Append('');
    strlData.Append('1. e4 Nc6 2. Nf3 e5');
    strlData.Append('');
    strlData.Append('[C4N "2"]');
    strlData.Append('');
    strlData.Append('1. e4 e5 2. Nf3 Nc6');
  end;

  procedure NCreateRefBase(out APosBase: TPosBase);
  var
    strlData: TStringList;
    PGNTraverser: TPGNTraverser;
    PosBaseCollector: TPosBaseCollector;
  begin
    APosBase := TPosBaseEx.CreateForTest(nil, PosBaseCollectorUnit.Reestimate);

    strlData := nil;
    PGNTraverser := nil;
    PosBaseCollector := nil;
    try
      NCreateData(strlData);
      PosBaseCollector := TPosBaseCollectorEx.CreateForTest(APosBase);
      PosBaseCollector.UseUniquePositions := TRUE;
      PGNTraverser := TPGNTraverser.Create(strlData, PosBaseCollector);
      PGNTraverser.Traverse;
    finally
      PGNTraverser.Free;
      PosBaseCollector.Free;
      strlData.Free;
    end;
  end;

var
  strlData: TStringList;
  PGNTraverser: TPGNTraverser;
  MoveTreeCollector: TMoveTreeCollector;
  PosBaseCollector: TPosBaseCollectorEx;
  RefBase: TPosBase;
begin
  strlData := nil;
  MoveTreeCollector := nil;
  PosBaseCollector := nil;
  PGNTraverser := nil;

  NCreateRefBase(RefBase);
  try
    NCreateData(strlData);

    MoveTreeCollector := TMoveTreeCollectorEx.CreateForTest(MoveTreeBase);

    PosBaseCollector := TPosBaseCollectorEx.CreateForTest(PosBase, RefBase);
    PosBaseCollector.MoveTreeBase := MoveTreeBase;
    PosBaseCollector.UseUniquePositions := TRUE;
    PosBaseCollector.GeneratedOpening := openExtended;
    PosBaseCollector.UniqueGames := TRUE;

    PGNTraverser := TPGNTraverser.Create(strlData, [MoveTreeCollector, PosBaseCollector]);
    PGNTraverser.Traverse;

    m_bSomeGamesSkipped := PosBaseCollector.SomeGamesSkippedForTest;

  finally
    PGNTraverser.Free;
    PosBaseCollector.Free;
    MoveTreeCollector.Free;
    strlData.Free;
    RefBase.Free;
  end;
  
end;


procedure TUniqueGamesTestCase.TestUniqueGames;
begin
  CheckTrue(m_bSomeGamesSkipped);
end;

////////////////////////////////////////////////////////////////////////////////
// TOpenExtendedTestCase

////////////////////////////////////////////////////////////////////////////////
// TOpenExtendedTestCase

procedure TOpenExtendedTestCase.RCreatePosBase(out APosBase: TPosBase);
begin
  APosBase := TPosBaseEx.CreateForTest(MoveTreeBase, PosBaseCollectorUnit.Reestimate);
end;


procedure TOpenExtendedTestCase.RFillBase;

  procedure NCreateData(out strlData: TStringList);
  begin
    strlData := TStringList.Create;

    strlData.Append('[C4N "2"]');
    strlData.Append('');
    strlData.Append('1. e4 e5 2. Nf3');
    strlData.Append('');
    strlData.Append('[C4N "2"]');
    strlData.Append('');
    strlData.Append('1. e4 e5 2. Nc3 Nf6');
    strlData.Append('');
    strlData.Append('[C4N "2"]');
    strlData.Append('');
    strlData.Append('1. Nc3 e5 2. e4 d6');
    strlData.Append('');
    strlData.Append('[C4N "2"]');
    strlData.Append('');
    strlData.Append('1. e4 e5 2. Nf3 Bc5');
    strlData.Append('');
    strlData.Append('[C4N "2"]');
    strlData.Append('');
    strlData.Append('1. e4 e5 2. Nf3 d6 3. b3');
    strlData.Append('');
    strlData.Append('[C4N "2"]');
    strlData.Append('');
    strlData.Append('1. Nf3 e5 2. e4 Nc6 3. d4');
  end;

  procedure NCreateRefBase(out APosBase: TPosBase);
  var
    strlData: TStringList;
    PGNTraverser: TPGNTraverser;
    PosBaseCollector: TPosBaseCollector;
  begin
    APosBase := TPosBaseEx.CreateForTest(nil, PosBaseCollectorUnit.Reestimate);

    strlData := nil;
    PGNTraverser := nil;
    PosBaseCollector := nil;
    try
      NCreateData(strlData);
      PosBaseCollector := TPosBaseCollectorEx.CreateForTest(APosBase);
      PosBaseCollector.UseUniquePositions := TRUE;
      PGNTraverser := TPGNTraverser.Create(strlData, PosBaseCollector);
      PGNTraverser.Traverse;
    finally
      PGNTraverser.Free;
      PosBaseCollector.Free;
      strlData.Free;
    end;
  end;

var
  strlData: TStringList;
  PGNTraverser: TPGNTraverser;
  MoveTreeCollector: TMoveTreeCollector;
  PosBaseCollector: TPosBaseCollectorEx;
  RefBase: TPosBase;
begin
  strlData := nil;
  MoveTreeCollector := nil;
  PosBaseCollector := nil;
  PGNTraverser := nil;

  NCreateRefBase(RefBase);
  try
    NCreateData(strlData);

    MoveTreeCollector := TMoveTreeCollectorEx.CreateForTest(MoveTreeBase);

    PosBaseCollector := TPosBaseCollectorEx.CreateForTest(PosBase, RefBase);
    PosBaseCollector.MoveTreeBase := MoveTreeBase;
    PosBaseCollector.GeneratedOpening := openExtended;
    PosBaseCollector.UseUniquePositions := TRUE;
    PosBaseCollector.UseStatisticalPrunning := TRUE;

    PGNTraverser := TPGNTraverser.Create(strlData, [MoveTreeCollector, PosBaseCollector]);
    PGNTraverser.Traverse;

  finally
    PGNTraverser.Free;
    PosBaseCollector.Free;
    MoveTreeCollector.Free;
    strlData.Free;
    RefBase.Free;
  end;

end;


procedure TOpenExtendedTestCase.TestOpenExtended;
type
  TIsType = (iOpening, iExtension, iFromMVT);
var
  pos: TChessPosition;
  move: TMoveAbs;

  procedure NCheckIs(IsType: TIsType; const strErrorMsg: string);
  var
    MoveEsts: TMoveEstList;
    bRes: boolean;
    i: integer;
    iEst: integer;
  begin
    bRes := PosBase.Find(pos, MoveEsts);
    try
      CheckTrue(bRes, strErrorMsg);
      for i := 0 to MoveEsts.Count - 1 do
      begin
        if (not move.Equals(MoveEsts[i].Move)) then
          continue;
        iEst := MoveEsts[i].Estimate and $FFFF;
        case IsType of
          iOpening:
            Check(iEst >= 2, strErrorMsg);
          iExtension:
            Check(iEst = 1, strErrorMsg);
          iFromMVT:
            Check(iEst = 0, strErrorMsg);
        end;
        exit;
      end;
    finally
      MoveEsts.Free;
    end;

    Fail(strErrorMsg);
  end;

  procedure NCheckIsOpening;
  begin
    NCheckIs(iOpening, 'NOT Opening!');
  end;

  procedure NCheckIsExtension;
  begin
    NCheckIs(iExtension, 'NOT Extension!');
  end;

  procedure NCheckIsFromMVT;
  begin
    NCheckIs(iFromMVT, 'NOT From MVT!');
  end;

  procedure NCheckNoEntry;
  var
    bRes: boolean;
  begin
    bRes := PosBase.Find(pos);
    CheckFalse(bRes, 'HAS entry!');
  end;

var
  cre: TChessRulesEngine;

  procedure NDoMove(const strMove: string);
  var
    bRes: boolean;
  begin
    pos := cre.Position^;
    bRes := cre.DoMove(strMove);
    CheckTrue(bRes, 'Assertion failed!');
    move := cre.lastMove^;
  end;

  procedure NProceed;
  begin
    // 1. e4 e5
    NDoMove('e4');
    NCheckIsOpening;
    NDoMove('e5');
    NCheckIsOpening;

    // (2. Nc3 Nf6)
    NDoMove('Nc3');
    NCheckIsFromMVT;
    NDoMove('Nf6');
    NCheckIsFromMVT;
    cre.TakeBack;
    cre.TakeBack;

    // 2. Nf3

    NDoMove('Nf3');
    NCheckIsOpening;

    // (2. ... d6 3. b3)
    NDoMove('d6');
    NCheckIsFromMVT;
    NDoMove('b3');
    NCheckNoEntry;
    cre.TakeBack;
    cre.TakeBack;

    // 2. ... Bc5
    NDoMove('Bc5');
    NCheckIsFromMVT;

    cre.InitNewGame;

    // 1. Nf3 e5 2. e4 Nc6 3. d4
    NDoMove('Nf3');
    NCheckIsFromMVT;
    NDoMove('e5');
    NCheckNoEntry;
    NDoMove('e4');
    NCheckNoEntry;
    NDoMove('Nc6');
    NCheckIsFromMVT;
    NDoMove('d4');
    NCheckNoEntry;

    cre.InitNewGame;

    // 1. Nc3 e5 2. e4 d6
    NDoMove('Nc3');
    NCheckIsFromMVT;
    NDoMove('e5');
    NCheckNoEntry;
    NDoMove('e4');
    NCheckNoEntry;
    NDoMove('d6');
    NCheckIsFromMVT;
  end;

begin
  cre := TChessRulesEngine.Create;
  try
    NProceed;
  finally
    cre.Free;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TOpenExtendedPlusTestCase

procedure TOpenExtendedPlusTestCase.RCreatePosBase(out APosBase: TPosBase);
begin
  APosBase := TPosBaseEx.CreateForTest(MoveTreeBase, PosBaseCollectorUnit.Reestimate);
end;


procedure TOpenExtendedPlusTestCase.RFillBase;

  procedure NCreateData(out strlData: TStringList);
  begin
    strlData := TStringList.Create;

    strlData.Append('[C4N "2"]');
    strlData.Append('');
    strlData.Append('1. e4 e5 2. Nf3');
    strlData.Append('');
    strlData.Append('[C4N "2"]');
    strlData.Append('');
    strlData.Append('1. e4 e5 2. Nc3 Nf6');
    strlData.Append('');
    strlData.Append('[C4N "2"]');
    strlData.Append('');
    strlData.Append('1. Nc3 e5 2. e4 d6');
    strlData.Append('');
    strlData.Append('[C4N "2"]');
    strlData.Append('');
    strlData.Append('1. e4 e5 2. Nf3 Bc5');
    strlData.Append('');
    strlData.Append('[C4N "2"]');
    strlData.Append('');
    strlData.Append('1. e4 e5 2. Nf3 d6 3. b3');
    strlData.Append('');
    strlData.Append('[C4N "2"]');
    strlData.Append('');
    strlData.Append('1. Nf3 e5 2. e4 Nc6 3. d4');
  end;

  procedure NCreateRefBase(out APosBase: TPosBase);
  var
    strlData: TStringList;
    PGNTraverser: TPGNTraverser;
    PosBaseCollector: TPosBaseCollector;
  begin
    APosBase := TPosBaseEx.CreateForTest(nil, PosBaseCollectorUnit.Reestimate);

    strlData := nil;
    PGNTraverser := nil;
    PosBaseCollector := nil;
    try
      NCreateData(strlData);
      PosBaseCollector := TPosBaseCollectorEx.CreateForTest(APosBase);
      PosBaseCollector.UseUniquePositions := TRUE;
      PGNTraverser := TPGNTraverser.Create(strlData, PosBaseCollector);
      PGNTraverser.Traverse;
    finally
      PGNTraverser.Free;
      PosBaseCollector.Free;
      strlData.Free;
    end;
  end;

var
  strlData: TStringList;
  PGNTraverser: TPGNTraverser;
  MoveTreeCollector: TMoveTreeCollector;
  PosBaseCollector: TPosBaseCollectorEx;
  RefBase: TPosBase;
begin
  strlData := nil;
  MoveTreeCollector := nil;
  PosBaseCollector := nil;

  NCreateRefBase(RefBase);
  try
    NCreateData(strlData);

    MoveTreeCollector := TMoveTreeCollectorEx.CreateForTest(MoveTreeBase);

    PosBaseCollector := TPosBaseCollectorEx.CreateForTest(PosBase, RefBase);
    PosBaseCollector.MoveTreeBase := MoveTreeBase;
    PosBaseCollector.GeneratedOpening := openExtendedPlus;
    PosBaseCollector.UseUniquePositions := TRUE;
    PosBaseCollector.UseStatisticalPrunning := TRUE;

    PGNTraverser := TPGNTraverser.Create(strlData, [MoveTreeCollector, PosBaseCollector]);
    PGNTraverser.Traverse;
  finally
    PGNTraverser.Free;
    PosBaseCollector.Free;
    MoveTreeCollector.Free;
    strlData.Free;
    RefBase.Free;
  end;

end;


procedure TOpenExtendedPlusTestCase.TestOpenExtendedPlus;
type
  TIsType = (iOpening, iExtension, iFromMVT);
var
  pos: TChessPosition;
  move: TMoveAbs;

  procedure NCheckIs(IsType: TIsType; const strErrorMsg: string);
  var
    MoveEsts: TMoveEstList;
    bRes: boolean;
    i: integer;
    iEst: integer;
  begin
    bRes := PosBase.Find(pos, MoveEsts);
    try
      CheckTrue(bRes, strErrorMsg);
      for i := 0 to MoveEsts.Count - 1 do
      begin
        if (not move.Equals(MoveEsts[i].Move)) then
          continue;
        iEst := MoveEsts[i].Estimate and $FFFF;
        case IsType of
          iOpening:
            Check(iEst >= 2, strErrorMsg);
          iExtension:
            Check(iEst = 1, strErrorMsg);
          iFromMVT:
            Check(iEst = 0, strErrorMsg);
        end;
        exit;
      end;
    finally
      MoveEsts.Free;
    end;

    Fail(strErrorMsg);
  end;

  procedure NCheckIsOpening;
  begin
    NCheckIs(iOpening, 'NOT Opening!');
  end;

  procedure NCheckIsExtension;
  begin
    NCheckIs(iExtension, 'NOT Extension!');
  end;

  procedure NCheckIsFromMVT;
  begin
    NCheckIs(iFromMVT, 'NOT From MVT!');
  end;

  procedure NCheckNoEntry;
  var
    bRes: boolean;
  begin
    bRes := PosBase.Find(pos);
    CheckFalse(bRes, 'HAS entry!');
  end;

var
  cre: TChessRulesEngine;

  procedure NDoMove(const strMove: string);
  var
    bRes: boolean;
  begin
    pos := cre.Position^;
    bRes := cre.DoMove(strMove);
    CheckTrue(bRes, 'Assertion failed!');
    move := cre.lastMove^;
  end;

  procedure NProceed;
  begin
    // 1. e4 e5
    NDoMove('e4');
    NCheckIsOpening;
    NDoMove('e5');
    NCheckIsOpening;

    // (2. Nc3 Nf6)
    NDoMove('Nc3');
    NCheckIsExtension;
    NDoMove('Nf6');
    NCheckIsFromMVT;
    cre.TakeBack;
    cre.TakeBack;

    // 2. Nf3

    NDoMove('Nf3');
    NCheckIsOpening;

    // (2. ... d6 3. b3)
    NDoMove('d6');
    NCheckIsExtension;
    NDoMove('b3');
    NCheckNoEntry;
    cre.TakeBack;
    cre.TakeBack;

    // 2. ... Bc5
    NDoMove('Bc5');
    NCheckIsExtension;

    cre.InitNewGame;

    // 1. Nf3 e5 2. e4 Nc6 3. d4
    NDoMove('Nf3');
    NCheckIsExtension;
    NDoMove('e5');
    NCheckNoEntry;
    NDoMove('e4');
    NCheckNoEntry;
    NDoMove('Nc6');
    NCheckIsFromMVT;
    NDoMove('d4');
    NCheckNoEntry;

    cre.InitNewGame;

    // 1. Nc3 e5 2. e4 d6
    NDoMove('Nc3');
    NCheckIsExtension;
    NDoMove('e5');
    NCheckNoEntry;
    NDoMove('e4');
    NCheckNoEntry;
    NDoMove('d6');
    NCheckIsFromMVT;
  end;

begin
  cre := TChessRulesEngine.Create;
  try
    NProceed;
  finally
    cre.Free;
  end;
end;

initialization
  RegisterTest(TTestSuiteEx.Create('TPosBase',
    [TMultipleOutletsTestCase,
     TEmptyMovesTestCase,
     TUniqueGamesTestCase,
     TOpenExtendedTestCase,
     TOpenExtendedPlusTestCase]));

end.
