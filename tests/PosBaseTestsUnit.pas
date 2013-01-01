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

    PGNTraverser := TPGNTraverser.Create(strlData, [PosBaseCollector, MoveTreeCollector]);

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
      PosBaseCollector.Free;
      PGNTraverser.Free;
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
    PosBaseCollector.UseUniquePositions := TRUE;
    PosBaseCollector.GeneratedOpening := openExtended;

    PGNTraverser := TPGNTraverser.Create(strlData, [PosBaseCollector, MoveTreeCollector]);
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
  moveEsts: TMoveEstList;
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

  moveEsts := nil;
  try
    CheckTrue(PosBase.Find(pos0, moveEsts), 'pos0');
    CheckTrue(PosBase.Find(pos1, moveEsts), 'pos1');
  finally
    moveEsts.Free;
  end;
end;

initialization
  RegisterTest(TTestSuiteEx.Create('TPosBase',
    [TMultipleOutletsTestCase,
     TEmptyMovesTestCase]));

end.
