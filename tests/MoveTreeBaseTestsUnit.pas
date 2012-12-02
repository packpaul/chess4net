unit MoveTreeBaseTestsUnit;

interface

uses
  TestFrameWork,
  //
  MoveTreeBaseUnit, ChessRulesEngine;

type
  TMoveTreeBaseTests = class(TTestCase)
  private
    m_ChessRulesEngine: TChessRulesEngine;
    class function FGetMoveTreeBase: TMoveTreeBase;
    class procedure FFillBaseWithInitialData;
    property MoveTreeBase: TMoveTreeBase read FGetMoveTreeBase;
  protected
    class procedure BeforeAllTests;
    class procedure AfterAllTests;
    procedure SetUp; override;
    procedure TearDown; override;
  public
    class function Suite: ITestSuite; override;
  published
    procedure TestFindInitialPosition;
  end;

implementation

uses
  Classes, SysUtils,
  //
  PGNTraverserUnit, MoveTreeCollectorUnit;

type
  TMoveTreeBaseTestSuite = class(TTestSuite)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  public
    constructor Create; overload;
    constructor Create(ATestClass: TTestCaseClass); overload;
  end;

  TMoveTreeBaseEx = class(TMoveTreeBase);
  TMoveTreeCollectorEx = class(TMoveTreeCollector);

var
  g_MoveTreeBase: TMoveTreeBase = nil;

////////////////////////////////////////////////////////////////////////////////
// TMoveTreeBaseTestSuite

constructor TMoveTreeBaseTestSuite.Create;
begin
  inherited Create('TMoveTreeBase');
end;


constructor TMoveTreeBaseTestSuite.Create(ATestClass: TTestCaseClass);
begin
  Create;
  AddTests(ATestClass);
end;


procedure TMoveTreeBaseTestSuite.SetUp;
begin
  TMoveTreeBaseTests.BeforeAllTests;
end;


procedure TMoveTreeBaseTestSuite.TearDown;
begin
  TMoveTreeBaseTests.AfterAllTests;
end;

////////////////////////////////////////////////////////////////////////////////
// TMoveTreeBaseTests

class function TMoveTreeBaseTests.Suite: ITestSuite;
begin
  Result := TMoveTreeBaseTestSuite.Create(self);
  // or
  // Result := TMoveTreeBaseTestSuite.Create;
  // Result.AddTest(TMoveTreeBaseTests.Create(<method name>));
end;


class function TMoveTreeBaseTests.FGetMoveTreeBase: TMoveTreeBase;
begin
  Result := g_MoveTreeBase;
end;


class procedure TMoveTreeBaseTests.FFillBaseWithInitialData;

  procedure NCreateInitialData(out strlData: TStringList);
  begin
    strlData := TStringList.Create;

    strlData.Append('[C4N "2"]');
    strlData.Append('');
    strlData.Append('1. e4 e5 2. Bc4');
    strlData.Append('');
    strlData.Append('[C4N "2"]');
    strlData.Append('');
    strlData.Append('1. e4 e5 2. Nf3');
    strlData.Append('');
    strlData.Append('[C4N "2"]');
    strlData.Append('');
    strlData.Append('1. e4 d5');
    strlData.Append('');
    strlData.Append('[C4N "2"]');
    strlData.Append('');
    strlData.Append('1. e4 d5');
    strlData.Append('');
    strlData.Append('[C4N "2"]');
    strlData.Append('');
    strlData.Append('1. e4 e5 2. Bc4');
    strlData.Append('');
    strlData.Append('[C4N "2"]');
    strlData.Append('');
    strlData.Append('1. e4 e6');
    strlData.Append('');
    strlData.Append('[C4N "2"]');
    strlData.Append('');
    strlData.Append('1. d4');
    strlData.Append('[C4N "2"]');
    strlData.Append('');
    strlData.Append('1. c4');
  end;

var
  strlData: TStringList;
  PGNTraverser: TPGNTraverser;
  MoveTreeCollector: TMoveTreeCollector;
begin // .FFillBaseWithInitialData
  strlData := nil;
  PGNTraverser := nil;
  try
    NCreateInitialData(strlData);
    MoveTreeCollector := TMoveTreeCollectorEx.Create(FGetMoveTreeBase);
    PGNTraverser := TPGNTraverser.Create(strlData, MoveTreeCollector);

    PGNTraverser.Traverse;

  finally
    PGNTraverser.Free;
    strlData.Free;
  end;
  
end;


class procedure TMoveTreeBaseTests.BeforeAllTests;
begin
  g_MoveTreeBase := TMoveTreeBaseEx.CreateForTest;
  TMoveTreeBaseTests.FFillBaseWithInitialData;
end;


class procedure TMoveTreeBaseTests.AfterAllTests;
begin
  FreeAndNil(g_MoveTreeBase);
end;


procedure TMoveTreeBaseTests.SetUp;
begin
  m_ChessRulesEngine := TChessRulesEngine.Create;  
end;


procedure TMoveTreeBaseTests.TearDown;
begin
  FreeAndNil(m_ChessRulesEngine);
end;


procedure TMoveTreeBaseTests.TestFindInitialPosition;
var
  Moves: TMoveAbsArr;
begin
  MoveTreeBase.Find(m_ChessRulesEngine.Position^, Moves);
  CheckEquals(3, Length(Moves));
end;

initialization
  TestFramework.RegisterTest(TMoveTreeBaseTests.Suite);

end.
