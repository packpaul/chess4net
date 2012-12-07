////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit MoveTreeBaseTestsUnit;

interface

uses
  TestFrameworkExUnit,
  //
  MoveTreeBaseUnit, ChessRulesEngine;

type
  TMoveTreeBaseTests1 = class(TTestCaseEx)
  private
    m_ChessRulesEngine: TChessRulesEngine;
    class function FGetMoveTreeBase: TMoveTreeBase;
    property MoveTreeBase: TMoveTreeBase read FGetMoveTreeBase;
  protected
    class procedure BeforeAllTests; override;
    class procedure AfterAllTests; override;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestFindInitialPosition;
  end;

  TMoveTreeBaseTests2 = class(TTestCaseEx)
  private
    m_ChessRulesEngine: TChessRulesEngine;
    class function FGetMoveTreeBase: TMoveTreeBase;
    property MoveTreeBase: TMoveTreeBase read FGetMoveTreeBase;
  protected
    class procedure BeforeAllTests; override;
    class procedure AfterAllTests; override;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestFindInitialPositionFar;
  end;

implementation

uses
  Classes, SysUtils, TestFramework,
  //
  PGNTraverserUnit, MoveTreeCollectorUnit;

type
  TMoveTreeBaseEx = class(TMoveTreeBase);
  TMoveTreeCollectorEx = class(TMoveTreeCollector);

var
  g_MoveTreeBase: TMoveTreeBase = nil;
  g_MoveTreeBaseFar: TMoveTreeBase = nil;

////////////////////////////////////////////////////////////////////////////////
// Globals

procedure FillBaseWithInitialData(ABase: TMoveTreeBase);

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
    MoveTreeCollector := TMoveTreeCollectorEx.Create(ABase);
    PGNTraverser := TPGNTraverser.Create(strlData, MoveTreeCollector);

    PGNTraverser.Traverse;

  finally
    PGNTraverser.Free;
    strlData.Free;
  end;
  
end;

////////////////////////////////////////////////////////////////////////////////
// TMoveTreeBaseTests1

class function TMoveTreeBaseTests1.FGetMoveTreeBase: TMoveTreeBase;
begin
  Result := g_MoveTreeBase;
end;


class procedure TMoveTreeBaseTests1.BeforeAllTests;
begin
  g_MoveTreeBase := TMoveTreeBaseEx.CreateForTest;
  FillBaseWithInitialData(g_MoveTreeBase);
end;


class procedure TMoveTreeBaseTests1.AfterAllTests;
begin
  FreeAndNil(g_MoveTreeBase);
end;


procedure TMoveTreeBaseTests1.SetUp;
begin
  m_ChessRulesEngine := TChessRulesEngine.Create;
end;


procedure TMoveTreeBaseTests1.TearDown;
begin
  FreeAndNil(m_ChessRulesEngine);
end;


procedure TMoveTreeBaseTests1.TestFindInitialPosition;
var
  Moves: TMoveAbsArr;
begin
  MoveTreeBase.Find(m_ChessRulesEngine.Position^, Moves);
  CheckEquals(3, Length(Moves));
end;

////////////////////////////////////////////////////////////////////////////////
// TMoveTreeBaseTests2

class function TMoveTreeBaseTests2.FGetMoveTreeBase: TMoveTreeBase;
begin
  Result := g_MoveTreeBaseFar;
end;


class procedure TMoveTreeBaseTests2.BeforeAllTests;
begin
  g_MoveTreeBaseFar := TMoveTreeBaseEx.CreateForTestFarJump;
  FillBaseWithInitialData(g_MoveTreeBaseFar);
end;


class procedure TMoveTreeBaseTests2.AfterAllTests;
begin
  FreeAndNil(g_MoveTreeBaseFar);
end;


procedure TMoveTreeBaseTests2.SetUp;
begin
  m_ChessRulesEngine := TChessRulesEngine.Create;
end;


procedure TMoveTreeBaseTests2.TearDown;
begin
  FreeAndNil(m_ChessRulesEngine);
end;


procedure TMoveTreeBaseTests2.TestFindInitialPositionFar;
var
  Moves: TMoveAbsArr;
begin
  MoveTreeBase.Find(m_ChessRulesEngine.Position^, Moves);
  CheckEquals(3, Length(Moves));
end;

initialization
  TestFramework.RegisterTest(TTestSuiteEx.Create('TMoveTreeBase',
    [TMoveTreeBaseTests1, TMoveTreeBaseTests2]));

end.
