unit MoveTreeBaseTestsUnit;

interface

uses
  TestFrameWork,
  //
  MoveTreeBaseUnit;

type
  TMoveTreeBaseTests = class(TTestCase)
  private
    class function FGetMoveTreeBase: TMoveTreeBase;
    class procedure FFillBaseWithInitialData;
    property MoveTreeBase: TMoveTreeBase read FGetMoveTreeBase;
  public
    class function Suite: ITestSuite; override;

    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure Test1;
    procedure Test2;
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
  // This method is run before all sub-tests
  g_MoveTreeBase := TMoveTreeBaseEx.CreateForTest;
  TMoveTreeBaseTests.FFillBaseWithInitialData;
end;


procedure TMoveTreeBaseTestSuite.TearDown;
begin
  // This method is run after all sub-tests
  FreeAndNil(g_MoveTreeBase);
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
  end;

var
  strlData: TStringList;
  PGNTraverser: TPGNTraverser;
  MoveTreeCollector: TMoveTreeCollector;
begin // .FFillBaseWithInitialData
  strlData := nil;
  MoveTreeCollector := nil;
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


procedure TMoveTreeBaseTests.SetUp;
begin
  // TODO:
end;


procedure TMoveTreeBaseTests.TearDown;
begin
  // TODO:
end;


procedure TMoveTreeBaseTests.Test1;
begin
  // TODO:
end;


procedure TMoveTreeBaseTests.Test2;
begin
  // TODO:
end;

initialization
  TestFramework.RegisterTest(TMoveTreeBaseTests.Suite);

end.
