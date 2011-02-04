unit ChessRulesEngineTests;

interface

uses
  TestFrameWork,
  //
  ChessRulesEngine;

type
  TChessRulesEngineTests = class(TTestCase)
  private
    m_ChessRulesEngine: TChessRulesEngine;

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
  SysUtils;

////////////////////////////////////////////////////////////////////////////////
// TChessRulesEngineTests

class function TChessRulesEngineTests.Suite: ITestSuite;
var
  TestSuite: TTestSuite;
begin
  TestSuite := TTestSuite.Create('TChessRulesEngine');

  TestSuite.AddTests(self);
  // or
  // TestSuite.AddTest(TChessRulesEngineTests.Create(<method name>));

  Result := TestSuite;
end;


procedure TChessRulesEngineTests.SetUp;
begin
  m_ChessRulesEngine := TChessRulesEngine.Create;
end;


procedure TChessRulesEngineTests.TearDown;
begin
  FreeAndNil(m_ChessRulesEngine);
end;


procedure TChessRulesEngineTests.Test1;
begin
  m_ChessRulesEngine.InitNewGame;
  CheckTrue(m_ChessRulesEngine.DoMove('e4'));
end;


procedure TChessRulesEngineTests.Test2;
begin
  CheckTrue(FALSE, 'Wront value in Test2!');
end;

initialization
  TestFramework.RegisterTest(TChessRulesEngineTests.Suite);

end.
