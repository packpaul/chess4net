////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit GameChessBoardTestsUnit;

interface

uses
  TestFramework, GUITesting,
  //
  GameChessBoardUnit;

type
  TGameChessBoardTests = class(TGuiTestCase)
  private
    m_TestResult: TTestResult;
    m_ChessBoard: TGameChessBoard;

    function FWasStopped: boolean;
    procedure FChessBoardHandler(e: TGameChessBoardEvent;
      d1: pointer = nil; d2: pointer = nil);

  protected
    procedure RunTest(testResult: TTestResult); override;

    procedure SetUp; override;

  public
    procedure TearDown; override;
    class function Suite: ITestSuite; override;

  published
    procedure TestResizing;
    procedure TestGamingMode;
    procedure TestAnalysisMode;
    procedure TestEditingMode;
  end;

implementation

uses
  Forms, SysUtils,
  //
  ChessBoardUnit;

////////////////////////////////////////////////////////////////////////////////
// TGameChessBoardTests

class function TGameChessBoardTests.Suite: ITestSuite;
var
  TestSuite: TTestSuite;
begin
  TestSuite := TTestSuite.Create(TGameChessBoard.ClassName);

  TestSuite.AddTests(self);
  // or
  // TestSuite.AddTest(TGameChessBoardTests.Create(<method name>));

  Result := TestSuite;
end;


procedure TGameChessBoardTests.RunTest(testResult: TTestResult);
begin
  m_TestResult := testResult;
  try
    inherited;
  finally
    m_TestResult := nil;
  end;
end;


procedure TGameChessBoardTests.SetUp;
begin
  inherited;
  m_ChessBoard := TGameChessBoard.Create(nil, FChessBoardHandler);
  GUI := m_ChessBoard;
  ActionDelay := 100;
  m_ChessBoard.Show;
end;


procedure TGameChessBoardTests.TearDown;
begin
  inherited;
end;


procedure TGameChessBoardTests.TestResizing;
begin
{$IFNDEF DEVELOP}
  exit;
{$ENDIF}

  while m_ChessBoard.Showing do
  begin
    Sleep(1);
    Application.ProcessMessages;
    if (FWasStopped) then
      break;
  end;
end;


procedure TGameChessBoardTests.TestEditingMode;
begin
{$IFNDEF DEVELOP}
  exit;
{$ENDIF}

  m_ChessBoard.Mode := mEdit;

  while m_ChessBoard.Showing do
  begin
    Sleep(1);
    Application.ProcessMessages;
    if (FWasStopped) then
      break;
  end;
end;


procedure TGameChessBoardTests.TestGamingMode;
begin
{$IFNDEF DEVELOP}
  exit;
{$ENDIF}

  m_ChessBoard.Mode := mGame;
  m_ChessBoard.LastMoveHilighted := TRUE;

  while m_ChessBoard.Showing do
  begin
    Sleep(1);
    Application.ProcessMessages;
    if (FWasStopped) then
      break;
  end;
end;


procedure TGameChessBoardTests.TestAnalysisMode;
begin
{$IFNDEF DEVELOP}
  exit;
{$ENDIF}

  m_ChessBoard.Mode := mAnalyse;
  m_ChessBoard.LastMoveHilighted := TRUE;

  while m_ChessBoard.Showing do
  begin
    Sleep(1);
    Application.ProcessMessages;
    if (FWasStopped) then
      break;
  end;
end;


function TGameChessBoardTests.FWasStopped: boolean;
begin
  Result := (Assigned(m_TestResult) and m_TestResult.WasStopped);
end;


procedure TGameChessBoardTests.FChessBoardHandler(e: TGameChessBoardEvent;
  d1: pointer = nil; d2: pointer = nil);
begin
  case e of
    cbeExit:
      m_ChessBoard.Shut; 
  end;
end;


initialization
  TestFramework.RegisterTest(TGameChessBoardTests.Suite);

end.
