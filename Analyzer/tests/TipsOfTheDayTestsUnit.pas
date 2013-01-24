////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit TipsOfTheDayTestsUnit;

interface

uses
  TestFramework, GUITesting,
  //
  TipsOfTheDayFormUnit, FloatingFormsUnit;

type
  TTipsOfTheDayTests = class(TGuiTestCase)
  private
    m_TestResult: TTestResult;

    m_MainForm: TMainFloatingForm;
    m_Form: TTipsOfTheDayForm;

    function FWasStopped: boolean;

  protected
    procedure RunTest(testResult: TTestResult); override;

    procedure SetUp; override;

  public
    procedure TearDown; override;
    class function Suite: ITestSuite; override;

  published
    procedure TestShowing;
  end;

implementation

uses
  Forms, SysUtils;

////////////////////////////////////////////////////////////////////////////////
// TTipsOfTheDayTests

class function TTipsOfTheDayTests.Suite: ITestSuite;
var
  TestSuite: TTestSuite;
begin
  TestSuite := TTestSuite.Create(TTipsOfTheDayForm.ClassName);

  TestSuite.AddTests(self);
  // or
  // TestSuite.AddTest(TTipsOfTheDayTests.Create(<method name>));

  Result := TestSuite;
end;


procedure TTipsOfTheDayTests.RunTest(testResult: TTestResult);
begin
  m_TestResult := testResult;
  try
    inherited;
  finally
    m_TestResult := nil;
  end;
end;


procedure TTipsOfTheDayTests.SetUp;
begin
  inherited;

  m_MainForm := TMainFloatingForm.CreateNew(nil);
  m_Form := TTipsOfTheDayForm.Create(nil, m_MainForm);
  GUI := m_Form;
  ActionDelay := 100;

  m_MainForm.Show;
  m_Form.Show;
end;


procedure TTipsOfTheDayTests.TearDown;
begin
  m_Form.Release;
  m_MainForm.Release;

  inherited;
end;


procedure TTipsOfTheDayTests.TestShowing;
begin
{$IFNDEF DEVELOP}
  exit;
{$ENDIF}

  while m_Form.Showing do
  begin
    Sleep(1);
    Application.ProcessMessages;
    if (FWasStopped) then
      break;
  end;
end;


function TTipsOfTheDayTests.FWasStopped: boolean;
begin
  Result := (Assigned(m_TestResult) and m_TestResult.WasStopped);
end;


initialization
  TestFramework.RegisterTest(TTipsOfTheDayTests.Suite);

end.
