////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit PositionEditingFormTestsUnit;

interface

uses
  TestFramework, GUITesting,
  //
  PositionEditingFormUnit;

type
  TPositionEditingFormTests = class(TGuiTestCase)
  private
    m_TestResult: TTestResult;
    m_PositionEditingForm: TPositionEditingForm;

    function FWasStopped: boolean;

  public
    class function Suite: ITestSuite; override;
    procedure RunTest(testResult: TTestResult); override;

    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure TestShowing;
  end;

implementation

uses
  Forms, SysUtils;

////////////////////////////////////////////////////////////////////////////////
// TPositionEditingFormTests

class function TPositionEditingFormTests.Suite: ITestSuite;
var
  TestSuite: TTestSuite;
begin
  TestSuite := TTestSuite.Create(TPositionEditingForm.ClassName);

  TestSuite.AddTests(self);
  // or
  // TestSuite.AddTest(TPositionEditingForm.Create(<method name>));

  Result := TestSuite;
end;


procedure TPositionEditingFormTests.RunTest(testResult: TTestResult);
begin
  m_TestResult := testResult;
  try
    inherited;
  finally
    m_TestResult := nil;
  end;
end;


function TPositionEditingFormTests.FWasStopped: boolean;
begin
  Result := (Assigned(m_TestResult) and m_TestResult.WasStopped);
end;


procedure TPositionEditingFormTests.SetUp;
begin
  inherited;

  m_PositionEditingForm := TPositionEditingForm.Create(nil, nil);
  GUI := m_PositionEditingForm;
  ActionDelay := 100;
  m_PositionEditingForm.Show;
end;


procedure TPositionEditingFormTests.TearDown;
begin
  inherited;
end;


procedure TPositionEditingFormTests.TestShowing;
begin
{$IFNDEF DEVELOP}
  exit;
{$ENDIF}

  while m_PositionEditingForm.Showing do
  begin
    Sleep(1);
    Application.ProcessMessages;
    if (FWasStopped) then
      break;
  end;
end;

initialization
  TestFramework.RegisterTest(TPositionEditingFormTests.Suite);

end.
