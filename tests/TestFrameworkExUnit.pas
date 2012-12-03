unit TestFrameworkExUnit;

interface

uses
  TestFramework;

type
  TTestCaseEx = class(TTestCase)
  protected
    class procedure BeforeAllTests; virtual;
    class procedure AfterAllTests; virtual;
  end;

  TTestCaseExClass = class of TTestCaseEx;

  TTestSuiteEx = class(TTestSuite)
  private
    m_arrTests: array of TTestCaseExClass;
    procedure FInit(arrTests: array of TTestCaseExClass);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  public
    constructor Create(const strName: string; arrTests: array of TTestCaseExClass);
  end;

implementation

////////////////////////////////////////////////////////////////////////////////
// TTestSuiteEx

constructor TTestSuiteEx.Create(const strName: string; arrTests: array of TTestCaseExClass);
begin
  inherited Create(strName);
  FInit(arrTests);
end;


procedure TTestSuiteEx.FInit(arrTests: array of TTestCaseExClass);
var
  i: integer;
begin
  SetLength(m_arrTests, Length(arrTests));
  for i := Low(arrTests) to High(arrTests) do
  begin
    m_arrTests[i] := arrTests[i];
    AddTests(arrTests[i]);
  end;
end;


procedure TTestSuiteEx.SetUp;
var
  i: integer;
begin
  for i := Low(m_arrTests) to High(m_arrTests) do
    m_arrTests[i].BeforeAllTests;
end;


procedure TTestSuiteEx.TearDown;
var
  i: integer;
begin
  for i := Low(m_arrTests) to High(m_arrTests) do
    m_arrTests[i].AfterAllTests;
end;

////////////////////////////////////////////////////////////////////////////////
// TTestCaseEx

class procedure TTestCaseEx.BeforeAllTests;
begin
end;


class procedure TTestCaseEx.AfterAllTests;
begin
end;


end.
