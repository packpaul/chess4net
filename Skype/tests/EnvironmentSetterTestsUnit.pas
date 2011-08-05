////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit EnvironmentSetterTestsUnit;

interface

uses
  TestFrameWork,
  //
  EnvironmentSetterUnit;

type
  TEnvironmentSetter = class(EnvironmentSetterUnit.TEnvironmentSetter);

  TEnvironmentSetterTests = class(TTestCase)
  private
    m_EnvironmentSetter: TEnvironmentSetter;
  public
    class function Suite: ITestSuite; override;

    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure SetEnvironmentTest;
  end;

implementation

uses
  SysUtils,
  //
  GlobalsLocalUnit;

////////////////////////////////////////////////////////////////////////////////
// TEnvironmentSetterTests

class function TEnvironmentSetterTests.Suite: ITestSuite;
var
  TestSuite: TTestSuite;
begin
  TestSuite := TTestSuite.Create(TEnvironmentSetter.ClassName);

  TestSuite.AddTests(self);
  // or
  // TestSuite.AddTest(TPGNWriterTests.Create(<method name>));

  Result := TestSuite;
end;


procedure TEnvironmentSetterTests.SetUp;
begin
  m_EnvironmentSetter := TEnvironmentSetter.RCreate;
  m_EnvironmentSetter.IsTesting := TRUE;
end;


procedure TEnvironmentSetterTests.TearDown;
begin
  FreeAndNil(m_EnvironmentSetter);
end;


procedure TEnvironmentSetterTests.SetEnvironmentTest;
begin
  m_EnvironmentSetter.RSetEnvironment;

  CheckNotEqualsString('', Chess4NetPath);
  Status('Chess4NetPath=' + Chess4NetPath);

  CheckNotEqualsString('', Chess4NetIniFilePath);
  Status('Chess4NetIniFilePath=' + Chess4NetIniFilePath);

  CheckNotEqualsString('', Chess4NetGamesLogPath);
  Status('Chess4NetGamesLogPath=' + Chess4NetGamesLogPath);
end;

initialization
  TestFramework.RegisterTest(TEnvironmentSetterTests.Suite);

end.
