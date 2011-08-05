program UnitsTester;

uses
  Forms,
  GUITestRunner,
  EnvironmentSetterTestsUnit in 'EnvironmentSetterTestsUnit.pas',
  EnvironmentSetterUnit in '..\EnvironmentSetterUnit.pas',
  GlobalsUnit in '..\..\GlobalsUnit.pas',
  GlobalsLocalUnit in '..\GlobalsLocalUnit.pas';

{$R *.res}

begin
  Application.Initialize;
  GUITestRunner.RunRegisteredTests;
end.
