////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

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
