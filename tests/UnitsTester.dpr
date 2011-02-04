program UnitsTester;

uses
  Forms,
  GUITestRunner,
  ChessRulesEngineTests in 'ChessRulesEngineTests.pas',
  ChessRulesEngine in '..\ChessRulesEngine.pas';

{$R *.res}

begin
  Application.Initialize;
  GUITestRunner.RunRegisteredTests;
end.
