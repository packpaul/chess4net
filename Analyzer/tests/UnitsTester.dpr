program UnitsTester;

uses
  Forms,
  GUITestRunner,
  PGNWriterTestsUnit in 'PGNWriterTestsUnit.pas',
  PGNWriterUnit in '..\PGNWriterUnit.pas',
  PlysTreeUnit in '..\PlysTreeUnit.pas',
  PlysProviderIntfUnit in '..\PlysProviderIntfUnit.pas',
  PGNParserUnit in '..\PGNParserUnit.pas',
  ChessRulesEngine in '..\..\ChessRulesEngine.pas',
  PGNParserTestsUnit in 'PGNParserTestsUnit.pas';

{$R *.res}

begin
  Application.Initialize;
  GUITestRunner.RunRegisteredTests;
end.
