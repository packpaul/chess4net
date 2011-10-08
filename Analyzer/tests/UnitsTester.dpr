////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

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
  PGNParserTestsUnit in 'PGNParserTestsUnit.pas',
  PositionEditingFormTestsUnit in 'PositionEditingFormTestsUnit.pas',
  PositionEditingFormUnit in '..\PositionEditingFormUnit.pas' {PositionEditingForm},
  BitmapResUnit in '..\..\BitmapResUnit.pas',
  ChessBoardHeaderUnit in '..\..\ChessBoardHeaderUnit.pas',
  FloatingFormsUnit in '..\FloatingFormsUnit.pas',
  WinControlHlpUnit in '..\WinControlHlpUnit.pas';

{$R *.res}

begin
  Application.Initialize;
  GUITestRunner.RunRegisteredTests;
end.
