program UnitsTester;

uses
  Forms,
  GUITestRunner,
  ChessRulesEngineTests in 'ChessRulesEngineTests.pas',
  ChessRulesEngine in '..\ChessRulesEngine.pas',
  GameChessBoardUnit in '..\GameChessBoardUnit.pas' {GameChessBoard: TTntForm},
  GameChessBoardTestsUnit in 'GameChessBoardTestsUnit.pas',
  ChessBoardHeaderUnit in '..\ChessBoardHeaderUnit.pas',
  ChessBoardUnit in '..\ChessBoardUnit.pas' {ChessBoard},
  BitmapResUnit in '..\BitmapResUnit.pas',
  PromotionUnit in '..\PromotionUnit.pas' {PromotionForm},
  PosBaseChessBoardLayerUnit in '..\PosBaseChessBoardLayerUnit.pas',
  PosBaseUnit in '..\PosBaseUnit.pas',
  LocalizerUnit in '..\LocalizerUnit.pas',
  GlobalsUnit in '..\GlobalsUnit.pas',
  ChessClockUnit in '..\ChessClockUnit.pas';

{$R *.res}

begin
  Application.Initialize;
  GUITestRunner.RunRegisteredTests;
end.
