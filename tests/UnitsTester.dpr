////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

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
  ChessClockUnit in '..\ChessClockUnit.pas',
  MoveTreeBaseUnit in '..\MoveTreeBaseUnit.pas',
  MoveTreeBaseTestsUnit in '..\MoveTreeBaseTestsUnit.pas',
  PGNTraverserUnit in '..\PGNTraverserUnit.pas',
  NonRefInterfacedObjectUnit in '..\NonRefInterfacedObjectUnit.pas',
  MoveTreeCollectorUnit in '..\PosDB\MoveTreeCollectorUnit.pas';

{$R *.res}

begin
  Application.Initialize;
  GUITestRunner.RunRegisteredTests;
end.
