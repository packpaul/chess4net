program Chess4Net_Analyzer;

(*
{$IFDEF FASTMM4}
  FastMM4,
{$ENDIF}
*)

uses
{$IFDEF FASTMM4}
  FastMM4,
{$ENDIF}
  Forms,
  AnalyseChessBoardUnit in 'AnalyseChessBoardUnit.pas' {AnalyseChessBoard: TTntForm},
  PosBaseChessBoardUnit in '..\PosBaseChessBoardUnit.pas',
  ChessBoardHeaderUnit in '..\ChessBoardHeaderUnit.pas',
  ChessBoardUnit in '..\ChessBoardUnit.pas' {ChessBoard},
  ChessRulesEngine in '..\ChessRulesEngine.pas',
  PosBaseUnit in '..\PosBaseUnit.pas',
  BitmapResUnit in '..\BitmapResUnit.pas',
  PromotionUnit in '..\PromotionUnit.pas' {PromotionForm};

{$R ..\Chess4Net.res}

var
  AnalyseChessBoard: TAnalyseChessBoard;

begin
  Application.Initialize;
  Application.Title := 'Chess4Net Analyzer';
  Application.CreateForm(TAnalyseChessBoard, AnalyseChessBoard);
  Application.CreateForm(TChessBoard, ChessBoard);
  Application.Run;
end.

