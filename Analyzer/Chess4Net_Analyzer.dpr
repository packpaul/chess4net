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
  PromotionUnit in '..\PromotionUnit.pas' {PromotionForm},
  PGNParserUnit in 'PGNParserUnit.pas',
  ChildProc in 'ChildProc.pas',
  ChessEngine in 'ChessEngine.pas',
  ChessEngineInfoUnit in 'ChessEngineInfoUnit.pas' {ChessEngineInfoForm},
  MoveListFormUnit in 'MoveListFormUnit.pas' {MoveListForm},
  PlysTreeUnit in 'PlysTreeUnit.pas',
  PlysProviderIntfUnit in 'PlysProviderIntfUnit.pas',
  URLVersionQueryUnit in '..\URLVersionQueryUnit.pas' {URLVersionQuery: TDataModule},
  GlobalsLocalUnit in 'GlobalsLocalUnit.pas',
  DontShowMessageDlgUnit in 'DontShowMessageDlgUnit.pas',
  IniSettingsUnit in 'IniSettingsUnit.pas',
  SelectLineFormUnit in 'SelectLineFormUnit.pas' {SelectLineForm},
  OpeningsDBManagerFormUnit in 'OpeningsDBManagerFormUnit.pas' {OpeningsDBSelectionForm},
  OpeningsDBManagerUnit in 'OpeningsDBManagerUnit.pas',
  NonRefInterfacedObjectUnit in '..\NonRefInterfacedObjectUnit.pas',
  PGNWriterUnit in 'PGNWriterUnit.pas',
  SplashFormUnit in 'SplashFormUnit.pas' {SplashForm},
  PositionEditingFormUnit in 'PositionEditingFormUnit.pas' {PositionEditingForm};

{$R ..\Chess4Net.res}

var
  AnalyseChessBoard: TAnalyseChessBoard;

begin
  Application.Initialize;
  Application.Title := 'Chess4Net Analyzer';

  with TSplashForm.Create(Application) do
  try
    Splash;
    Application.CreateForm(TAnalyseChessBoard, AnalyseChessBoard);
    WaitSplashShowing;
  finally
    Free;
  end;

  Application.Run;
end.

