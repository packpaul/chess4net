program Chess4Net;

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
  NonMainFormStayOnTopUnit in '..\NonMainFormStayOnTopUnit.pas',
  ConnectionUnit in '..\ConnectionUnit.pas' {ConnectionForm},
  ConnectingUnit in '..\ConnectingUnit.pas' {ConnectingForm},
  GameOptionsUnit in '..\GameOptionsUnit.pas' {GameOptionsForm},
  ManagerUnit in 'ManagerUnit.pas' {Manager},
  ChessBoardHeaderUnit in '..\ChessBoardHeaderUnit.pas',
  ChessBoardUnit in '..\ChessBoardUnit.pas' {ChessBoard},
  PosBaseChessBoardUnit in '..\PosBaseChessBoardUnit.pas',
  GameChessBoardUnit in '..\GameChessBoardUnit.pas' {GameChessBoard},
  PromotionUnit in '..\PromotionUnit.pas' {PromotionForm},
  LookFeelOptionsUnit in '..\LookFeelOptionsUnit.pas' {OptionsForm},
  ConnectorUnit in 'ConnectorUnit.pas' {Connector: TDataModule},
  PosBaseUnit in '..\PosBaseUnit.pas',
  ModalForm in '..\ModalForm.pas',
  GlobalsLocalUnit in 'GlobalsLocalUnit.pas',
  GlobalsUnit in '..\GlobalsUnit.pas',
  ContinueUnit in '..\ContinueUnit.pas' {ContinueForm},
  DialogUnit in '..\DialogUnit.pas',
  MessageDialogUnit in '..\MessageDialogUnit.pas',
  BitmapResUnit in '..\BitmapResUnit.pas',
  ChessRulesEngine in '..\ChessRulesEngine.pas',
  LocalizerUnit in '..\LocalizerUnit.pas',
  ChessClockUnit in '..\ChessClockUnit.pas',
  PosBaseChessBoardLayerUnit in '..\PosBaseChessBoardLayerUnit.pas';

{$R ..\Chess4Net.res}

var
  Manager: TManager;
  
begin
  Application.Initialize;
  Application.Title := 'Chess4Net';
  Application.CreateForm(TManager, Manager);
  Application.ShowMainForm := False;
  Application.Run;
end.

