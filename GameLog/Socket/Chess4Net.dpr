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
  ConnectionUnit in '..\ConnectionUnit.pas' {ConnectionForm},
  ConnectingUnit in '..\ConnectingUnit.pas' {ConnectingForm},
  GameOptionsUnit in '..\GameOptionsUnit.pas' {GameOptionsForm},
  ManagerUnit in 'ManagerUnit.pas' {Manager},
  ChessBoardHeaderUnit in '..\ChessBoardHeaderUnit.pas',
  ChessBoardUnit in '..\ChessBoardUnit.pas' {ChessBoard},
  PromotionUnit in '..\PromotionUnit.pas' {PromotionForm},
  LookFeelOptionsUnit in '..\LookFeelOptionsUnit.pas' {OptionsForm},
  ConnectorUnit in 'ConnectorUnit.pas' {Connector: TDataModule},
  PosBaseUnit in '..\PosBaseUnit.pas',
  PosBaseChessBoardUnit in '..\PosBaseChessBoardUnit.pas',
  ModalForm in '..\ModalForm.pas',
  GlobalsLocalUnit in 'GlobalsLocalUnit.pas',
  GlobalsUnit in '..\GlobalsUnit.pas',
  ContinueUnit in '..\ContinueUnit.pas' {ContinueForm},
  DialogUnit in '..\DialogUnit.pas',
  MessageDialogUnit in '..\MessageDialogUnit.pas',
  BitmapResUnit in '..\BitmapResUnit.pas',
  GameLogUnit in '..\GameLogUnit.pas' {GameLogForm};

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

