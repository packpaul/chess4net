library Chess4Net_QIP;

(*
{$IFDEF FASTMM4}
  FastMM4,
{$ENDIF}
*)

uses
{$IFDEF FASTMM4}
  FastMM4,
{$ENDIF}
  u_common in 'QIP_SDK\u_common.pas',
  u_plugin_info in 'QIP_SDK\u_plugin_info.pas',
  u_plugin_msg in 'QIP_SDK\u_plugin_msg.pas',
  ControlUnit in 'ControlUnit.pas',
  GlobalsLocalUnit in 'GlobalsLocalUnit.pas',
  InfoUnit in '..\InfoUnit.pas' {InfoForm},
  ManagerUnit in '..\ManagerUnit.pas' {Manager},
  ConnectorUnit in 'ConnectorUnit.pas' {Connector: TDataModule},
  ConnectingUnit in '..\ConnectingUnit.pas' {ConnectingForm},
  GameOptionsUnit in '..\GameOptionsUnit.pas' {GameOptionsForm},
  ChessBoardHeaderUnit in '..\ChessBoardHeaderUnit.pas',
  ChessBoardUnit in '..\ChessBoardUnit.pas' {ChessBoard},
  PosBaseChessBoardUnit in '..\PosBaseChessBoardUnit.pas',  
  GameChessBoardUnit in '..\GameChessBoardUnit.pas' {GameChessBoard},
  PromotionUnit in '..\PromotionUnit.pas' {PromotionForm},
  LookFeelOptionsUnit in '..\LookFeelOptionsUnit.pas' {OptionsForm},
  DialogUnit in '..\DialogUnit.pas',
  ModalForm in '..\ModalForm.pas',
  MessageDialogUnit in '..\MessageDialogUnit.pas',
  GlobalsUnit in '..\GlobalsUnit.pas',
  PosBaseUnit in '..\PosBaseUnit.pas',
  ContinueUnit in '..\ContinueUnit.pas' {ContinueForm},
  BitmapResUnit in '..\BitmapResUnit.pas',
  ChessRulesEngine in '..\ChessRulesEngine.pas',
  LocalizerUnit in '..\LocalizerUnit.pas',
  ChessClockUnit in '..\ChessClockUnit.pas';

{$R ..\Chess4Net.res}

exports
  CreateInfiumPLUGIN;

end.
 