library Chess4Net_AndRQ;
{*******************************
  plugin library for &RQ
********************************}

(*
{$IFDEF FASTMM4}
  FastMM4,
{$ENDIF}
*)

uses
{$IFDEF FASTMM4}
  FastMM4,
{$ENDIF}
  ConnectingUnit in '..\ConnectingUnit.pas' {ConnectingForm},
  GameOptionsUnit in '..\GameOptionsUnit.pas' {GameOptionsForm},
  ChessBoardHeaderUnit in '..\ChessBoardHeaderUnit.pas',
  ChessBoardUnit in '..\ChessBoardUnit.pas' {ChessBoard},
  PosBaseChessBoardLayerUnit in '..\PosBaseChessBoardLayerUnit.pas',
  GameChessBoardUnit in '..\GameChessBoardUnit.pas' {GameChessBoard},
  PromotionUnit in '..\PromotionUnit.pas' {PromotionForm},
  LookFeelOptionsUnit in '..\LookFeelOptionsUnit.pas' {OptionsForm},
  DialogUnit in '..\DialogUnit.pas',
  ModalForm in '..\ModalForm.pas',
  CallExec in 'AndRQINC\CallExec.pas',
  plugin in 'AndRQINC\plugin.pas',
  pluginutil in 'AndRQINC\pluginutil.pas',
  ControlUnit in 'ControlUnit.pas',
  ManagerUnit in '..\ManagerUnit.pas' {Manager},
  ConnectorUnit in 'ConnectorUnit.pas',
  GlobalsLocalUnit in 'GlobalsLocalUnit.pas',
  InfoUnit in '..\InfoUnit.pas' {InfoForm},
  MessageDialogUnit in '..\MessageDialogUnit.pas',
  GlobalsUnit in '..\GlobalsUnit.pas',
  PosBaseUnit in '..\PosBaseUnit.pas',
  ContinueUnit in '..\ContinueUnit.pas' {ContinueForm},
  BitmapResUnit in '..\BitmapResUnit.pas',
  LocalizerUnit in '..\LocalizerUnit.pas',
  ChessRulesEngine in '..\ChessRulesEngine.pas',
  ChessClockUnit in '..\ChessClockUnit.pas',
  URLVersionQueryUnit in '..\URLVersionQueryUnit.pas',
  DontShowMessageDlgUnit in '..\DontShowMessageDlgUnit.pas',
  IniSettingsUnit in '..\IniSettingsUnit.pas';

{$R ..\Chess4Net.res}

exports
  pluginFun;

end.
