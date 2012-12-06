////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

library Chess4Net_Trillian;
{*******************************
  plugin library for Trillian Pro
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
  plugin in 'TrillianINC\plugin.pas',
  GlobalsLocalUnit in 'GlobalsLocalUnit.pas',
  ControlUnit in 'ControlUnit.pas',
  ManagerUnit in '..\ManagerUnit.pas' {Manager},
  ConnectorUnit in 'ConnectorUnit.pas' {Connector: TDataModule},
  MessageDialogUnit in '..\MessageDialogUnit.pas',
  GlobalsUnit in '..\GlobalsUnit.pas',
  PosBaseUnit in '..\PosBaseUnit.pas',
  MoveTreeBaseUnit in '..\MoveTreeBaseUnit.pas',
  ContinueUnit in '..\ContinueUnit.pas' {ContinueForm},
  InfoUnit in '..\InfoUnit.pas' {InfoForm},
  BitmapResUnit in '..\BitmapResUnit.pas',
  LocalizerUnit in '..\LocalizerUnit.pas',
  ChessRulesEngine in '..\ChessRulesEngine.pas',
  ChessClockUnit in '..\ChessClockUnit.pas',
  URLVersionQueryUnit in '..\URLVersionQueryUnit.pas',
  DontShowMessageDlgUnit in '..\DontShowMessageDlgUnit.pas',
  IniSettingsUnit in '..\IniSettingsUnit.pas';

{$R ..\Chess4Net.res}

function PluginVersion: integer; cdecl;
begin
  Result := 2;
end;


procedure PluginLoad(vPluginInfo: PTtkPluginInfo);
begin
  PluginSend := vPluginInfo.plugin_send;

  vPluginInfo.GUID := GUID;
  vPluginInfo.name := 'Chess4Net';
  vPluginInfo.company := 'Pavel Perminov (http://chess4net.ru)';
  vPluginInfo.version := '2008.1.0';
  vPluginInfo.description := 'Plugin for playing chess via Trillian';
end;


procedure PluginUnload;
begin
  // TODO:
end;


function PluginMain(Event: PChar; Data: Pointer): integer; cdecl;
begin
  Result := 0;
  if Event = 'load' then PluginLoad(Data)
    else
  if Event = 'start' then PluginStart
    else
  if Event = 'stop' then PluginStop
    else
  if Event = 'unload' then PluginUnload;
end;


exports
 PluginVersion name 'plugin_version',
 PluginMain name 'plugin_main';

end.
