library Chess4Net_MI;
{*******************************
  plugin library for Miranda
********************************}

uses
{$IFDEF FASTMM4}
  FastMM4,
{$ENDIF}
  Forms,
  SysUtils,
  Windows,
  ConnectingUnit in '..\ConnectingUnit.pas' {ConnectingForm},
  GameOptionsUnit in '..\GameOptionsUnit.pas' {GameOptionsForm},
  ChessBoardHeaderUnit in '..\ChessBoardHeaderUnit.pas',
  ChessBoardUnit in '..\ChessBoardUnit.pas' {ChessBoard},
  PosBaseChessBoardUnit in '..\PosBaseChessBoardUnit.pas',
  PromotionUnit in '..\PromotionUnit.pas' {PromotionForm},
  LookFeelOptionsUnit in '..\LookFeelOptionsUnit.pas' {OptionsForm},
  DialogUnit in '..\DialogUnit.pas',
  ModalForm in '..\ModalForm.pas',
  m_globaldefs in 'MirandaINC\m_globaldefs.pas',
  m_api in 'MirandaINC\m_api.pas',
  ManagerUnit in '..\ManagerUnit.pas' {Manager},
  GlobalsLocalUnit in 'GlobalsLocalUnit.pas',
  ConnectorUnit in 'ConnectorUnit.pas' {Connector: TDataModule},
  MessageDialogUnit in '..\MessageDialogUnit.pas',
  GlobalsUnit in '..\GlobalsUnit.pas',
  ContinueUnit in '..\ContinueUnit.pas' {ContinueForm},
  InfoUnit in '..\InfoUnit.pas' {InfoForm},
  PosBaseUnit in '..\PosBaseUnit.pas',
  BitmapResUnit in '..\BitmapResUnit.pas';

{$R ..\Chess4Net.res}

const
  PLUGIN_GUID: TGUID = '{BF17C6E3-C52C-4CB8-88ED-E0FC5F5D566A}';
  MIID_PLUGIN: TGUID = '{09A68681-841C-4728-9F33-07A9F1E976A6}';


function MirandaPluginInfo(mirandaVersion: DWORD): PPLUGININFO; cdecl;
begin
  PLUGININFO.cbSize := sizeof(TPLUGININFO);
  PLUGININFO.shortName := 'Chess4Net';
  PLUGININFO.version := PLUGIN_MAKE_VERSION(200,8,1,0); // 2008.1
  PLUGININFO.description := 'Plugin for playing chess via Miranda';
  PLUGININFO.author := 'Pavel Perminov';
  PLUGININFO.authorEmail := 'packpaul@mail.ru';
  PLUGININFO.copyright := '(c) 2007-2008 No Copyrights';
  PLUGININFO.homepage := 'http://www.chess4net.ru';
  PLUGININFO.isTransient := 0;
  PLUGININFO.replacesDefaultModule := 0;

  Result := @PLUGININFO;
end;

function MirandaPluginInfoEx(mirandaVersion: DWORD): PPLUGININFOEX; cdecl;
begin
  MirandaPluginInfo(mirandaVersion); // Инициализировать PLUGININFO

  Move(PLUGININFO, PLUGININFOEX, sizeof(TPLUGININFO));
  PLUGININFOEX.cbSize := sizeof(TPLUGININFOEX);
  PLUGININFOEX.uuid := PLUGIN_GUID; // ANSI

  Result := @PLUGININFOEX;
end;

var
  PluginInterfaces: array[0..1] of TGUID;

function MirandaPluginInterfaces: PGUID; cdecl;
begin
  PluginInterfaces[0] := MIID_PLUGIN;
  PluginInterfaces[1] := MIID_LAST;

  Result := @PluginInterfaces;
end;

function Start(wParam: WPARAM; lParam_: LPARAM): Integer; cdecl;
begin
  TManager.Create(wParam);
  Result := 0;
end;

function Load(link: PPLUGINLINK): int; cdecl;
var
  mi: TCListMenuItem;
  prt: TPROTOCOLDESCRIPTOR;
begin
  PLUGINLINK := Pointer(link);
  pluginLink^.CreateServiceFunction('Chess4Net/MenuCommand', @Start);
  FillChar(mi, sizeof(mi), 0);
  mi.cbSize := sizeof(mi);
  mi.position := $7FFFFFFF;
  mi.flags := 0;
//  mi.hIcon := LoadSkinnedIcon(SKINICON_OTHER_MIRANDA); // TODO: загрузка родной иконки
  mi.hIcon := LoadIcon(hInstance, 'MAINICON');
  mi.pszName := '&Chess4Net';
  mi.pszService := 'Chess4Net/MenuCommand';
  CallService(MS_CLIST_ADDCONTACTMENUITEM, 0, LPARAM(@mi));

  // регистрация фильтра сообщений
  prt.cbSize := sizeof(prt);
  prt.szName := 'Chess4Net';
  prt.type_ := PROTOTYPE_FILTER;
  CallService(MS_PROTO_REGISTERMODULE, 0, LPARAM(@prt));

  Result := 0;
end;


function Unload: int; cdecl;
begin
  Result := 0;
end;

exports
  MirandaPluginInfo, MirandaPluginInfoEx, MirandaPluginInterfaces, Load, Unload;

begin
end.
