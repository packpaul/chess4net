unit ControlUnit;

interface

uses
  Graphics,
  SysUtils, //Classes,
  m_globaldefs, m_api,
  ConnectorUnit;

type
  IMirandaPlugin = interface(IConnectorable)
    procedure Start;
  end;

const
  PLUGIN_NAME: string = 'MirandaPlugin';
  PLUGIN_MENU_NAME: string = 'Miranda&Plugin';

var
  _PluginInfo: PPLUGININFO = @PLUGININFO;
  guidPlugin, miidPlugin: TGUID;

  MirandaPluginsPath, MirandaPluginPath: string;

  MirandaPluginIcon: TIcon = nil;
  MirandaPluginMenuPosition: integer = $7FFFFFFF;

//  gShowPluginOptions: TProcedure = nil;
//  gShowWrongSDKVersion: TProcedure = nil;
  gCreatePluginInstance: function(Connector: TConnector): IMirandaPlugin = nil;
  gInitializeControls: TProcedure = nil;
  gDeinitializeControls: TProcedure = nil;
//  gStartOnWrongMsgProtocol: function: boolean = nil;
  gErrorDuringPluginStart: TProcedure = nil;

function MirandaPluginInfo(mirandaVersion: DWORD): PPLUGININFO; cdecl;
function MirandaPluginInfoEx(mirandaVersion: DWORD): PPLUGININFO; cdecl;
function MirandaPluginInterfaces: PGUID; cdecl;
function Load(link: PPLUGINLINK): int; cdecl;
function Unload: int; cdecl;

function MakeMirandaPluginVersion(a, b, c, d: byte): int;

exports
  MirandaPluginInfo, MirandaPluginInfoEx, MirandaPluginInterfaces, Load, Unload;

implementation

uses
  Dialogs, Controls, Forms,
  PluginCommonUnit;

var
  PluginInterfaces: array[0..1] of TGUID;

function MirandaPluginInfo(mirandaVersion: DWORD): PPLUGININFO; cdecl;
begin
  PLUGININFO.cbSize := sizeof(TPLUGININFO);
  PLUGININFO.isTransient := 0;
  PLUGININFO.replacesDefaultModule := 0;

  Result := @PLUGININFO;
end;


function MirandaPluginInfoEx(mirandaVersion: DWORD): PPLUGININFO; cdecl;
begin
  MirandaPluginInfo(mirandaVersion); // Initialize PLUGININFO

  Move(PLUGININFO, PLUGININFOEX, sizeof(TPLUGININFO));
  PLUGININFOEX.cbSize := sizeof(TPLUGININFOEX);
  PLUGININFOEX.uuid := guidPlugin;

  Result := @PLUGININFOEX;
end;


function MirandaPluginInterfaces: PGUID; cdecl;
begin
  PluginInterfaces[0] := miidPlugin;
  PluginInterfaces[1] := MIID_LAST;

  Result := @PluginInterfaces;
end;


function Start(wParam: WPARAM; lParam_: LPARAM): Integer; cdecl;
var
  Connector: TConnector;
  pluginInstance: IMirandaPlugin;
begin
  Connector := nil;
  try
    Connector := TConnector.Create(wParam);
    pluginInstance := gCreatePluginInstance(Connector);
    Connector.SetPlugin(pluginInstance);
    pluginInstance.Start;
    Result := 0;
  except
    if Assigned(Connector) then
      Connector.Free;
    if Assigned(gErrorDuringPluginStart) then
      gErrorDuringPluginStart;
    Result := -1;
  end;
end;


function Load(link: PPLUGINLINK): int; cdecl;
var
  mi: TCListMenuItem;
  prt: TPROTOCOLDESCRIPTOR;
begin
  if Assigned(gInitializeControls) then
    gInitializeControls;

  PLUGINLINK := Pointer(link);
  pluginLink^.CreateServiceFunction(PChar(PLUGIN_NAME + '/MenuCommand'), @Start);
  FillChar(mi, sizeof(mi), 0);
  mi.cbSize := sizeof(mi);
  mi.position := MirandaPluginMenuPosition;
  mi.flags := 0; // ?
//  mi.hIcon := LoadSkinnedIcon(SKINICON_OTHER_MIRANDA); // �������� ������ ������
//  mi.hIcon := LoadIcon(hInstance, 'MAINICON'); // �������� ������ �� �������
  if Assigned(MirandaPluginIcon) then
    mi.hIcon := MirandaPluginIcon.Handle;
  mi.pszName := PChar(PLUGIN_MENU_NAME);
  mi.pszService := PChar(PLUGIN_NAME + '/MenuCommand');
  CallService(MS_CLIST_ADDCONTACTMENUITEM, 0, LPARAM(@mi));

  // ����������� ������� ���������
  prt.cbSize := sizeof(prt);
  prt.szName := PChar(PLUGIN_NAME);
  prt.type_ := PROTOTYPE_FILTER;
  CallService(MS_PROTO_REGISTERMODULE, 0, LPARAM(@prt));

  Result := 0;
end;


function Unload: int; cdecl;
begin
  if Assigned(gDeinitializeControls) then
    gDeinitializeControls;
  Result := 0;
end;


function MakeMirandaPluginVersion(a, b, c, d: byte): int;
begin
  Result := PLUGIN_MAKE_VERSION(a,b,c,d);
end;

initialization
  MirandaPluginsPath := ExtractFileDir(Application.ExeName) + '\Plugins\';

end.