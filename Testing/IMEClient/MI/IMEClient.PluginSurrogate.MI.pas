unit IMEClient.PluginSurrogate.MI;

interface

uses
  Windows, SysUtils,
  //
  IMEClient.PluginSurrogate,
  IMEClient.ModelModule;

type
  EPluginSurrogateMI = class(Exception);
  TPluginSurrogateMI = class(TPluginSurrogate)
  private
//    m_Model: TModelModule;
    m_hLib: THandle;

    constructor FCreate(Model: TModelModule);

    procedure FLoadPlugin;
    procedure FUnloadPlugin;

    procedure FLoadLibrary;
    procedure FUnLoadLibrary;

  public
    class function Create(Model: TModelModule): TPluginSurrogateMI;
    destructor Destroy; override;

    function SendData(const strData: string; iContactID: integer): boolean; override;
    procedure SendError; override;
    procedure StartPlugin(iContactID: integer); override;
  end;

implementation

uses
  // Miranda
  m_globaldefs, m_api;

const
  CHESS4NET_MI_LIB = '.\Plugins\Chess4Net_MI.dll';

////////////////////////// Miranda API ///////////////////////////////

var
  g_Model: TModelModule;
  g_iServiceFuncCount: integer = 0;

  PLUGINLINK: TPLUGINLINK;
  Start: TMIRANDASERVICE;     // точка входа в плагин
  FilterMsg: TMIRANDASERVICE; // фильтр сообщений
  NotifySender: TMIRANDAHOOK; // уведомление клиенту

  g_pi: PPLUGININFO;
  g_bChainReceivedFlag: boolean = FALSE;

  MirandaPluginInfoEx: TMirandaPluginInfo = nil;
  Load: TLoad = nil;
  Unload: TUnload = nil;

(*
// function MirandaPluginInfo(mirandaVersion: DWORD): PPLUGININFO; cdecl; external CHESS4NET_MI;
function MirandaPluginInfoEx(mirandaVersion: DWORD): PPLUGININFO; cdecl; external CHESS4NET_MI_LIB;
function Load(link: PPLUGINLINK): int; cdecl; external CHESS4NET_MI_LIB;
function Unload: int; cdecl; external CHESS4NET_MI_LIB;
*)

function CreateServiceFunction(const char: PChar; MIRANDASERVICE: TMIRANDASERVICE): THandle; cdecl;
const
  M_CHESS4NET = 'Chess4Net';
begin
  if (string(char) = M_CHESS4NET + '/MenuCommand') then
    Start := @MIRANDASERVICE
  else if (string(char) = (M_CHESS4NET + PSR_MESSAGE)) then
    FilterMsg := @MIRANDASERVICE;

  Result := g_iServiceFuncCount;

  inc(g_iServiceFuncCount);
end;


function DestroyServiceFunction(Handle: THandle): int; cdecl;
begin
  dec(g_iServiceFuncCount);

  if (g_iServiceFuncCount = 1) then
    g_Model.CanUnloadPlugin
  else if (g_iServiceFuncCount = 0) then
  begin
    FilterMsg := nil;
    Start := nil;
  end;
  Result := 0;
end;


function CallService(const char: PChar; wParam: WPARAM; lParam_: LPARAM): int; cdecl;
var
  ack: TACKDATA;
begin
  if (string(char) = MS_CLIST_GETCONTACTDISPLAYNAME) then
  begin
    if (wParam = 0) then
      Result := Integer(PChar(g_Model.GetHandleName))
    else
      Result := Integer(PChar(g_Model.GetContactHandleName(wParam)));
  end
  else if (string(char) = MS_PROTO_CALLCONTACTSERVICE) then
  begin
    if (string(PCCSDATA(lParam_).szProtoService) = PSS_MESSAGE) then
    begin
      ack.type_ := ACKTYPE_MESSAGE;
      ack.hContact := PCCSDATA(lParam_).hContact;

      if (g_Model.SendPluginData(PChar(PCCSDATA(lParam_).lparam), PCCSDATA(lParam_).hContact)) then
        ack.result_ := ACKRESULT_SUCCESS
      else
        ack.result_ := ACKRESULT_FAILED;

      if (Assigned(NotifySender)) then
        NotifySender(0, LPARAM(@ack));
    end;
    Result := 0;
  end
  else if (string(char) = MS_PROTO_CHAINRECV) then
  begin
    g_bChainReceivedFlag := TRUE;
    Result := 0;
  end
  else
    Result := 0;
end;


function HookEvent(const szHook: PChar; hook_proc: TMIRANDAHOOK): int; cdecl;
begin
  if (string(szHook) = ME_PROTO_ACK) then
    NotifySender := @hook_proc;
  Result := -1;
end;


function UnhookEvent(Handle: THandle): int; cdecl;
begin
  NotifySender := nil;
  Result := 0;
end;

////////////////////////////////////////////////////////////////////////////////
// TPluginSurrogateMI

var
  g_Instance: TPluginSurrogateMI = nil;

constructor TPluginSurrogateMI.FCreate(Model: TModelModule);
begin
  inherited Create;

  g_Model := Model;

  FLoadLibrary;
  FLoadPlugin;
end;


class function TPluginSurrogateMI.Create(Model: TModelModule): TPluginSurrogateMI;
begin
  if (not Assigned(g_Instance)) then
    g_Instance := TPluginSurrogateMI.FCreate(Model);
  Result := g_Instance;
end;


destructor TPluginSurrogateMI.Destroy;
begin
  g_Model := nil;
  g_Instance := nil;

  FUnloadPlugin;
  FUnLoadLibrary;

  inherited;
end;


function TPluginSurrogateMI.SendData(const strData: string; iContactID: integer): boolean;
var
  css: TCCSDATA;
  proto: TPROTORECVEVENT;
begin
  Result := FALSE;

  css.hContact := iContactID;
  proto.szMessage := PChar(strData);
  css.lParam := LPARAM(@proto);

  if (Assigned(FilterMsg)) then
  begin
    FilterMsg(0, LPARAM(@css));
    Result := (not g_bChainReceivedFlag);
    g_bChainReceivedFlag := FALSE;
  end;
end;


procedure TPluginSurrogateMI.SendError;
begin
  // TODO:
end;


procedure TPluginSurrogateMI.StartPlugin(iContactID: integer);
begin
  Assert(iContactID > 0);
  Start(iContactID, 0);
end;


procedure TPluginSurrogateMI.FLoadLibrary;

  procedure NRaiseError;
  begin
    raise EPluginSurrogateMI.Create('DLL couldn''t be loaded!');
  end;

begin
  if (m_hLib <> 0) then
    exit;

  m_hLib := LoadLibrary(CHESS4NET_MI_LIB);
  if (m_hLib = 0) then
    NRaiseError;

  @MirandaPluginInfoEx := GetProcAddress(m_hLib, 'MirandaPluginInfoEx');
  @Load := GetProcAddress(m_hLib, 'Load');
  @Unload := GetProcAddress(m_hLib, 'Unload');

  if (not (Assigned(MirandaPluginInfoEx) and Assigned(Load) and Assigned(Unload))) then
  begin
    FUnloadLibrary;
    NRaiseError;
  end;
end;


procedure TPluginSurrogateMI.FUnLoadLibrary;
begin
  if (m_hLib <> 0) then
  begin
    MirandaPluginInfoEx := nil;
    Load := nil;
    Unload := nil;

    FreeLibrary(m_hLib);
    m_hLib := 0;
  end;
end;


procedure TPluginSurrogateMI.FLoadPlugin;
begin
  PLUGINLINK.CreateServiceFunction := @CreateServiceFunction;
  PLUGINLINK.DestroyServiceFunction := @DestroyServiceFunction;
  PLUGINLINK.CallService := @CallService;
  PLUGINLINK.HookEvent := @HookEvent;
  PLUGINLINK.UnhookEvent := @UnhookEvent;

  g_pi := MirandaPluginInfoEx(PLUGIN_MAKE_VERSION(0, 8, 0, 40));

  Load(@PLUGINLINK);
end;


procedure TPluginSurrogateMI.FUnloadPlugin;
begin
  if (Assigned(Unload)) then
    Unload;
end;

end.
