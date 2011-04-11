////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit ControlUnit;

interface

uses
  u_plugin_info;

function CreateInfiumPLUGIN(PluginService: IQIPPluginService): IQIPPlugin; stdcall;
procedure QIPSendMessage(const msg: string; const accName: WideString; protoDllHandle: integer);
function GetOwnerNick(const wAccName: WideString; iProtoDllHandle: integer): string;
function GetContactNick(const wAccName: WideString; iProtoDllHandle: integer): string;

implementation

uses
  Windows, Dialogs, Controls, Classes, SysUtils,
  u_common, u_plugin_msg,
  GlobalsLocalUnit, InfoUnit, ConnectorUnit, ManagerUnit;

type
  TANEntry = record
    wAccount, wNick: WideString;
  end;
  PANEntry = ^TANEntry;

  TQIPControl = class(TInterfacedObject, IQIPPlugin)
  private
    FPluginInfo: TPluginInfo;
    FPluginService: IQIPPluginService;
    FdllPath: WideString;
    FButtonID: integer;
    FAPNList: TList; // SDK doesn't support returning names on account names. This list is used for aquiring'em from messages

    procedure LoadPlugin;
    procedure CreateControls;
    procedure FreeControls;
    procedure AddButtons;
    procedure ButtonClicked(const PlugMsg: TPluginMessage);
    procedure ProcessSpecMsg(PlugMsg: TPluginMessage);
    function SendMessage(const wMsg, wAccName: WideString; protoDllHandle: integer): boolean;
    procedure InstantMsgRcvd(var PlugMsg: TPluginMessage);
    procedure InstantMsgSend(var PlugMsg: TPluginMessage);
    procedure Add2APNList(const wAccount, wNick: WideString);
    function FindNickInAPNList(const wAccount: WideString): WideString;

  public
    OwnerNick: WideString;

    constructor Create(const PluginService: IQIPPluginService);
    destructor Destroy; override;
    procedure OnQipMessage(var PlugMsg: TPluginMessage); stdcall;
    function  GetPluginInfo: pPluginInfo; stdcall;
  end;

var
  QIPControl: TQIPControl;

const
  NO_NICK_STR = 'NN';

function CreateInfiumPLUGIN(PluginService: IQIPPluginService): IQIPPlugin; stdcall;
begin
  QIPControl := TQIPControl.Create(PluginService);
  Result := QIPControl;
end;

procedure QIPSendMessage(const msg: string; const accName: WideString; protoDllHandle: integer);
begin
  if QIPControl.SendMessage(WideString(msg), accName, protoDllHandle) then
    MessageSent(msg)
  else
    ErrorSendingMessage(msg);  
end;

function GetOwnerNick(const wAccName: WideString; iProtoDllHandle: integer): string;
begin
  Result := QIPControl.OwnerNick;
  if Result = '' then
    Result := NO_NICK_STR;
end;


function GetContactNick(const wAccName: WideString; iProtoDllHandle: integer): string;
begin
  Result := QIPControl.FindNickInAPNList(wAccName);
  if Result = '' then
    Result := NO_NICK_STR;
end;

{----------------------- TQIPControl ---------------------------}

constructor TQIPControl.Create(const PluginService: IQIPPluginService);
begin
  OwnerNick := '';
  FAPNList := TList.Create;
  
  FPluginService := PluginService;
end;

destructor TQIPControl.Destroy;
var
  i: integer;
begin
  for i := 0 to FAPNList.Count - 1 do
    begin
      with PANEntry(FAPNList[i])^ do
        begin
          wAccount := '';
          wNick := '';
        end;
      Dispose(FAPNList[i]);
    end;
    
  FAPNList.Free;
end;

procedure TQIPControl.OnQipMessage(var PlugMsg: TPluginMessage); stdcall;
begin
  case PlugMsg.Msg of
    {Dll library was successfuly loaded and FProtoInfo.DllHandle was updated by qip core}
    {This msg sent always on qip start, even if plugin is disabled in options}
    PM_PLUGIN_LOAD_SUCCESS:
      begin
        LoadPlugin; {updating FPluginInfo, ONLY!!!}
      end;

    {Here you can start plugin work, create any forms, objects etc...
     Note that if plugin is Disabled, it will not receive this message.
     Instead of this msg it will receive PM_PLUGIN_ENABLE if user will enable this plugin in options}
    PM_PLUGIN_RUN:
      begin
        CreateControls;
        // LoadPluginOptions;
      end;

    {The same message like PM_PLUGIN_RUN but when plugin was enabled in
    options and after that plugin will start receive messages from QIP}
    PM_PLUGIN_ENABLE:
      begin
        CreateControls;
        // LoadPluginOptions;
      end;

    {Plugin was disabled in qip options, after this, this plugin will not receive
     any QIP messages (excepting PM_PLUGIN_ENABLE and PM_PLUGIN_QUIT) and
     any plugin message to qip core will be ignored, but plugin will stay loaded into memory.
     Any connections and forms have to be closed.}
    PM_PLUGIN_DISABLE:
      begin
        FreeControls;
      end;

    {Open plugin options window if needed.
     Hint: You can save 30 different parameters in qip profile dedicated to your plugin using
           interface function PluginOptions. For example how to get access to your plugin
           options parameters: var aPluginSpecific: pPluginSpecific;
           aPluginSpecific := FPluginSvc.PluginOptions(FPluginInfo.DllHandle);
           if Assigned(aPluginSpecific) then aPluginSpecific^.Bool1 := True;}
    PM_PLUGIN_OPTIONS:
      begin
        ShowInfo;
      end;

    {Qip sends this message if plugin sdk version higher than qip's sdk version,
     after this msg qip will send PM_PLUGIN_QUIT message}
    PM_PLUGIN_WRONG_SDK_VER:
      begin
        ShowMessage(PLUGIN_WRONG_SDK_VERSION);
      end;

    {text message received notifier}
    PM_PLUGIN_MSG_RCVD:
      InstantMsgRcvd(PlugMsg);

    {text message is going to be sent notifier}
    PM_PLUGIN_MSG_SEND:
      InstantMsgSend(PlugMsg);

    {You can add you own buttons to qip message window below contact avatar.
     If you want to add a button, then you MUST add button after receiving this message ONLY!!!}
    PM_PLUGIN_CAN_ADD_BTNS:
      AddButtons;

    {User clicked on one of your plugin buttons below avatar}
    PM_PLUGIN_MSG_BTN_CLICK:
      ButtonClicked(PlugMsg);

    {Special plugin message received from remote contact, check maybe this is your plugin special message}
    PM_PLUGIN_SPEC_RCVD:
      ProcessSpecMsg(PlugMsg);
  end; { case PlugMsg.Msg }
end;


procedure TQIPControl.LoadPlugin;
var
  buf: array[0..MAX_PATH] of WideChar;
begin
  {Getting and updating Plugin dll path and filling FProtoInfo record}
  GetModuleFileNameW(FPluginInfo.DllHandle, buf, SizeOf(buf));
  FdllPath := buf;

  FPluginInfo.DllPath          := PWideChar(FdllPath);
  FPluginInfo.QipSdkVerMajor   := QIP_SDK_VER_MAJOR;
  FPluginInfo.QipSdkVerMinor   := QIP_SDK_VER_MINOR;
  FPluginInfo.PlugVerMajor     := PLUGIN_VER_MAJOR;
  FPluginInfo.PlugVerMinor     := PLUGIN_VER_MINOR;
  FPluginInfo.PluginName       := PWideChar(PLUGIN_NAME);
  FPluginInfo.PluginAuthor     := PWideChar(PLUGIN_AUTHOR);
end;


function  TQIPControl.GetPluginInfo: pPluginInfo; stdcall;
begin
  Result := @FPluginInfo;
end;


procedure TQIPControl.CreateControls;
begin
  InitGlobals(FdllPath);
end;


procedure TQIPControl.FreeControls;
begin
  DeinitGlobals;
end;


procedure TQIPControl.AddButtons;
var
  BtnInfo : TAddBtnInfo;
  PlugMsg : TPluginMessage;
begin
  BtnInfo.BtnIcon := pluginIcon.Handle;
  BtnInfo.BtnPNG  := 0;
  BtnInfo.BtnHint := PLUGIN_NAME;

  PlugMsg.Msg       := PM_PLUGIN_ADD_BTN;
  PlugMsg.WParam    := LongInt(@BtnInfo);
  PlugMsg.DllHandle := FPluginInfo.DllHandle;

  FPluginService.OnPluginMessage(PlugMsg);

  FButtonID := PlugMsg.Result;
end;


procedure TQIPControl.ButtonClicked(const PlugMsg: TPluginMessage);
var
  iButtonID, iProtoDllHandle: integer;
  wProtoName, wAccName: WideString;
begin
  {get unique btn value from message}
  iButtonID := PlugMsg.WParam;

  {get protocol name of contact from message}
  wProtoName := PWideChar(PlugMsg.NParam);

  {get contact account name from message}
  wAccName   := PWideChar(PlugMsg.LParam);

  {get protocol dll handle from message}
  iProtoDllHandle := PlugMsg.Result;

  if FButtonID <> iButtonID then
    exit;

  if wProtoName <> 'ICQ' then
    begin
      if MessageDlg(PLUGIN_WRONG_PROTOCOL, mtConfirmation, [mbYes, mbNo], 0) = mrNo then
        exit;
    end;

  // Start plugin
  TManager.Create(wAccName, iProtoDllHandle);
end;


procedure TQIPControl.ProcessSpecMsg(PlugMsg: TPluginMessage);
var
  wAccName, wSpecMsgText: WideString;
  iProtoDllHandle: integer;
begin
  {get protocol dll handle from message, you can use it for spec msg reply}
  iProtoDllHandle := PlugMsg.WParam;
  {get contact account name from message}
  wAccName        := PWideChar(PlugMsg.LParam);
  {get special msg text from message}
  wSpecMsgText    := PWideChar(PlugMsg.NParam);

  MessageGot(string(wSpecMsgText), wAccName, iProtoDllHandle);
end;


function TQIPControl.SendMessage(const wMsg, wAccName: WideString; protoDllHandle: integer): boolean;
var
  PlugMsg: TPluginMessage;
begin
  with PlugMsg do
    begin
      Msg       := PM_PLUGIN_SPEC_SEND;
      WParam    := protoDllHandle; //handle of protocol which maybe will send your special message, currently protocol handle can be get only from plugin buttons below avatar
      LParam    := LongInt(PWideChar(wAccName));
      NParam    := LongInt(PWideChar(wMsg));
      DllHandle := FPluginInfo.DllHandle;
    end;

  FPluginService.OnPluginMessage(PlugMsg);
  Result := Boolean(PlugMsg.Result);
end;

procedure TQIPControl.InstantMsgRcvd(var PlugMsg: TPluginMessage);
var
  aQipMsg : pQipMsgPlugin;
begin
  {Getting record pointer of instant message}
  aQipMsg := pQipMsgPlugin(PlugMsg.WParam);

  Add2APNList(aQipMsg.SenderAcc, aQipMsg.SenderNick);

  OwnerNick := aQipMsg.RcvrNick;
end;


procedure TQIPControl.InstantMsgSend(var PlugMsg: TPluginMessage);
var
  aQipMsg : pQipMsgPlugin;
begin
  {Getting record pointer of instant message}
  aQipMsg := pQipMsgPlugin(PlugMsg.WParam);

  OwnerNick := aQipMsg.SenderNick;

  Add2APNList(aQipMsg.RcvrAcc, aQipMsg.RcvrNick);
end;


procedure TQIPControl.Add2APNList(const wAccount, wNick: WideString);

  function MakeANEntry: PANEntry;
  begin
    new(Result);
    Result.wAccount := wAccount;
    Result.wNick  := wNick;
  end;

var
  i, c: integer;
begin
  if FindNickInAPNList(wAccount) = wNick then
    exit;

  i := 0;
  while(i < FAPNList.Count) do
    begin
      c := CompareStr(wAccount, PANEntry(FAPNList[i]).wAccount);
      if c = 0 then // Entry exists - change it
        begin
          PANEntry(FAPNList[i]).wNick := wNick;
          exit;
        end
      else if c < 0 then
        begin
          FAPNList.Insert(i, MakeANEntry);
          exit;
        end;
      inc(i);
    end;
  FAPNList.Add(MakeANEntry);
end;


function TQIPControl.FindNickInAPNList(const wAccount: WideString): WideString;
var
  iLow, iHigh, i: integer;
  c: integer;
begin
  iLow := 0;
  iHigh := FAPNList.Count - 1;

  while (iHigh >= iLow) do
    begin
      i := (iLow + iHigh) div 2;
      c := CompareStr(wAccount, PANEntry(FAPNList[i]).wAccount);
      if c = 0 then
        begin
          Result := PANEntry(FAPNList[i]).wNick;
          exit;
        end
      else if c < 0 then
        iHigh := i - 1
      else // c > 0
        iLow := i + 1;
    end;

  Result := '';
end;

end.
