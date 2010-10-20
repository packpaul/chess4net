unit SkypeAPI_Skype;

interface

uses
  Classes, SysUtils, ExtCtrls,
  //
  SkypeAPI,
  //
  SkypeAPI_Object, SkypeAPI_Command;

type
  TAttachmentStatus = (apiAttachUnknown = -1, apiAttachSuccess = 0,
    apiAttachRefused = 2, apiAttachNotAvailable = 3, apiAttachAvailable = 4);
  TChatMessageStatus = (cmsUnknown = -1, cmsSending = 0, cmsSent = 1, cmsReceived = 2);

  IUser = interface
    function GetHandle: WideString;
    function GetFullName: WideString;
    function GetDisplayName: WideString;
    property Handle: WideString read GetHandle;
    property FullName: WideString read GetFullName;
    property DisplayName: WideString read GetDisplayName;
  end;

  IUserCollection = interface
    function GetCount: Integer;
    function GetItem(iIndex: Integer): IUser;
    property Count: Integer read GetCount;
    property Item[iIndex: Integer]: IUser read GetItem; default;
  end;  

  IChatMessage = interface
    function GetSender: IUser;
    function GetBody: WideString;
    property Sender: IUser read GetSender;
    property Body: WideString read GetBody;
  end;

  IApplicationStream = interface(IObject)
  end;

  IApplicationStreamCollection = interface(IObject)
  end;

  IApplication = interface
    procedure Delete;
    procedure Create;
    procedure Connect(const Username: WideString; WaitConnected: Boolean);
    procedure SendDatagram(const Text: WideString; const pStreams: IApplicationStreamCollection);
    function GetStreams: IApplicationStreamCollection;
    function GetConnectableUsers: IUserCollection;
    function GetConnectingUsers: IUserCollection;
    function GetName: WideString;
    property Streams: IApplicationStreamCollection read GetStreams;
    property ConnectableUsers: IUserCollection read GetConnectableUsers;
    property ConnectingUsers: IUserCollection read GetConnectingUsers;
    property Name: WideString read GetName;
  end;

  IClient = interface
    function GetIsRunning: Boolean;
    procedure Start(Minimized: Boolean; Nosplash: Boolean);
    property IsRunning: Boolean read GetIsRunning;
  end;

  IChat = interface(IObject)
    function Get_Name: WideString;
    function SendMessage(const MessageText: WideString): IChatMessage;
    property Name: WideString read Get_Name;
  end;

  ISkype = interface
    procedure Attach(Protocol: Integer; Wait: Boolean);
    function GetApplication(const Name: WideString): IApplication;
    function SendMessage(const Username: WideString; const Text: WideString): IChatMessage;
    function GetClient: IClient;
    function GetCurrentUser: IUser;
    function GetCurrentUserHandle: WideString;
    property Application[const Name: WideString]: IApplication read GetApplication;
    property Client: IClient read GetClient;
    property CurrentUser: IUser read GetCurrentUser;
    property CurrentUserHandle: WideString read GetCurrentUserHandle;
  end;

  TOnMessageStatus = procedure(ASender: TObject; const pMessage: IChatMessage;
    Status: TChatMessageStatus) of object;
  TOnAttachmentStatus = procedure(ASender: TObject; Status: TAttachmentStatus) of object;
  TOnApplicationDatagram = procedure(ASender: TObject; const pApp: IApplication;
    const pStream: IApplicationStream; const Text: WideString) of object;

  ESkype = class(Exception);

  TSkype = class(TInterfacedObject, ISkype)
  private
    m_SkypeAPI: TSkypeAPI;
    m_AttachmentStatus: TAttachmentStatus;
    m_iProtocol: integer;

    m_wstrCurrentUserHandle: WideString;

    m_Command: TCommand;
    m_ListenersManager: TListenersManager;
    m_ChatMessageStatusListener: TListener;
    m_PendingSkypeAPICommands: TStringList;

    m_Applications: TInterfaceList;
    m_Users: TInterfaceList;
    m_Chats: TInterfaceList;

    m_Client: IClient;

    m_FinishAttachmentTimer: TTimer;
    m_PendingSkypeAPICommandsTimer: TTimer;

    FOnMessageStatus: TOnMessageStatus;
    FOnAttachmentStatus: TOnAttachmentStatus;
    FOnApplicationDatagram: TOnApplicationDatagram;

    procedure FSetOnMessageStatus(Value: TOnMessageStatus);

    function GetApplication(const Name: WideString): IApplication;
    function GetClient: IClient;
    function GetCurrentUser: IUser;
    function GetCurrentUserHandle: WideString;

    procedure FCreateFinishAttachmentTimer;
    procedure FDestroyFinishAttachmentTimer;
    procedure FCreatePendingSkypeAPICommandsTimer;
    procedure FDestroyPendingSkypeAPICommandsTimer;

    procedure FOnFinishAttachmentTimer(Sender: TObject);
    procedure FOnPendingSkypeAPICommandsTimer(Sender: TObject);

    procedure FOnSkypeAPIAttachementStatus(ASender: TObject; Status: skypeapi.TAttachmentStatus);
    procedure FOnSkypeAPICommandReceived(ASender: TObject; const wstrCommand: WideString);

    procedure FDoAttachmentStatus(Status: TAttachmentStatus);

    procedure FFinishAttachment;

    procedure FProcessPendingSkypeAPICommandsForListeners;

  public
    constructor Create(const strFriendlyName: string); reintroduce;
    destructor Destroy; override;

    class function Instance: TSkype;

    procedure Attach(Protocol: Integer; Wait: Boolean);
    function SendMessage(const Username: WideString; const Text: WideString): IChatMessage;

    procedure Log(const wstrLogMsg: WideString);
    procedure SendCommand(const wstrCommand: WideString); overload;
    function SendCommand(ACommand: TCommand): boolean; overload;

    function GetUserByHandle(const wstrHandle: WideString): IUser;
    function GetChatWithUser(const wstrUserName: WideString): IChat;

    procedure DoMessageStatus(const pMessage: IChatMessage;
      Status: TChatMessageStatus);
    procedure DoApplicationDatagram(const pApp: IApplication;
      const pStream: IApplicationStream; const Text: WideString);

    property Application[const Name: WideString]: IApplication read GetApplication;
    property Client: IClient read GetClient;
    property CurrentUser: IUser read GetCurrentUser;
    property CurrentUserHandle: WideString read GetCurrentUserHandle;

    property ListenersManager: TListenersManager read m_ListenersManager;

    property OnMessageStatus: TOnMessageStatus read FOnMessageStatus write FSetOnMessageStatus;
    property OnAttachmentStatus: TOnAttachmentStatus read FOnAttachmentStatus
                                                     write FOnAttachmentStatus;
    property OnApplicationDatagram: TOnApplicationDatagram read FOnApplicationDatagram
                                                           write FOnApplicationDatagram;
  end;

implementation

uses
  Forms,
  //
  SkypeAPI_Client, SkypeAPI_Application, SkypeAPI_User, SkypeAPI_ChatMessage,
  SkypeAPI_Chat;

type
  TProtocolCommand = class(TCommand)
  private
    m_iRequestedProtocol: integer;
    m_iReturnedProtocol: integer;
  protected
    function RGetCommand: WideString; override;
    function RProcessResponse(const wstrCommand: WideString): boolean; override;
  public
    constructor Create(iRequestedProtocol: integer);
    property Protocol: integer read m_iReturnedProtocol;
  end;


  TCurrentUserHandleCommand = class(TCommand)
  private
    m_wstrCurrentUserHandle: WideString;
  protected
    function RGetCommand: WideString; override;
    function RProcessResponse(const wstrCommand: WideString): boolean; override;
  public
    property CurrentUserHandle: WideString read m_wstrCurrentUserHandle;
  end;

const
  CMD_PROTOCOL: WideString = 'PROTOCOL';
  CMD_GET_CURRENTUSERHANDLE = 'GET CURRENTUSERHANDLE';
  CMD_CURRENTUSERHANDLE = 'CURRENTUSERHANDLE';

////////////////////////////////////////////////////////////////////////////////
// TSkype

var
  g_SkypeInstance: TSkype = nil;

constructor TSkype.Create(const strFriendlyName: string);
begin
  if (Assigned(g_SkypeInstance)) then
    raise ESkype.Create('TSkype instance already exists!');

  m_SkypeAPI := TSkypeAPI.Create(strFriendlyName);

  inherited Create;

  g_SkypeInstance := self;

  FCreateFinishAttachmentTimer;
  FCreatePendingSkypeAPICommandsTimer;

  m_AttachmentStatus := apiAttachUnknown;
  m_ListenersManager := TListenersManager.Create;
  m_PendingSkypeAPICommands := TStringList.Create;

  m_SkypeAPI.OnAttachmentStatus := FOnSkypeAPIAttachementStatus;
  m_SkypeAPI.OnCommandReceived := FOnSkypeAPICommandReceived;
end;


destructor TSkype.Destroy;
begin
  // TODO: Take care if we're in SendCommand()

  m_PendingSkypeAPICommands.Free;
  m_Applications.Free;
  m_Chats.Free;
  m_Users.Free;

  m_ListenersManager.Free;

  FDestroyPendingSkypeAPICommandsTimer;
  FDestroyFinishAttachmentTimer;

  g_SkypeInstance := nil;

  m_SkypeAPI.Free;

  inherited;
end;


class function TSkype.Instance: TSkype;
begin
  Result := g_SkypeInstance;
end;


function TSkype.GetApplication(const Name: WideString): IApplication;
var
  i: integer;
begin
  Result := nil;

  if (not Assigned(m_Applications)) then
    m_Applications := TInterfaceList.Create;

  for i := 0 to m_Applications.Count - 1 do
  begin
    Result := IApplication(m_Applications[i]);
    if (Result.Name = Name) then
      exit;
  end;

  Result := TApplication.Create(Name);
  m_Applications.Add(Result);
end;


function TSkype.SendMessage(const Username: WideString; const Text: WideString): IChatMessage;
var
  Chat: IChat;
begin
  Chat := GetChatWithUser(Username);
  Result := Chat.SendMessage(Text);
end;


function TSkype.GetClient: IClient;
begin
  if (not Assigned(m_Client)) then
    m_Client := TClient.Create;

  Result := m_Client;
end;


function TSkype.GetCurrentUser: IUser;
begin
  Result := GetUserByHandle(CurrentUserHandle);
end;


function TSkype.GetCurrentUserHandle: WideString;
var
  Command: TCurrentUserHandleCommand;
begin
  if (m_wstrCurrentUserHandle = '') then
  begin
    Command := TCurrentUserHandleCommand.Create;
    try
      if (SendCommand(Command)) then
        m_wstrCurrentUserHandle := Command.CurrentUserHandle;
    finally
      Command.Free;
    end;
  end;

  Result := m_wstrCurrentUserHandle;
end;


procedure TSkype.Attach(Protocol: Integer; Wait: Boolean);
begin
  if (Wait) then
    raise ESkype.Create('Waiting attach not supported!');

  m_iProtocol := Protocol;
  m_SkypeAPI.Attach;
end;


procedure TSkype.FOnSkypeAPIAttachementStatus(ASender: TObject; Status: skypeapi.TAttachmentStatus);
begin
  m_AttachmentStatus := apiAttachAvailable;

  case Status  of
    asAttachSuccess:
    begin
      if (not m_FinishAttachmentTimer.Enabled) then
        m_FinishAttachmentTimer.Enabled := TRUE;
      exit;
    end;

    asAttachPendingAuthorization:
      Log('** Attach pending');

    asAttachRefused:
    begin
      Log('** Attach refused');
      m_AttachmentStatus := apiAttachRefused;
    end;

    asAttachNotAvailable:
    begin
      Log('** Attach unavailable');
      m_AttachmentStatus := apiAttachNotAvailable;
    end;

    asAttachAvailable:
      Log('** Attach available');
  end;

  m_FinishAttachmentTimer.Enabled := FALSE;

  FDoAttachmentStatus(m_AttachmentStatus);
end;


procedure TSkype.FOnSkypeAPICommandReceived(ASender: TObject; const wstrCommand: WideString);
begin
  Log(WideString('->') + wstrCommand);

  if (Assigned(m_Command)) then
  begin
    if (not m_Command.HasResponse) then
    begin
      m_Command.ProcessResponse(wstrCommand);
      if (m_Command.HasResponse) then
        Log('Command processed: ' + m_Command.ClassName)
      else
        m_PendingSkypeAPICommands.Add(UTF8Encode(wstrCommand));
    end;
  end
  else
  begin
    FProcessPendingSkypeAPICommandsForListeners;
    m_ListenersManager.ProcessCommand(wstrCommand);
  end;
end;


procedure TSkype.FProcessPendingSkypeAPICommandsForListeners;
var
  wstrCommand: WideString;
begin
  while (m_PendingSkypeAPICommands.Count > 0) do
  begin
    wstrCommand := UTF8Decode(m_PendingSkypeAPICommands[0]);
    m_PendingSkypeAPICommands.Delete(0);
    m_ListenersManager.ProcessCommand(wstrCommand);
  end;
end;


procedure TSkype.SendCommand(const wstrCommand: WideString);
begin
  Log(WideString('<-') + wstrCommand);
  m_SkypeAPI.SendCommand(wstrCommand);
end;


function TSkype.SendCommand(ACommand: TCommand): boolean;
const
  COMMAND_TIMEOUT = 5000;
var
  iTimeOutTimer: integer;
begin
  Result := FALSE;

  if (Assigned(m_Command)) then
  begin
    Log('Unable to process command: ' + m_Command.ClassName);
    exit;
  end;

  m_Command := ACommand;
  try
    Log('Processing command: ' + m_Command.ClassName);
    SendCommand(m_Command.Command);

    iTimeOutTimer := COMMAND_TIMEOUT;

    while ((not ACommand.HasResponse)) do
    begin
      if (iTimeOutTimer <= 0) then
        exit;
        
      Forms.Application.ProcessMessages;

      if (ACommand.HasResponse) then
        break;
        
      Sleep(1);

      dec(iTimeOutTimer);
    end;

  finally
    m_Command := nil;
  end;

  m_PendingSkypeAPICommandsTimer.Enabled := TRUE;

  Result := TRUE;
end;


procedure TSkype.FOnPendingSkypeAPICommandsTimer(Sender: TObject);
begin
  m_PendingSkypeAPICommandsTimer.Enabled := FALSE;
  if (Assigned(m_Command)) then
    exit;
  FProcessPendingSkypeAPICommandsForListeners;
end;


procedure TSkype.Log(const wstrLogMsg: WideString);
begin
  // TODO: Output to log
end;


procedure TSkype.DoMessageStatus(const pMessage: IChatMessage;
  Status: TChatMessageStatus);
begin
  if (Assigned(FOnMessageStatus)) then
    FOnMessageStatus(self, pMessage, Status);
end;


procedure TSkype.FDoAttachmentStatus(Status: TAttachmentStatus);
begin
  if (Assigned(FOnAttachmentStatus)) then
    FOnAttachmentStatus(self, Status);
end;


procedure TSkype.DoApplicationDatagram(const pApp: IApplication;
  const pStream: IApplicationStream; const Text: WideString);
begin
  if (Assigned(FOnApplicationDatagram)) then
    FOnApplicationDatagram(self, pApp, pStream, Text);
end;


procedure TSkype.FOnFinishAttachmentTimer(Sender: TObject);
begin
  m_FinishAttachmentTimer.Enabled := FALSE;
  FFinishAttachment;
end;


procedure TSkype.FFinishAttachment;
var
  Command: TProtocolCommand;
begin
  Command := TProtocolCommand.Create(999);
  try
    m_AttachmentStatus := apiAttachNotAvailable;
    if (not SendCommand(Command)) then
      exit;
    if (Command.Protocol >= m_iProtocol) then
    begin
      m_AttachmentStatus := apiAttachSuccess;
      Log('** Attach success');
    end;
    FDoAttachmentStatus(m_AttachmentStatus);    
  finally
    Command.Free;
  end;
end;


function TSkype.GetUserByHandle(const wstrHandle: WideString): IUser;
var
  i: integer;
begin
  // It is supposed that a user with a handle wstrHandle is a real user

  Result := nil;

  if (wstrHandle = '') then
    exit;

  if (not Assigned(m_Users)) then
    m_Users := TInterfaceList.Create;

  for i := 0 to m_Users.Count - 1 do
  begin
    Result := IUser(m_Users[i]);
    if (Result.Handle = wstrHandle) then
      exit;
  end;

  Result := TUser.Create(wstrHandle);
  m_Users.Add(Result);
end;


function TSkype.GetChatWithUser(const wstrUserName: WideString): IChat;
var
  strUserName: string;
  i: integer;
begin
  Result := nil;

  strUserName := LowerCase(Trim(wstrUserName));
  if (strUserName = '') then
    exit;

  if (not Assigned(m_Chats)) then
    m_Chats := TInterfaceList.Create;

  for i := 0 to m_Chats.Count - 1 do
  begin
    Result := IChat(m_Chats[i]);
    if ((Result._Object as TChat).UserName = strUserName) then
      exit;
  end;

  Result := TChat.Create(strUserName);
  m_Chats.Add(Result);
end;


procedure TSkype.FSetOnMessageStatus(Value: TOnMessageStatus);
begin
  if (Assigned(FOnMessageStatus)) then
    m_ListenersManager.DestroyListener(m_ChatMessageStatusListener);

  FOnMessageStatus := Value;
  m_ChatMessageStatusListener := m_ListenersManager.CreateListener(TChatMessageStatusListener);
end;


procedure TSkype.FCreateFinishAttachmentTimer;
begin
  if (not Assigned(m_FinishAttachmentTimer)) then
    m_FinishAttachmentTimer := TTimer.Create(nil);
  m_FinishAttachmentTimer.Enabled := FALSE;
  m_FinishAttachmentTimer.Interval := 1000;
  m_FinishAttachmentTimer.OnTimer := FOnFinishAttachmentTimer;
end;


procedure TSkype.FDestroyFinishAttachmentTimer;
begin
  FreeAndNil(m_FinishAttachmentTimer);
end;


procedure TSkype.FCreatePendingSkypeAPICommandsTimer;
begin
  if (not Assigned(m_PendingSkypeAPICommandsTimer)) then
    m_PendingSkypeAPICommandsTimer := TTimer.Create(nil);
  m_PendingSkypeAPICommandsTimer.Enabled := FALSE;
  m_PendingSkypeAPICommandsTimer.Interval := 1;
  m_PendingSkypeAPICommandsTimer.OnTimer := FOnPendingSkypeAPICommandsTimer;
end;


procedure TSkype.FDestroyPendingSkypeAPICommandsTimer;
begin
  FreeAndNil(m_PendingSkypeAPICommandsTimer);
end;

////////////////////////////////////////////////////////////////////////////////
// TProtocolCommand

constructor TProtocolCommand.Create(iRequestedProtocol: integer);
begin
  inherited Create;
  m_iRequestedProtocol := iRequestedProtocol;
end;


function TProtocolCommand.RGetCommand: WideString;
begin
  Result := CMD_PROTOCOL + ' ' + IntToStr(m_iRequestedProtocol);
end;


function TProtocolCommand.RProcessResponse(const wstrCommand: WideString): boolean;
var
  wstrHead, wstrBody: WideString;
begin
  Assert(not HasResponse);

  Result := FALSE;

  RSplitCommandToHeadAndBody(wstrCommand, wstrHead, wstrBody);

  if (wstrHead <> CMD_PROTOCOL) then
    exit;

  m_iReturnedProtocol := StrToInt(wstrBody);

  Result := TRUE;
end;

////////////////////////////////////////////////////////////////////////////////
// TCurrentUserHandleCommand

function TCurrentUserHandleCommand.RGetCommand: WideString;
begin
  Result := CMD_GET_CURRENTUSERHANDLE;
end;


function TCurrentUserHandleCommand.RProcessResponse(const wstrCommand: WideString): boolean;
var
  wstrHead, wstrBody: WideString;
begin
  Assert(not HasResponse);

  Result := FALSE;

  RSplitCommandToHeadAndBody(wstrCommand, wstrHead, wstrBody);

  if (wstrHead <> CMD_CURRENTUSERHANDLE) then
    exit;

  m_wstrCurrentUserHandle := RNextToken(wstrBody, wstrBody);   

  Result := (m_wstrCurrentUserHandle <> '');
end;

end.

