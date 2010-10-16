unit SkypeAPI_Application;

interface

uses
  Classes, SysUtils,
  //
  SkypeAPI_Skype, SkypeAPI_Command;

type
  TApplication = class;
  

  TApplicationStream = class(TInterfaceList, IApplicationStream)
  private
    m_wstrHandle: WideString;
    m_Application: TApplication;
    m_DatagramListener: TListener;
    procedure FCreateListeners;
    procedure FDestroyListeners;
  public
    constructor Create(AApplication: TApplication; const wstrHandle: WideString);
    destructor Destroy; override;
    property Handle: WideString read m_wstrHandle;
  end;


  TApplicationStreamCollection = class(TInterfaceList, IApplicationStreamCollection)
  private
    function FGetCount: integer;
    procedure Add(const Stream: IApplicationStream);
    function FGetItem(iIndex: integer): IApplicationStream;
  public
    property Count: integer read FGetCount;
    property Item[iIndex: Integer]: IApplicationStream read FGetItem; default;
  end;


  EApplication = class(Exception);

  TApplication = class(TInterfacedObject, IApplication)
  private
    m_wstrName: WideString;
    m_bCreated: boolean;
    m_Streams: IApplicationStreamCollection;

    function GetStreams: IApplicationStreamCollection;
    function GetConnectableUsers: IUserCollection;
    function GetConnectingUsers: IUserCollection;
    function GetName: WideString;
  public
    constructor Create(const wstrName: WideString);
    destructor Destroy; override;

    procedure Delete;
    procedure IApplication.Create = _Create;
    procedure _Create;
    procedure Connect(const Username: WideString; WaitConnected: Boolean);
    procedure SendDatagram(const Text: WideString; const pStreams: IApplicationStreamCollection);

    property Streams: IApplicationStreamCollection read m_Streams;
    property ConnectableUsers: IUserCollection read GetConnectableUsers;
    property ConnectingUsers: IUserCollection read GetConnectingUsers;
    property Name: WideString read GetName;
  end;

implementation

uses
  Forms,
  //
  SkypeAPI_User;

type
  TAppCommand = class(TCommand)
  private
    m_Application: TApplication;
  protected
    property Application: TApplication read m_Application;
  public
    constructor Create(AApplication: TApplication);
  end;


  TAppCreateCommand = class(TAppCommand)
  protected
    function RGetCommand: WideString; override;
    function RProcessResponse(const wstrCommand: WideString): boolean; override;
  end;


  TAppDeleteCommand = class(TAppCommand)
  protected
    function RGetCommand: WideString; override;
    function RProcessResponse(const wstrCommand: WideString): boolean; override;
  end;


  TAppConnectableUsersCommand = class(TAppCommand)
  private
    m_Users: IUserCollection;
    procedure FBuildUserCollection(wstrUserHandles: WideString);
  protected
    function RGetCommand: WideString; override;
    function RProcessResponse(const wstrCommand: WideString): boolean; override;
  public
    procedure GetUsers(out Users: IUserCollection);
  end;


  TAppConnectCommand = class(TAppCommand)
  private
    m_wstrUserName: WideString;
  protected
    function RGetCommand: WideString; override;
    function RProcessResponse(const wstrCommand: WideString): boolean; override;
  public
    constructor Create(AApplication: TApplication; const wstrUserName: WideString);
  end;


  TAppConnectingUsersCommand = class(TAppCommand)
  private
    m_Users: IUserCollection;
    procedure FBuildUserCollection(wstrUserHandles: WideString);
  protected
    function RGetCommand: WideString; override;
    function RProcessResponse(const wstrCommand: WideString): boolean; override;
  public
    procedure GetUsers(out Users: IUserCollection);
  end;


  TAppConnectingAndStreamsListener = class(TListener)
  private
    m_State: (sConnectionEstablishing, sConnectionEstablished, sMonitoringStreams);

    m_wstrUserName: WideString;
    m_Application: TApplication;

    m_bStreamAquiredFlag: boolean;

    m_wstrParsedStreamHandle: WideString;

    function FConnectionEstablishingParseCommand(const wstrCommand: WideString): boolean;
    function FConnectionEstablishedParseCommand(const wstrCommand: WideString): boolean;
    function FMonitoringStreamsParseCommand(const wstrCommand: WideString): boolean;

    procedure FMonitoringStreamsProcessCommand;

  protected
    constructor RCreate; override;
    function RParseCommand(const wstrCommand: WideString): boolean; override;
    function RProcessCommand(const wstrCommand: WideString): boolean; override;

  public
    property UserName: WideString read m_wstrUserName write m_wstrUserName;
    property Application: TApplication read m_Application write m_Application;
    property StreamAquiredFlag: boolean read m_bStreamAquiredFlag;
  end;


  TAppSendDatagramCommand = class(TAppCommand)
  private
    m_Stream: TApplicationStream;
    m_wstrText: WideString;
  protected
    function RGetCommand: WideString; override;
    function RProcessResponse(const wstrCommand: WideString): boolean; override;
  public
    constructor Create(AApplication: TApplication;
      AStream: TApplicationStream; const wstrText: WideString);
  end;


  TAppStreamDatagramListener = class(TListener)
  private
    m_Application: TApplication;
    m_Stream: TApplicationStream;

    m_wstrParsedText: WideString;
    m_wstrText: WideString;
  protected
    function RParseCommand(const wstrCommand: WideString): boolean; override;
    function RProcessCommand(const wstrCommand: WideString): boolean; override;
    procedure RDoNotify; override;
  public
    property Application: TApplication read m_Application write m_Application;
    property Stream: TApplicationStream read m_Stream write m_Stream;
  end;

const
  CMD_APPLICATION: WideString = 'APPLICATION';
  CMD_CREATE_APPLIATION: WideString = 'CREATE APPLICATION';
  CMD_DELETE_APPLIATION: WideString = 'DELETE APPLICATION';
  CMD_GET_APPLICATION: WideString = 'GET APPLICATION';
  CMD_ALTER_APPLICATION: WideString = 'ALTER APPLICATION';
  CMD_CONNECTABLE: WideString = 'CONNECTABLE';
  CMD_CONNECTING: WideString = 'CONNECTING';
  CMD_CONNECT: WideString = 'CONNECT';
  CMD_STREAMS: WideString = 'STREAMS';
  CMD_DATAGRAM: WideString = 'DATAGRAM';

////////////////////////////////////////////////////////////////////////////////
// TApplication

constructor TApplication.Create(const wstrName: WideString);
begin
  inherited Create;
  m_wstrName := wstrName;
  m_Streams := TApplicationStreamCollection.Create;
end;


destructor TApplication.Destroy;
begin
  inherited;
end;


procedure TApplication._Create;
var
  Command: TAppCreateCommand;
begin
  if (m_bCreated) then
    exit;

  Command := TAppCreateCommand.Create(self);
  try
    if (TSkype.Instance.SendCommand(Command)) then
      m_bCreated := TRUE;
  finally
    Command.Free;
  end;
end;


procedure TApplication.Delete;
var
  Command: TAppDeleteCommand;
begin
  if (not m_bCreated) then
    exit;

  Command := TAppDeleteCommand.Create(self);
  try
    if (TSkype.Instance.SendCommand(Command)) then
      m_bCreated := FALSE;
  finally
    Command.Free;
  end;
end;


procedure TApplication.Connect(const Username: WideString; WaitConnected: Boolean);
const
  WAIT_CONNECTED_TIMEOUT = 10000;
var
  strUserName: string;
  Command: TAppConnectCommand;
  Listener: TAppConnectingAndStreamsListener;
  iTimeOutTimer: integer;
begin
  if (not m_bCreated) then
    exit;

  strUserName := LowerCase(Trim(UserName));
  if (strUserName = '') then
    exit;

  Command := TAppConnectCommand.Create(self, strUserName);
  try
    if (TSkype.Instance.SendCommand(Command)) then
      ;
  finally
    Command.Free;
  end;

  Listener := TSkype.Instance.ListenersManager.CreateListener(
    TAppConnectingAndStreamsListener) as TAppConnectingAndStreamsListener;
  Listener.UserName := strUserName;
  Listener.Application := self;

  if (not WaitConnected) then
    exit;

  iTimeOutTimer := WAIT_CONNECTED_TIMEOUT;

  while ((not Listener.StreamAquiredFlag)) do
  begin
    if (iTimeOutTimer <= 0) then
    begin
      TSkype.Instance.Log('TApplication.Connect(): timeout occured!');
      exit;
    end;

    Forms.Application.ProcessMessages;
    Sleep(1);
    
    dec(iTimeOutTimer);
  end;
end;


procedure TApplication.SendDatagram(const Text: WideString; const pStreams: IApplicationStreamCollection);
var
  Streams: TApplicationStreamCollection;
  Stream: TApplicationStream;
  i: integer;
  Command: TAppSendDatagramCommand;
begin
  Streams := TApplicationStreamCollection(pStreams);
  for i := 0 to Streams.Count - 1 do
  begin
    Stream := TApplicationStream(Streams[i]);
    Command := TAppSendDatagramCommand.Create(self, Stream, Text);
    try
      if (TSkype.Instance.SendCommand(Command)) then
      begin
        // Skype will returns in turn:
        // "APPLICATION <app. name> SENDING <stream handle>=11" - when data is transmited
        // "APPLICATION <app. name> SENDING" - when data is sent
        // Use a listener for additional notification for the client
      end;
    finally
      Command.Free;
    end;
    
  end;
end;


function TApplication.GetStreams: IApplicationStreamCollection;
begin
  Result := m_Streams;
end;


function TApplication.GetConnectableUsers: IUserCollection;
var
  Command: TAppConnectableUsersCommand;
begin
  Result := nil;

  Command := TAppConnectableUsersCommand.Create(self);
  try
    if (TSkype.Instance.SendCommand(Command)) then
      Command.GetUsers(Result)
    else
      Result := TUserCollection.Create; // Empty collection 
  finally
    Command.Free;
  end;
end;


function TApplication.GetConnectingUsers: IUserCollection;
var
  Command: TAppConnectingUsersCommand;
begin
  Result := nil;

  Command := TAppConnectingUsersCommand.Create(self);
  try
    if (TSkype.Instance.SendCommand(Command)) then
      Command.GetUsers(Result)
    else
      Result := TUserCollection.Create; // Empty collection
  finally
    Command.Free;
  end;
end;


function TApplication.GetName: WideString;
begin
  Result := m_wstrName;
end;

////////////////////////////////////////////////////////////////////////////////
// TAppCommand

constructor TAppCommand.Create(AApplication: TApplication);
begin
  inherited Create;
  m_Application := AApplication;
end;

////////////////////////////////////////////////////////////////////////////////
// TAppCreateCommand

function TAppCreateCommand.RGetCommand: WideString;
begin
  Result := CMD_CREATE_APPLIATION + ' ' + Application.Name;
end;


function TAppCreateCommand.RProcessResponse(const wstrCommand: WideString): boolean;
begin
  Assert(not HasResponse);
  
  Result := SameText(Command, wstrCommand);
{
  Other responses:
  ERROR 536 CREATE: no object or type given
  ERROR 537 CREATE: Unknown object type given
  ERROR 540 CREATE APPLICATION: Missing or invalid name
  ERROR 541 APPLICATION: operation failed - in case an application with this name already exists
}
end;

////////////////////////////////////////////////////////////////////////////////
// TAppDeleteCommand

function TAppDeleteCommand.RGetCommand: WideString;
begin
  Result := CMD_DELETE_APPLIATION + ' ' + Application.Name;
end;


function TAppDeleteCommand.RProcessResponse(const wstrCommand: WideString): boolean;
begin
  Assert(not HasResponse);

  Result := SameText(Command, wstrCommand);
{
  Other responses:
  ERROR 538 DELETE: no object or type given
  ERROR 539 DELETE: Unknown object type given
  ERROR 542 DELETE APPLICATION : missing or invalid application name
  ERROR 541 APPLICATION: operation failed
}
end;

////////////////////////////////////////////////////////////////////////////////
// TAppConnectableUsersCommand

function TAppConnectableUsersCommand.RGetCommand: WideString;
begin
  Result := CMD_GET_APPLICATION + ' ' + Application.Name + ' ' + CMD_CONNECTABLE;
end;


function TAppConnectableUsersCommand.RProcessResponse(const wstrCommand: WideString): boolean;
var
  wstrHead, wstrBody: WideString;
  wstrAppName: WideString;
begin
  Assert(not HasResponse);

  Result := FALSE;

  RSplitCommandToHeadAndBody(wstrCommand, wstrHead, wstrBody);
  if (wstrHead <> CMD_APPLICATION) then
    exit;

  wstrAppName := RNextToken(wstrBody, wstrBody);
  if (wstrAppName <> Application.Name) then
    exit;

  RSplitCommandToHeadAndBody(wstrBody, wstrHead, wstrBody);
  if (wstrHead <> CMD_CONNECTABLE) then
    exit;

  FBuildUserCollection(wstrBody);

  Result := TRUE;
end;


procedure TAppConnectableUsersCommand.FBuildUserCollection(wstrUserHandles: WideString);
var
  wstrHandle: WideString;
  User: IUser;
  Users: TUserCollection;
begin
  Assert(not Assigned(m_Users));

  Users := TUserCollection.Create;

  wstrHandle := RNextToken(wstrUserHandles, wstrUserHandles);
  while (wstrHandle <> '') do
  begin
    User := TSkype.Instance.GetUserByHandle(wstrHandle);
    if (Assigned(User)) then
      Users.Add(User);
    wstrHandle := RNextToken(wstrUserHandles, wstrUserHandles);
  end;

  m_Users := Users;
end;


procedure TAppConnectableUsersCommand.GetUsers(out Users: IUserCollection);
begin
  Users := m_Users;
  m_Users := nil;
end;

////////////////////////////////////////////////////////////////////////////////
// TAppConnectingUsersCommand

function TAppConnectingUsersCommand.RGetCommand: WideString;
begin
  Result := CMD_GET_APPLICATION + ' ' + Application.Name + ' ' + CMD_CONNECTING;
end;


function TAppConnectingUsersCommand.RProcessResponse(const wstrCommand: WideString): boolean;
var
  wstrHead, wstrBody: WideString;
  wstrAppName: WideString;
begin
  Assert(not HasResponse);

  Result := FALSE;

  RSplitCommandToHeadAndBody(wstrCommand, wstrHead, wstrBody);
  if (wstrHead <> CMD_APPLICATION) then
    exit;

  wstrAppName := RNextToken(wstrBody, wstrBody);
  if (wstrAppName <> Application.Name) then
    exit;

  RSplitCommandToHeadAndBody(wstrBody, wstrHead, wstrBody);
  if (wstrHead <> CMD_CONNECTING) then
    exit;

  FBuildUserCollection(wstrBody);

  Result := TRUE;
end;


procedure TAppConnectingUsersCommand.FBuildUserCollection(wstrUserHandles: WideString);
var
  wstrHandle: WideString;
  User: IUser;
  Users: TUserCollection;
begin
  Assert(not Assigned(m_Users));

  Users := TUserCollection.Create;

  wstrHandle := RNextToken(wstrUserHandles, wstrUserHandles);
  while (wstrHandle <> '') do
  begin
    User := TSkype.Instance.GetUserByHandle(wstrHandle);
    if (Assigned(User)) then
      Users.Add(User);
    wstrHandle := RNextToken(wstrUserHandles, wstrUserHandles);
  end;

  m_Users := Users;
end;


procedure TAppConnectingUsersCommand.GetUsers(out Users: IUserCollection);
begin
  Users := m_Users;
  m_Users := nil;
end;

////////////////////////////////////////////////////////////////////////////////
// TAppConnectCommand

constructor TAppConnectCommand.Create(AApplication: TApplication; const wstrUserName: WideString);
begin
  inherited Create(AApplication);
  m_wstrUserName := wstrUserName;
end;


function TAppConnectCommand.RGetCommand: WideString;
begin
  Result := CMD_ALTER_APPLICATION + m_Application.Name + ' ' + CMD_CONNECT + ' ' + m_wstrUserName;
end;


function TAppConnectCommand.RProcessResponse(const wstrCommand: WideString): boolean;
var
  wstrHead, wstrBody: WideString;
  wstrAppName, wstrUserName: WideString;
begin
  Assert(not HasResponse);

  Result := FALSE;

  RSplitCommandToHeadAndBody(wstrCommand, wstrHead, wstrBody, 2);
  if (wstrHead <> CMD_ALTER_APPLICATION) then
    exit;

  wstrAppName := RNextToken(wstrBody, wstrBody);
  if (wstrAppName <> Application.Name) then
    exit;

  RSplitCommandToHeadAndBody(wstrBody, wstrHead, wstrBody);
  if (wstrHead <> CMD_CONNECT) then
    exit;

  wstrUserName := RNextToken(wstrBody, wstrBody);
  if (wstrAppName <> m_wstrUserName) then
    exit;

  Result := TRUE;
end;

////////////////////////////////////////////////////////////////////////////////
// TAppConnectingAndStreamsListener

constructor TAppConnectingAndStreamsListener.RCreate;
begin
  inherited Create;
  m_State := sConnectionEstablishing;  
end;


function TAppConnectingAndStreamsListener.RParseCommand(const wstrCommand: WideString): boolean;
begin
  Result := FALSE;

  case m_State of
    sConnectionEstablishing:
      Result := FConnectionEstablishingParseCommand(wstrCommand);
    sConnectionEstablished:
      Result := FConnectionEstablishedParseCommand(wstrCommand);
    sMonitoringStreams:
      Result := FMonitoringStreamsParseCommand(wstrCommand);
  end;
end;


function TAppConnectingAndStreamsListener.FConnectionEstablishingParseCommand(
  const wstrCommand: WideString): boolean;
var
  wstrHead, wstrBody: WideString;
  wstrAppName, wstrUserName: WideString;
begin
  Result := FALSE;

  RSplitCommandToHeadAndBody(wstrCommand, wstrHead, wstrBody);
  if (wstrHead <> CMD_APPLICATION) then
    exit;

  wstrAppName := RNextToken(wstrBody, wstrBody);
  if (wstrAppName <> Application.Name) then
    exit;

  RSplitCommandToHeadAndBody(wstrBody, wstrHead, wstrBody);
  if (wstrHead <> CMD_CONNECTING) then
    exit;

  wstrUserName := RNextToken(wstrBody, wstrBody);
  if (wstrUserName <> m_wstrUserName) then
    exit;

  Result := TRUE;
end;


function TAppConnectingAndStreamsListener.FConnectionEstablishedParseCommand(
  const wstrCommand: WideString): boolean;
var
  wstrHead, wstrBody: WideString;
  wstrAppName: WideString;
begin
  Result := FALSE;

  RSplitCommandToHeadAndBody(wstrCommand, wstrHead, wstrBody);
  if (wstrHead <> CMD_APPLICATION) then
    exit;

  wstrAppName := RNextToken(wstrBody, wstrBody);
  if (wstrAppName <> Application.Name) then
    exit;

  RSplitCommandToHeadAndBody(wstrBody, wstrHead, wstrBody);
  if (wstrHead <> CMD_CONNECTING) then
    exit;

  if (RNextToken(wstrBody, wstrBody) <> '') then
    exit;

  Result := TRUE;
end;


function TAppConnectingAndStreamsListener.FMonitoringStreamsParseCommand(
  const wstrCommand: WideString): boolean;
var
  wstrHead, wstrBody: WideString;
  wstrAppName: WideString;
begin
  Result := FALSE;

  RSplitCommandToHeadAndBody(wstrCommand, wstrHead, wstrBody);
  if (wstrHead <> CMD_APPLICATION) then
    exit;

  wstrAppName := RNextToken(wstrBody, wstrBody);
  if (wstrAppName <> Application.Name) then
    exit;

  RSplitCommandToHeadAndBody(wstrBody, wstrHead, wstrBody);
  if (wstrHead <> CMD_STREAMS) then
    exit;

  m_wstrParsedStreamHandle := RNextToken(wstrBody, wstrBody);

  Result := TRUE;
end;


function TAppConnectingAndStreamsListener.RProcessCommand(const wstrCommand: WideString): boolean;
begin
  Result := RParseCommand(wstrCommand);

  if (not Result) then
    exit;

  case m_State of
    sConnectionEstablishing:
      m_State := sConnectionEstablished;

    sConnectionEstablished:
      m_State := sMonitoringStreams;

    sMonitoringStreams:
      FMonitoringStreamsProcessCommand;
  end;

end;


procedure TAppConnectingAndStreamsListener.FMonitoringStreamsProcessCommand;
var
  wstr: WideString;
  strHandle: string;
  Streams: TApplicationStreamCollection;
  i: integer;
  Stream: TApplicationStream;
  iIndex: integer;
  DummyListener: TListener;
begin
  with TStringList.Create do
  try
    strHandle := RNextToken(m_wstrParsedStreamHandle, wstr);
    while (strHandle <> '') do
    begin
      Add(strHandle);
      strHandle := RNextToken(wstr, wstr);
    end;

    Streams := TApplicationStreamCollection(Application.Streams);

    i := Streams.Count - 1;
    while (i >= 0) do
    begin
      Stream := TApplicationStream(Streams[i]);
      if (Find(Stream.Handle, iIndex)) then
        Streams.Delete(i)
      else
      begin
        strHandle := Strings[i];
        Streams.Add(TApplicationStream.Create(m_Application, strHandle));
        m_bStreamAquiredFlag := TRUE;
      end;
      dec(i);
    end;

  finally
    Free;
  end;

  if (Streams.Count = 0) then
  begin
    DummyListener := self;
    TSkype.Instance.ListenersManager.DestroyListener(DummyListener);
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TApplicationStream

constructor TApplicationStream.Create(AApplication: TApplication;
  const wstrHandle: WideString);
begin
  inherited Create;
  m_Application := AApplication;
  m_wstrHandle := wstrHandle;
  FCreateListeners;
end;


destructor TApplicationStream.Destroy;
begin
  FDestroyListeners;
  inherited;
end;

procedure TApplicationStream.FCreateListeners;
begin
  m_DatagramListener := TSkype.Instance.ListenersManager.CreateListener(
    TAppStreamDatagramListener);
end;


procedure TApplicationStream.FDestroyListeners;
begin
  TSkype.Instance.ListenersManager.DestroyListener(m_DatagramListener);
end;

////////////////////////////////////////////////////////////////////////////////
// TApplicationStreamCollection

function TApplicationStreamCollection.FGetCount: Integer;
begin
  Result := inherited Count;
end;


procedure TApplicationStreamCollection.Add(const Stream: IApplicationStream);
begin
  inherited Add(Stream);
end;


function TApplicationStreamCollection.FGetItem(iIndex: Integer): IApplicationStream;
begin
  Result := IApplicationStream(inherited Items[iIndex]);
end;

////////////////////////////////////////////////////////////////////////////////
// TAppSendDatagramCommand

constructor TAppSendDatagramCommand.Create(AApplication: TApplication;
  AStream: TApplicationStream; const wstrText: WideString);
begin
  inherited Create(AApplication);
  m_Stream := AStream;
  m_wstrText := wstrText;
end;

function TAppSendDatagramCommand.RGetCommand: WideString;
begin
  Result := CMD_ALTER_APPLICATION + ' ' + Application.Name + ' ' + CMD_DATAGRAM + ' ' +
    m_Stream.Handle + ' ' + m_wstrText;
end;


function TAppSendDatagramCommand.RProcessResponse(const wstrCommand: WideString): boolean;
var
  wstrHead, wstrBody: WideString;
  wstrAppName, wstrStreamHandle: WideString;
begin
  Assert(not HasResponse);

  Result := FALSE;

  RSplitCommandToHeadAndBody(wstrCommand, wstrHead, wstrBody, 2);
  if (wstrHead <> CMD_ALTER_APPLICATION) then
    exit;

  wstrAppName := RNextToken(wstrBody, wstrBody);
  if (wstrAppName <> Application.Name) then
    exit;

  RSplitCommandToHeadAndBody(wstrBody, wstrHead, wstrBody);
  if (wstrHead <> CMD_DATAGRAM) then
    exit;

  wstrStreamHandle := RNextToken(wstrBody, wstrBody);
  if (wstrStreamHandle <> m_Stream.Handle) then
    exit;

  Result := TRUE;
end;

////////////////////////////////////////////////////////////////////////////////
// TAppStreamDatagramListener

function TAppStreamDatagramListener.RParseCommand(const wstrCommand: WideString): boolean;
var
  wstrHead, wstrBody: WideString;
  wstrAppName, wstrStreamHandle: WideString;
begin
  Result := FALSE;

  RSplitCommandToHeadAndBody(wstrCommand, wstrHead, wstrBody);
  if (wstrHead <> CMD_APPLICATION) then
    exit;

  wstrAppName := RNextToken(wstrBody, wstrBody);
  if (wstrAppName <> m_Application.Name) then
    exit;

  RSplitCommandToHeadAndBody(wstrBody, wstrHead, wstrBody);
  if (wstrHead <> CMD_DATAGRAM) then
    exit;

  wstrStreamHandle := RNextToken(wstrBody, wstrBody);
  if (wstrStreamHandle <> m_Stream.Handle) then
    exit;

  m_wstrParsedText := wstrBody;

  Result := TRUE;
end;


function TAppStreamDatagramListener.RProcessCommand(const wstrCommand: WideString): boolean;
begin
  Result := RParseCommand(wstrCommand);

  if (not Result) then
    exit;

  m_wstrText := m_wstrParsedText;
end;


procedure TAppStreamDatagramListener.RDoNotify;
begin
  TSkype.Instance.DoApplicationDatagram(m_Application, m_Stream, m_wstrText); 
end;

end.
