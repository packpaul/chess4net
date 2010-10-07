unit SkypeAPI_Application;

interface

uses
  SkypeAPI_Skype;

type
  TApplication = class(TInterfacedObject, IApplication)
  private
    m_wstrName: WideString;
    m_bCreated: boolean;

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

    property Streams: IApplicationStreamCollection read GetStreams;
    property ConnectableUsers: IUserCollection read GetConnectableUsers;
    property ConnectingUsers: IUserCollection read GetConnectingUsers;
    property Name: WideString read GetName;
  end;

implementation

uses
  SysUtils,
  //
  SkypeAPI_User, SkypeAPI_Command;

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

const
  CMD_APPLICATION: WideString = 'APPLICATION';
  CMD_CREATE_APPLIATION: WideString = 'CREATE APPLICATION';
  CMD_DELETE_APPLIATION: WideString = 'DELETE APPLICATION';
  CMD_GET_APPLICATION: WideString = 'GET APPLICATION';
  CMD_CONNECTABLE: WideString = 'CONNECTABLE';
  CMD_CONNECTING: WideString = 'CONNECTING';

////////////////////////////////////////////////////////////////////////////////
// TApplication

constructor TApplication.Create(const wstrName: WideString);
begin
  inherited Create;
  m_wstrName := wstrName;
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
begin
  // TODO:
end;


procedure TApplication.SendDatagram(const Text: WideString; const pStreams: IApplicationStreamCollection);
begin
  // TODO:
end;


function TApplication.GetStreams: IApplicationStreamCollection;
begin
  Result := nil;
  // TODO:
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

end.