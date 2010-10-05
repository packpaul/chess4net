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
  TAppCreateCommand = class(TCommand)
  private
    m_Application: TApplication;
  protected
    function RGetCommand: WideString; override;
    function RProcessResponse(const wstrCommand: WideString): boolean; override;
  public
    constructor Create(AApplication: TApplication);
  end;

  TAppDeleteCommand = class(TCommand)
  private
    m_Application: TApplication;
  protected
    function RGetCommand: WideString; override;
    function RProcessResponse(const wstrCommand: WideString): boolean; override;
  public
    constructor Create(AApplication: TApplication);
  end;

const
  CMD_CREATE_APPLIATION: WideString = 'CREATE APPLICATION';
  CMD_DELETE_APPLIATION: WideString = 'DELETE APPLICATION';

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
  AppCreateCommand: TAppCreateCommand;
begin
  AppCreateCommand := TAppCreateCommand.Create(self);
  try
    TSkype.Instance.SendCommand(AppCreateCommand);
  finally
    AppCreateCommand.Free;
  end;
end;


procedure TApplication.Delete;
var
  AppDeleteCommand: TAppDeleteCommand;
begin
  AppDeleteCommand := TAppDeleteCommand.Create(self);
  try
    TSkype.Instance.SendCommand(AppDeleteCommand);
  finally
    AppDeleteCommand.Free;
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
begin
  Result := TUserCollection.Create;
  // TODO:
end;


function TApplication.GetConnectingUsers: IUserCollection;
begin
  Result := TUserCollection.Create;
  // TODO:  
end;


function TApplication.GetName: WideString;
begin
  Result := m_wstrName;
end;

////////////////////////////////////////////////////////////////////////////////
// TAppCreateCommand

constructor TAppCreateCommand.Create(AApplication: TApplication);
begin
  inherited Create;
  m_Application := AApplication;
end;


function TAppCreateCommand.RGetCommand: WideString;
begin
  Result := CMD_CREATE_APPLIATION + ' ' + m_Application.Name;
end;


function TAppCreateCommand.RProcessResponse(const wstrCommand: WideString): boolean;
begin
  Result := SameText(Command, wstrCommand);
  if (Result) then
    m_Application.m_bCreated := TRUE;
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

constructor TAppDeleteCommand.Create(AApplication: TApplication);
begin
  inherited Create;
  m_Application := AApplication;
end;


function TAppDeleteCommand.RGetCommand: WideString;
begin
  Result := CMD_DELETE_APPLIATION + ' ' + m_Application.Name;
end;


function TAppDeleteCommand.RProcessResponse(const wstrCommand: WideString): boolean;
begin
  Result := SameText(Command, wstrCommand);
  if (Result) then
    m_Application.m_bCreated := FALSE;
{
  Other responses:
  ERROR 538 DELETE: no object or type given
  ERROR 539 DELETE: Unknown object type given
  ERROR 542 DELETE APPLICATION : missing or invalid application name
  ERROR 541 APPLICATION: operation failed
}
end;

end.