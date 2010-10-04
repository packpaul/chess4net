unit SkypeAPI_Skype;

interface

uses
  Classes, SysUtils,
  //
  skypeapi;

type
  TAttachmentStatus = (apiAttachUnknown = -1, apiAttachSuccess = 0,
    apiAttachRefused = 2, apiAttachNotAvailable = 3, apiAttachAvailable = 4);
  TChatMessageStatus = (cmsUnknown = -1, cmsReceived = 2);

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

  IApplicationStream = interface
  end;

  IApplicationStreamCollection = interface
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

  TSkype = class(TDataModule, ISkype)
    procedure DataModuleCreate(Sender: TObject);
  private
    m_SkypeAPI: TSkypeAPI;
    m_AttachmentStatus: TAttachmentStatus;
    m_iProtocol: integer;

    FOnMessageStatus: TOnMessageStatus;
    FOnAttachmentStatus: TOnAttachmentStatus;
    FOnApplicationDatagram: TOnApplicationDatagram;

    function GetApplication(const Name: WideString): IApplication;
    function GetClient: IClient;
    function GetCurrentUser: IUser;
    function GetCurrentUserHandle: WideString;

    procedure FOnSkypeAPIAttachementStatus(ASender: TObject; Status: skypeapi.TAttachmentStatus);
    procedure FOnSkypeAPICommandReceived(ASender: TObject; const wstrCommand: WideString);

    procedure FDoMessageStatus(ASender: TObject; const pMessage: IChatMessage;
      Status: TChatMessageStatus);
    procedure FDoAttachmentStatus(ASender: TObject; Status: TAttachmentStatus);
    procedure FDoApplicationDatagram(ASender: TObject; const pApp: IApplication;
      const pStream: IApplicationStream; const Text: WideString);

    procedure FLog(const wstrLogMsg: WideString);
    function FSendCommand(const wstrCommand: WideString; bBlocking: boolean): boolean;

  public
    constructor Create(const strFriendlyName: string);
    destructor Destroy; override;

    procedure Attach(Protocol: Integer; Wait: Boolean);
    function SendMessage(const Username: WideString; const Text: WideString): IChatMessage;

    property Application[const Name: WideString]: IApplication read GetApplication;
    property Client: IClient read GetClient;
    property CurrentUser: IUser read GetCurrentUser;
    property CurrentUserHandle: WideString read GetCurrentUserHandle;

    property OnMessageStatus: TOnMessageStatus read FOnMessageStatus write FOnMessageStatus;
    property OnAttachmentStatus: TOnAttachmentStatus read FOnAttachmentStatus
                                                     write FOnAttachmentStatus;
    property OnApplicationDatagram: TOnApplicationDatagram read FOnApplicationDatagram
                                                           write FOnApplicationDatagram;
  end;

implementation

uses
  SkypeAPI_Command;

{$R *.dfm}

////////////////////////////////////////////////////////////////////////////////
// TSkype

constructor TSkype.Create(const strFriendlyName: string);
begin
  m_SkypeAPI := TSkypeAPI.Create(strFriendlyName);
  inherited Create(nil);
end;


destructor TSkype.Destroy;
begin
  m_SkypeAPI.Free;
  inherited;
end;


function TSkype.GetApplication(const Name: WideString): IApplication;
begin
  Result := nil;
  // TODO:
end;


function TSkype.SendMessage(const Username: WideString; const Text: WideString): IChatMessage;
begin
  Result := nil;
  // TODO:
end;


function TSkype.GetClient: IClient;
begin
  Result := nil;
  // TODO:
end;


function TSkype.GetCurrentUser: IUser;
begin
  Result := nil;
  // TODO:
end;


function TSkype.GetCurrentUserHandle: WideString;
begin
  Result := '';
  // TODO:
end;


procedure TSkype.Attach(Protocol: Integer; Wait: Boolean);
begin
  if (Wait) then
    raise ESkype.Create('Waiting attach not supported!');

  m_iProtocol := Protocol;
  m_SkypeAPI.Attach;
end;


procedure TSkype.FOnSkypeAPIAttachementStatus(ASender: TObject; Status: skypeapi.TAttachmentStatus);
var
  iRequestedProtocol: integer; 
begin
  ASender := ASender; // To avoid warning

  m_AttachmentStatus := apiAttachAvailable;

  case Status  of
    asAttachSuccess:
    begin
      FLog('** Attach success');
      iRequestedProtocol := m_iProtocol;
      FSendCommand(TCommand.Protocol(999), TRUE); // ???
      if (iRequestedProtocol >= m_iProtocol) then
        m_AttachmentStatus := apiAttachSuccess
      else
        m_AttachmentStatus := apiAttachNotAvailable;
    end;

    asAttachPendingAuthorization:
      FLog('** Attach pending');

    asAttachRefused:
    begin
      FLog('** Attach refused');
      m_AttachmentStatus := apiAttachRefused;
    end;

    asAttachNotAvailable:
    begin
      FLog('** Attach unavailable');
      m_AttachmentStatus := apiAttachNotAvailable;
    end;

    asAttachAvailable:
      FLog('** Attach available');
  end;

  FDoAttachmentStatus(self, m_AttachmentStatus);
end;


procedure TSkype.FOnSkypeAPICommandReceived(ASender: TObject; const wstrCommand: WideString);
begin
  // TODO:
end;


function TSkype.FSendCommand(const wstrCommand: WideString; bBlocking: boolean): boolean;
begin
  if (not bBlocking) then
    raise ESkype.Create('Non-blocking commands sending not supported!');
  FLog(WideString('<-') + wstrCommand);
  m_SkypeAPI.SendCommand(wstrCommand);
end;


procedure TSkype.FLog(const wstrLogMsg: WideString);
begin
  // TODO: Output to log
end;


procedure TSkype.FDoMessageStatus(ASender: TObject; const pMessage: IChatMessage;
  Status: TChatMessageStatus);
begin
  if (Assigned(FOnMessageStatus)) then
    FOnMessageStatus(ASender, pMessage, Status);
end;


procedure TSkype.FDoAttachmentStatus(ASender: TObject; Status: TAttachmentStatus);
begin
  if (Assigned(FOnAttachmentStatus)) then
    FOnAttachmentStatus(ASender, Status);
end;


procedure TSkype.FDoApplicationDatagram(ASender: TObject; const pApp: IApplication;
  const pStream: IApplicationStream; const Text: WideString);
begin
  if (Assigned(FOnApplicationDatagram)) then
    FOnApplicationDatagram(ASender, pApp, pStream, Text);
end;

procedure TSkype.DataModuleCreate(Sender: TObject);
begin
  m_AttachmentStatus := apiAttachUnknown;
  
  m_SkypeAPI.OnAttachmentStatus := FOnSkypeAPIAttachementStatus;  
  m_SkypeAPI.OnCommandReceived := FOnSkypeAPICommandReceived;
end;

end.

