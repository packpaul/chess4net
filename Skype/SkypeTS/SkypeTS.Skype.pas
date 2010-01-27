unit SkypeTS.Skype;

interface

uses
  ComObj, ActiveX, AxCtrls, Classes, StdVcl, ExtCtrls,
  SkypeTS_TLB;

type
  TState = (sAttached);
  TStates = set of TState;

  TOnInstantMessageReceived = procedure(Sender: TObject; const wstrMessage: WideString) of object;

  TSkype = class(TAutoObject, IConnectionPointContainer, ISkype)
  private
    { Private declarations }
    FConnectionPoints: TConnectionPoints;
    FConnectionPoint: TConnectionPoint;
    FEvents: ISkypeEvents;
    { note: FEvents maintains a *single* event sink. For access to more
      than one event sink, use FConnectionPoint.SinkList, and iterate
      through the list of sinks. }

    m_iID: integer;

    m_Client: IClient;
    m_CurrentUser: IUser;

    m_States: TStates;
    m_AttachTimer: TTimer;
    m_bCanAttach: boolean;

    m_Applications: TInterfaceList;

    FOnInstantMessageReceived: TOnInstantMessageReceived;


    procedure FAttachID;
    procedure FDetachID;

    procedure FCreateControls;
    procedure FDestroyControls;

    procedure FOnAttachTimer(Sender: TObject);

    procedure FDeleteApplication(const wstrApplicationName: WideString);
    procedure FWaitForAllAttached;

    procedure FDoReceiveInstantMessage(const ChatMessage: IChatMessage);

    procedure Attach(Protocol: Integer; Wait: WordBool); safecall;
    function Get_Application(const Name: WideString): IApplication; safecall;
    function Get_Client: IClient; safecall;
    function SendMessage(const Username: WideString; const Text: WideString): IChatMessage; safecall;
    function Get_CurrentUser: IUser; safecall;
    function Get_CurrentUserHandle: WideString; safecall;

  protected
    { Protected declarations }
    property ConnectionPoints: TConnectionPoints read FConnectionPoints
      implements IConnectionPointContainer;
    procedure EventSinkChanged(const EventSink: IUnknown); override;

  public
    destructor Destroy; override;
    procedure Initialize; override;
    procedure DoAttach(const wstrHandle, wstrFullName, wstrDisplayName: WideString);
    procedure SendInstantMessge(const wstrSkypeHandle, wstrMessage: WideString);

    property ID: integer read m_iID;
    property States: TStates read m_States;
    property CurrentUserHandle: WideString read Get_CurrentUserHandle;

    property OnInstantMessageReceived: TOnInstantMessageReceived read FOnInstantMessageReceived
                                                                 write FOnInstantMessageReceived;
  end;

var
  g_arrSkypes: array of TSkype;

implementation

uses
  SysUtils, Forms, ComServ,
  SkypeTS.MainForm;

const
  MAX_SKYPES_NUM = 2;

type
  TAutoObjectFactory_ = class(TAutoObjectFactory)
  public
    function CreateComObject(const Controller: IUnknown): TComObject; override;
  end;

  TClient = class(TAutoIntfObject, IClient)
  private
    function Get_IsRunning: WordBool; safecall;
    procedure Start(Minimized: WordBool; Nosplash: WordBool); safecall;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TUser = class(TAutoIntfObject, IUser)
  private
    m_wstrHandle: WideString;
    m_wstrFullName: WideString;
    m_wstrDisplayName: WideString;
    function Get_Handle: WideString; safecall;
    function Get_FullName: WideString; safecall;
    function Get_DisplayName: WideString; safecall;
  public
    constructor Create(const wstrHandle, wstrFullName, wstrDisplayName: WideString);
    destructor Destroy; override;
  end;

  EApplication = class(Exception);
  TApplication = class(TAutoIntfObject, IApplication)
  private
    m_Skype: TSkype;
    m_wstrName: WideString;
    m_bHasContext: boolean;

    procedure FCheckContext;
    procedure FWaitForAllAttached;

    procedure Delete; safecall;
    procedure IApplication.Create = FCreate;
    procedure FCreate; safecall;
    procedure Connect(const Username: WideString; WaitConnected: WordBool); safecall;
    procedure SendDatagram(const Text: WideString; const pStreams: IApplicationStreamCollection); safecall;
    function Get_Streams: IApplicationStreamCollection; safecall;
    function Get_ConnectableUsers: IUserCollection; safecall;
    function Get_ConnectingUsers: IUserCollection; safecall;
    function Get_Name: WideString; safecall;

  public
    constructor Create(const wstrName: WideString; const ASkype: TSkype);
    destructor Destroy; override;
  end;

  TUserCollection = class(TAutoIntfObject, IUserCollection)
  private
    m_Users: TInterfaceList;
    function Get_Count: Integer; safecall;
    function Get_Item(Param2: Integer): IUser; safecall; // Param2 = 1, 2, ...
  public
    constructor Create(const AUsers: TInterfaceList);
    destructor Destroy; override;
  end;

  TChatMessage = class(TAutoIntfObject, IChatMessage)
  private
    m_wstrMessageBody: WideString;
    m_Sender: IUser;
    function Get_Sender: IUser; safecall;
    function Get_Body: WideString; safecall;
  public
    constructor Create(const wstrMessageBody: WideString; Sender: IUser);
  end;

////////////////////////////////////////////////////////////////////////////////
// Globals

var
  g_TypeLib: ITypeLib = nil;

function GetDataTypeLib: ITypeLib;
begin
  if (not Assigned(g_TypeLib)) then
    LoadRegTypeLib(LIBID_SkypeTS, SkypeTSMajorVersion, SkypeTSMinorVersion,
      0, g_TypeLib);
  Result := g_TypeLib;
end;

////////////////////////////////////////////////////////////////////////////////
// TSkype

destructor TSkype.Destroy;
begin
  FDetachID;
  FDestroyControls;
  m_Applications.Free;

  inherited;

  MainForm.UpdateGUI;
end;


procedure TSkype.FDestroyControls;
begin
  m_AttachTimer.Free;
end;


procedure TSkype.FDetachID;
begin
  if (m_iID = Pred(Length(g_arrSkypes))) then
    SetLength(g_arrSkypes, m_iID)
  else
    g_arrSkypes[m_iID] := nil;
end;


procedure TSkype.EventSinkChanged(const EventSink: IUnknown);
begin
  FEvents := EventSink as ISkypeEvents;
end;


procedure TSkype.Initialize;
begin
  inherited Initialize;
  FConnectionPoints := TConnectionPoints.Create(Self);
  if AutoFactory.EventTypeInfo <> nil then
    FConnectionPoint := FConnectionPoints.CreateConnectionPoint(
      AutoFactory.EventIID, ckSingle, EventConnect)
  else FConnectionPoint := nil;

  m_Client := TClient.Create;
  FCreateControls;
  FAttachID;
  MainForm.UpdateGUI;
end;


procedure TSkype.DoAttach(const wstrHandle, wstrFullName, wstrDisplayName: WideString);
begin
  m_CurrentUser := TUser.Create(wstrHandle, wstrFullName, wstrDisplayName);
  m_bCanAttach := TRUE;
end;


procedure TSkype.FCreateControls;
begin
  m_AttachTimer := TTimer.Create(nil);
  m_AttachTimer.Enabled := FALSE;
  m_AttachTimer.Interval := 1000;
  m_AttachTimer.OnTimer := FOnAttachTimer;
end;


procedure TSkype.FOnAttachTimer(Sender: TObject);
begin
  if (not m_bCanAttach) then
    exit;

  m_AttachTimer.Enabled := FALSE;
  Include(m_States, sAttached);
  FEvents.AttachmentStatus(apiAttachSuccess);
  MainForm.UpdateGUI;
end;


procedure TSkype.FAttachID;
var
  i: integer;
begin
  for i := Low(g_arrSkypes) to High(g_arrSkypes) do
  begin
    if (not Assigned(g_arrSkypes[i])) then
    begin
      g_arrSkypes[i] := self;
      m_iID := i;
      exit;
    end;
  end; // for

  m_iID := Length(g_arrSkypes);
  SetLength(g_arrSkypes, Succ(m_iID));
  g_arrSkypes[m_iID] := self;
end;



procedure TSkype.Attach(Protocol: Integer; Wait: WordBool);
begin
  Assert(Protocol = 5);

  m_AttachTimer.Enabled := TRUE;
  if (Wait) then
  begin
    while (not (sAttached in m_States)) do
      Sleep(1);
  end;
end;


function TSkype.Get_Application(const Name: WideString): IApplication;
var
  i: integer;
begin
  if (not Assigned(m_Applications)) then
    m_Applications := TInterfaceList.Create;

  for i := 0 to m_Applications.Count - 1 do
  begin
    Result := (m_Applications[i] as IApplication);
    if (Result.Name = Name) then
      exit;
  end;

  Result := TApplication.Create(Name, self);
  m_Applications.Add(Result);
end;


function TSkype.Get_Client: IClient;
begin
  Result := m_Client;
end;


function TSkype.SendMessage(const Username: WideString; const Text: WideString): IChatMessage;
var
  i: integer;
begin
  Result := TChatMessage.Create(Text, m_CurrentUser);
  for i := Low(g_arrSkypes) to High(g_arrSkypes) do
    if (Assigned(g_arrSkypes[i]) and (g_arrSkypes[i].CurrentUserHandle = Username)) then
      g_arrSkypes[i].FDoReceiveInstantMessage(Result);
end;


procedure TSkype.FDeleteApplication(const wstrApplicationName: WideString);
var
  i: integer;
begin
  for i := m_Applications.Count - 1 downto 0 do
    if ((m_Applications[i] as IApplication).Name = wstrApplicationName) then
    begin
      m_Applications.Delete(i);
      exit;
    end;
end;


procedure TSkype.FWaitForAllAttached;

  function NOtherAttached: boolean;
  var
    i: integer;
    Skype: TSkype;
  begin
    Assert(Length(g_arrSkypes) > 0);

    Result := FALSE;
    for i := Low(g_arrSkypes) to High(g_arrSkypes) do
    begin
      Skype := g_arrSkypes[i];
      if (not (Assigned(Skype) and (sAttached in Skype.States))) then
        exit;
    end;
    Result := TRUE;
  end;

begin // TSkype.FWaitForAllAttached
  while (not NOtherAttached) do
  begin
    Sleep(1);
    Forms.Application.ProcessMessages;
  end;
end;


procedure TSkype.FDoReceiveInstantMessage(const ChatMessage: IChatMessage);
begin
  FEvents.MessageStatus(ChatMessage, cmsReceived);
  if (Assigned(OnInstantMessageReceived)) then
    OnInstantMessageReceived(self, ChatMessage.Body);
end;


function TSkype.Get_CurrentUser: IUser;
begin
  Result := m_CurrentUser;
end;


function TSkype.Get_CurrentUserHandle: WideString;
begin
  if (Assigned(m_CurrentUser)) then
    Result := m_CurrentUser.Handle
  else
    Result := '';
end;


procedure TSkype.SendInstantMessge(const wstrSkypeHandle, wstrMessage: WideString);
begin
  SendMessage(wstrSkypeHandle, wstrMessage);
end;

////////////////////////////////////////////////////////////////////////////////
// TAutoObjectFactory_

function TAutoObjectFactory_.CreateComObject(const Controller: IUnknown): TComObject;
var
  i: integer;
  iSkypesNum: integer;
begin
  iSkypesNum := 0;
  for i := Low(g_arrSkypes) to High(g_arrSkypes) do
    if (Assigned(g_arrSkypes[i])) then
      inc(iSkypesNum);

  if (iSkypesNum < MAX_SKYPES_NUM) then
    Result := inherited CreateComObject(Controller)
  else
    Result := nil;
end;

////////////////////////////////////////////////////////////////////////////////
// TClient

constructor TClient.Create;
begin
  inherited Create(GetDataTypeLib, IClientDisp);
end;


destructor TClient.Destroy;
begin
  inherited;
end;


function TClient.Get_IsRunning: WordBool;
begin
  Result := TRUE;
end;


procedure TClient.Start(Minimized: WordBool; Nosplash: WordBool); safecall;
begin
end;

////////////////////////////////////////////////////////////////////////////////
// TApplication

constructor TApplication.Create(const wstrName: WideString; const ASkype: TSkype);
begin
  m_Skype := ASkype;
  m_wstrName := wstrName;
  inherited Create(GetDataTypeLib, IApplicationDisp);
end;


destructor TApplication.Destroy;
begin
  inherited;
end;


procedure TApplication.Delete;
var
  i: integer;
begin
  m_Skype.FDeleteApplication(m_wstrName);
end;


procedure TApplication.FCreate;
begin
  Assert(not m_bHasContext);
  m_bHasContext := TRUE;
end;


procedure TApplication.Connect(const Username: WideString; WaitConnected: WordBool);
begin
  FCheckContext;
  Assert(FALSE);
  // TODO:
end;


procedure TApplication.FCheckContext;
begin
  if (not m_bHasContext) then
    raise EApplication.CreateFmt('Application %s doesn''t have context!', [m_wstrName]);
end;


procedure TApplication.FWaitForAllAttached;
begin
  m_Skype.FWaitForAllAttached;
end;


procedure TApplication.SendDatagram(const Text: WideString; const pStreams: IApplicationStreamCollection);
begin
  FCheckContext;
  Assert(FALSE);
  // TODO:
end;


function TApplication.Get_Streams: IApplicationStreamCollection;
begin
  FCheckContext;
  Assert(FALSE);
  // TODO:
end;


function TApplication.Get_ConnectableUsers: IUserCollection;
var
  i: integer;
  Users: TInterfaceList;
begin
  FCheckContext;
  FWaitForAllAttached;

  Users := TInterfaceList.Create;
  try
    for i := Low(g_arrSkypes) to High(g_arrSkypes) do
    begin
      with g_arrSkypes[i] do
      if ((m_Skype.m_iID <> m_iID) and Assigned(m_CurrentUser)) then
        Users.Add(m_CurrentUser);
    end;

    Result := TUserCollection.Create(Users);
  finally
    Users.Free;
  end;
end;


function TApplication.Get_ConnectingUsers: IUserCollection;
begin
  FCheckContext;
  FWaitForAllAttached;

  Result := TUserCollection.Create(nil);
end;


function TApplication.Get_Name: WideString;
begin
  Result := m_wstrName;
end;

////////////////////////////////////////////////////////////////////////////////
// TUser

constructor TUser.Create(const wstrHandle, wstrFullName, wstrDisplayName: WideString);
begin
  m_wstrHandle := wstrHandle;
  m_wstrFullName := wstrFullName;
  m_wstrDisplayName := wstrDisplayName;

  inherited Create(GetDataTypeLib, IUserDisp);
end;


destructor TUser.Destroy;
begin
  inherited;
end;


function TUser.Get_Handle: WideString;
begin
  Result := m_wstrHandle;
end;


function TUser.Get_FullName: WideString;
begin
  Result := m_wstrFullName;
end;


function TUser.Get_DisplayName: WideString;
begin
  Result :=  m_wstrDisplayName;
end;

////////////////////////////////////////////////////////////////////////////////
// TUserCollection

constructor TUserCollection.Create(const AUsers: TInterfaceList);
var
  i: integer;
begin
  inherited Create(GetDataTypeLib, IUserCollectionDisp);

  m_Users := TInterfaceList.Create;
  if (Assigned(AUsers)) then
  begin
    for i := 0 to AUsers.Count - 1 do
      m_Users.Add(AUsers[i]);
  end;
end;


destructor TUserCollection.Destroy;
begin
  m_Users.Free;
  inherited;
end;


function TUserCollection.Get_Count: Integer;
begin
  Result := m_Users.Count;
end;


function TUserCollection.Get_Item(Param2: Integer): IUser;
begin
  Result := m_Users[Pred(Param2)] as IUser;
end;

////////////////////////////////////////////////////////////////////////////////
// TChatMessage

constructor TChatMessage.Create(const wstrMessageBody: WideString; Sender: IUser);
begin
  inherited Create(GetDataTypeLib, IChatMessageDisp);
  m_wstrMessageBody := wstrMessageBody;
  m_Sender := Sender;
end;


function TChatMessage.Get_Sender: IUser;
begin
  Result := m_Sender;
end;


function TChatMessage.Get_Body: WideString;
begin
  Result := m_wstrMessageBody;
end;


initialization
  TAutoObjectFactory_.Create(ComServer, TSkype, CLASS_Skype,
    ciMultiInstance, tmSingle);

finalization
  ComServer.UpdateRegistry(FALSE);

end.
