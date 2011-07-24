////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit SkypeTS.Skype;

interface

uses
  ComObj, ActiveX, AxCtrls, Classes, TntClasses, StdVcl, ExtCtrls,
  SkypeTS_TLB;

type
  IObject = interface
    ['{1902F9A6-FAC6-48FE-84C5-320261DA323E}']
    function GetObject: TObject;
    property _Object: TObject read GetObject;
  end;

  TSkypeState = (ssAttached);
  TSkypeStates = set of TSkypeState;

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

    m_States: TSkypeStates;
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
    procedure FDoReceiveApplicationDatagram(const ASenderSkype: TSkype;
      const ASenderApplication: IApplication; const wstrText: WideString);
    procedure FDoApplicationReceiving(const ASenderSkype: TSkype;
      const ASenderApplication: IApplication; const wstrText: WideString);
    procedure FDoApplicationStreams(const pApp: IApplication;
      const pStreams: IApplicationStreamCollection);

    function FGetApplicationByName(const wstrName: WideString): IApplication;
    function FGetApplicationStreamByPartnerHandle(const AApplication: IApplication;
      const wstrPartnerHadndle: WideString): IApplicationStream;

    class procedure FUpdateApplicationsConnectedSkypes;

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

    class function GetSkypeByID(iID: integer): TSkype;

    property ID: integer read m_iID;
    property States: TSkypeStates read m_States;
    property CurrentUserHandle: WideString read Get_CurrentUserHandle;

    property OnInstantMessageReceived: TOnInstantMessageReceived read FOnInstantMessageReceived
                                                                 write FOnInstantMessageReceived;
  end;

implementation

uses
  SysUtils, Forms, ComServ,
  SkypeTS.MainForm;

var
  g_arrSkypes: array of TSkype;

const
  MAX_SKYPES_NUM = 4;
  APPLICATION_CONNECTING_USER_TIMEOUT = 700;

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
  TApplication = class(TAutoIntfObject, IApplication, IObject)
  private
    m_Skype: TSkype;
    m_wstrName: WideString;
    m_bHasContext: boolean;

    m_ConnectingUsers: TInterfaceList;
    m_ConnectableUser: IUser;

    m_lstConnectedSkypeIDs: TList;
    m_Streams: IApplicationStreamCollection;

    m_ConnectingUserTimer: TTimer;

    procedure FOnConnectingUserTimer(Sender: TObject);
    function FGetConnectingUserTimer: TTimer;

    procedure FCheckContext;
    procedure FWaitForAllAttached;
    procedure FAddUserSkypeToConnected(const wstrHandle: WideString);

    procedure Delete; safecall;
    procedure IApplication.Create = FCreate;
    procedure FCreate; safecall;
    procedure Connect(const Username: WideString; WaitConnected: WordBool); safecall;
    procedure SendDatagram(const Text: WideString; const pStreams: IApplicationStreamCollection); safecall;
    function Get_Streams: IApplicationStreamCollection; safecall;
    function Get_ConnectableUsers: IUserCollection; safecall;
    function Get_ConnectingUsers: IUserCollection; safecall;
    function Get_Name: WideString; safecall;

    function GetObject: TObject;

    procedure FUpdateConnectedSkypes;
    procedure FAddStream(const ContactSkype: TSkype);

    property ConnectingUserTimer: TTimer read FGetConnectingUserTimer;
    property Skype: TSkype read m_Skype;

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

  TApplicationStream = class(TAutoIntfObject, IApplicationStream, IObject)
  private
    m_iPartnerSkypeID: integer;
    m_Application: TApplication;
    m_wstrlTextQueue: TTntStringList;
    function GetObject: TObject;
    function Get_PartnerHandle: WideString; safecall;
    procedure SendDatagram(const Text: WideString); safecall;
    procedure Write(const Text: WideString); safecall;
    function Read: WideString; safecall;
    procedure FAddText(const wstrText: WideString);
  public
    constructor Create(iPartnerSkypeID: integer; const AApplication: TApplication);
    destructor Destroy; override;
    property PartnerSkypeID: integer read m_iPartnerSkypeID;
  end;

  TApplicationStreamCollection = class(TAutoIntfObject, IApplicationStreamCollection)
  private
    m_ApplicationStreams: TInterfaceList;
    procedure Add(const pItem: IApplicationStream); safecall;
    function Get_Count: Integer; safecall;
    procedure Remove(Index: Integer); safecall;
    procedure RemoveAll; safecall;
    function Get_Item(Index: Integer): IApplicationStream; safecall; // Param2 = 1, 2, ...
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TApplicationStreamCollection_ = class(TAutoObject, IApplicationStreamCollection)
  private
    m_ApplicationStreamCollection: IApplicationStreamCollection;
    property ApplicationStreamCollection: IApplicationStreamCollection
      read m_ApplicationStreamCollection implements IApplicationStreamCollection;
  public
    procedure Initialize; override;
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

  FUpdateApplicationsConnectedSkypes;
end;


class procedure TSkype.FUpdateApplicationsConnectedSkypes;
var
  i, j: integer;
  ASkype: TSkype;
  AApplication: IApplication;
begin
  for i := Low(g_arrSkypes) to High(g_arrSkypes) do
  begin
    ASkype := g_arrSkypes[i];
    if (not Assigned(ASkype)) then
      continue;

    for j := 0 to ASkype.m_Applications.Count - 1 do
    begin
      AApplication := ASkype.m_Applications[j] as IApplication;
      ((AApplication as IObject)._Object as TApplication).FUpdateConnectedSkypes;
    end; // for j

  end; // for i
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
  Include(m_States, ssAttached);
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
  Assert(Protocol >= 5);

  m_AttachTimer.Enabled := TRUE;
  if (Wait) then
  begin
    while (not (ssAttached in m_States)) do
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
      if (not (Assigned(Skype) and (ssAttached in Skype.States))) then
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


procedure TSkype.FDoReceiveApplicationDatagram(const ASenderSkype: TSkype;
  const ASenderApplication: IApplication; const wstrText: WideString);
var
  AReceiverApplication: IApplication;
  AReceiverApplicationStream: IApplicationStream;
begin
  AReceiverApplication := FGetApplicationByName(ASenderApplication.Name);
  if (not Assigned(AReceiverApplication)) then
    exit;

  AReceiverApplicationStream := FGetApplicationStreamByPartnerHandle(
    AReceiverApplication, ASenderSkype.CurrentUserHandle);
  if (not Assigned(AReceiverApplicationStream)) then
    exit;

  FEvents.ApplicationDatagram(AReceiverApplication, AReceiverApplicationStream,
    wstrText);
end;


function TSkype.FGetApplicationByName(const wstrName: WideString): IApplication;
var
  i: integer;
begin
  Result := nil;

  for i := 0 to m_Applications.Count - 1 do
  begin
    Result := m_Applications[i] as IApplication;
    if (Result.Name = wstrName) then
      break;
    Result := nil;
  end;
end;


function TSkype.FGetApplicationStreamByPartnerHandle(const AApplication: IApplication;
  const wstrPartnerHadndle: WideString): IApplicationStream;
var
  i: integer;
begin
  Result := nil;

  for i := 1 to AApplication.Streams.Count do
  begin
    Result := AApplication.Streams[i];
    if (Result.PartnerHandle = wstrPartnerHadndle) then
      break;

    Result := nil;
  end;
end;


procedure TSkype.FDoApplicationReceiving(const ASenderSkype: TSkype;
  const ASenderApplication: IApplication; const wstrText: WideString);
var
  AReceiverApplication: IApplication;
  AReceiverApplicationStream: IApplicationStream;
  AReceiverApplicationStreamCollection: IApplicationStreamCollection;
begin
  AReceiverApplication := FGetApplicationByName(ASenderApplication.Name);
  if (not Assigned(AReceiverApplication)) then
    exit;

  AReceiverApplicationStream := FGetApplicationStreamByPartnerHandle(
    AReceiverApplication, ASenderSkype.CurrentUserHandle);
  if (not Assigned(AReceiverApplicationStream)) then
    exit;

  ((AReceiverApplicationStream as IObject)._Object as TApplicationStream).FAddText(wstrText);

  AReceiverApplicationStreamCollection := TApplicationStreamCollection.Create;
  AReceiverApplicationStreamCollection.Add(AReceiverApplicationStream);

  FEvents.ApplicationReceiving(AReceiverApplication, AReceiverApplicationStreamCollection);
end;


procedure TSkype.FDoApplicationStreams(const pApp: IApplication;
  const pStreams: IApplicationStreamCollection);
begin
  FEvents.ApplicationStreams(pApp, pStreams);
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


class function TSkype.GetSkypeByID(iID: integer): TSkype;
begin
  if ((iID >= 0) and (iID < Length(g_arrSkypes))) then
    Result := g_arrSkypes[iID]
  else
    Result := nil;
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
  inherited Create(GetDataTypeLib, IApplicationDisp);

  m_Skype := ASkype;
  m_wstrName := wstrName;
  m_ConnectingUsers := TInterfaceList.Create;
  m_lstConnectedSkypeIDs := TList.Create;
  m_Streams := TApplicationStreamCollection.Create;
end;


destructor TApplication.Destroy;
begin
  m_Streams := nil;

  m_ConnectingUserTimer.Free;
  m_lstConnectedSkypeIDs.Free;
  m_ConnectingUsers.Free;

  inherited;
end;


procedure TApplication.Delete;
begin
  m_Skype.FDeleteApplication(m_wstrName);
end;


procedure TApplication.FCreate;
begin
  Assert(not m_bHasContext);
  m_bHasContext := TRUE;
end;


procedure TApplication.FAddUserSkypeToConnected(const wstrHandle: WideString);
var
  i: integer;
  AUserSkype: TSkype;
  AUserSkypeApplication: IApplication;
begin
  for i := Low(g_arrSkypes) to High(g_arrSkypes) do
  begin
    AUserSkype := g_arrSkypes[i];
    if (Assigned(AUserSkype) and (AUserSkype.CurrentUserHandle = wstrHandle)) then
    begin
      m_lstConnectedSkypeIDs.Add(Pointer(AUserSkype.ID));

      AUserSkypeApplication := AUserSkype.Get_Application(m_wstrName);
      ((AUserSkypeApplication as IObject)._Object as TApplication).FAddStream(m_Skype);

      exit;
    end;
  end;

  Assert(FALSE);
end;


procedure TApplication.Connect(const Username: WideString; WaitConnected: WordBool);

  function NGetConnectableUser(const wstrHandle: WideString): IUser;
  var
    UserCollection: IUserCollection;
    i: integer;
  begin
    UserCollection := Get_ConnectableUsers;
    for i := 1 to UserCollection.Count do
    begin
      Result := UserCollection.Item[i];
      if (Result.Handle = wstrHandle) then
        exit;
    end;
    Result := nil;
  end;

  function NUserAlreadyConnected(const wstrHandle: WideString): boolean;
  var
    i: integer;
    ConnectedSkype: TSkype;
  begin
    Result := FALSE;
    for i := 0 to m_lstConnectedSkypeIDs.Count - 1 do
    begin
      ConnectedSkype := TSkype.GetSkypeByID(Integer(m_lstConnectedSkypeIDs[i]));
      Result := (Assigned(ConnectedSkype) and (ConnectedSkype.CurrentUserHandle = wstrHandle));
      if (Result) then
        exit;
    end; // for
  end;

var
  i: integer;
begin // TApplication.Connect
  FCheckContext;

  m_ConnectableUser := NGetConnectableUser(UserName);
  if (not Assigned(m_ConnectableUser)) then
    EApplication.CreateFmt('User handle %s is not connectable!', [UserName]);

  if (WaitConnected) then
  begin
    try
      m_ConnectingUsers.Add(m_ConnectableUser);
      i := APPLICATION_CONNECTING_USER_TIMEOUT;
      while (i > 0) do
      begin
        Sleep(1);
        Forms.Application.ProcessMessages;
        dec(i);
      end;

      FAddUserSkypeToConnected(UserName);

      i := m_ConnectingUsers.IndexOf(m_ConnectableUser);
      m_ConnectingUsers.Delete(i);
    finally
      m_ConnectableUser := nil;
    end;
  end
  else // (not WaitConnected)
  begin
    while (ConnectingUserTimer.Enabled) do // Wait for other users to be connected
    begin
      Sleep(1);
      Forms.Application.ProcessMessages;
    end;
    if (not NUserAlreadyConnected(UserName)) then
    begin
      m_ConnectingUsers.Add(m_ConnectableUser);
      ConnectingUserTimer.Enabled := TRUE;
    end;
  end; // if
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


procedure TApplication.SendDatagram(const Text: WideString;
  const pStreams: IApplicationStreamCollection);
var
  i: integer;
  ASenderStream: TApplicationStream;
begin
  FCheckContext;

  for i := 1 to pStreams.Count do
  begin
    ASenderStream := (pStreams.Item[i] as IObject)._Object as TApplicationStream;
    ASenderStream.SendDatagram(Text);
  end;
  
end;


function TApplication.Get_Streams: IApplicationStreamCollection;
begin
  FCheckContext;
  Result := m_Streams;
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
  Result := TUserCollection.Create(m_ConnectingUsers);
end;


function TApplication.Get_Name: WideString;
begin
  Result := m_wstrName;
end;


function TApplication.FGetConnectingUserTimer: TTimer;
begin
  if (not Assigned(m_ConnectingUserTimer)) then
  begin
    m_ConnectingUserTimer := TTimer.Create(nil);
    m_ConnectingUserTimer.Enabled := FALSE;
    m_ConnectingUserTimer.Interval := APPLICATION_CONNECTING_USER_TIMEOUT;
    m_ConnectingUserTimer.OnTimer := FOnConnectingUserTimer;
//    m_ConnectingUserTimer.Name := m_Skype.CurrentUserHandle + '_ConnectingUserTimer'; // test
  end;
  Result := m_ConnectingUserTimer;
end;


procedure TApplication.FOnConnectingUserTimer(Sender: TObject);
var
  i: integer;
begin
  ConnectingUserTimer.Enabled := FALSE;

  Assert(Assigned(m_ConnectableUser));
  try
    FAddUserSkypeToConnected(m_ConnectableUser.Handle);
    i := m_ConnectingUsers.IndexOf(m_ConnectableUser);
    if (i >= 0) then
      m_ConnectingUsers.Delete(i);
  finally
    m_ConnectableUser := nil;
  end;
end;


function TApplication.GetObject: TObject;
begin
  Result := self;
end;


procedure TApplication.FUpdateConnectedSkypes;
var
  iDeletedSkypeID: integer;

  procedure NUpdateStreams;
  var
    i: integer;
    AStream: TApplicationStream;
  begin
    i := m_Streams.Count;
    while (i >= 1) do
    begin
      AStream := ((m_Streams[i] as IObject)._Object as TApplicationStream);
      if (AStream.PartnerSkypeID = iDeletedSkypeID) then
        m_Streams.Remove(i);
      dec(i);
    end;
  end;

var
  i: integer;
begin // .FUpdateConnectedSkypes
  i := m_lstConnectedSkypeIDs.Count - 1;

  while (i >= 0) do
  begin
    iDeletedSkypeID := Integer(m_lstConnectedSkypeIDs[i]);
    if (not Assigned(TSkype.GetSkypeByID(iDeletedSkypeID))) then
    begin
      m_lstConnectedSkypeIDs.Delete(i);
      NUpdateStreams;
    end;
    dec(i);
  end;
  
end;


procedure TApplication.FAddStream(const ContactSkype: TSkype);
begin
  m_Streams.Add(TApplicationStream.Create(ContactSkype.ID, self));
  m_Skype.FDoApplicationStreams(self, m_Streams);
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

////////////////////////////////////////////////////////////////////////////////
// TApplicationStream

constructor TApplicationStream.Create(iPartnerSkypeID: integer;
   const AApplication: TApplication);
begin
  inherited Create(GetDataTypeLib, IApplicationStreamDisp);

  m_iPartnerSkypeID := iPartnerSkypeID;
  m_Application := AApplication;

  m_wstrlTextQueue := TTntStringList.Create;
end;


destructor TApplicationStream.Destroy;
begin
  m_wstrlTextQueue.Free;
  inherited;
end;


function TApplicationStream.Get_PartnerHandle: WideString; safecall;
var
  ASkype: TSkype;
begin
  ASkype := TSkype.GetSkypeByID(m_iPartnerSkypeID);
  if (Assigned(ASkype)) then
    Result := ASkype.CurrentUserHandle;
end;


function TApplicationStream.GetObject: TObject;
begin
  Result := self
end;


procedure TApplicationStream.SendDatagram(const Text: WideString);
var
  AReceiverSkype: TSkype;
begin
  AReceiverSkype := TSkype.GetSkypeByID(m_iPartnerSkypeID);
  AReceiverSkype.FDoReceiveApplicationDatagram(m_Application.Skype, m_Application, Text);
end;


procedure TApplicationStream.Write(const Text: WideString);
var
  AReceiverSkype: TSkype;
begin
  AReceiverSkype := TSkype.GetSkypeByID(m_iPartnerSkypeID);
  AReceiverSkype.FDoApplicationReceiving(m_Application.Skype, m_Application, Text);
end;


function TApplicationStream.Read: WideString;
var
  iIndex: integer;
begin
  if (m_wstrlTextQueue.Count > 0) then
  begin
    Result := m_wstrlTextQueue[0];
    m_wstrlTextQueue.Delete(0)
  end
  else
    Result := '';  
end;


procedure TApplicationStream.FAddText(const wstrText: WideString);
begin
  m_wstrlTextQueue.Append(wstrText); 
end;

////////////////////////////////////////////////////////////////////////////////
// TApplicationStreamCollection

constructor TApplicationStreamCollection.Create;
begin
  inherited Create(GetDataTypeLib, IApplicationStreamCollectionDisp);
  m_ApplicationStreams := TInterfaceList.Create;
end;


destructor TApplicationStreamCollection.Destroy;
begin
  RemoveAll;
  m_ApplicationStreams.Free;
  
  inherited;
end;


procedure TApplicationStreamCollection.Add(const pItem: IApplicationStream);
begin
  m_ApplicationStreams.Add(pItem);
end;


function TApplicationStreamCollection.Get_Count: Integer;
begin
  Result := m_ApplicationStreams.Count;
end;


procedure TApplicationStreamCollection.Remove(Index: Integer); safecall;
begin
  m_ApplicationStreams.Delete(Index - 1);
end;


procedure TApplicationStreamCollection.RemoveAll; safecall;
begin
  m_ApplicationStreams.Clear;
end;


function TApplicationStreamCollection.Get_Item(Index: Integer): IApplicationStream; safecall;
begin
  Result := m_ApplicationStreams[Index - 1] as IApplicationStream;
end;

////////////////////////////////////////////////////////////////////////////////
// TApplicationStreamCollection_

procedure TApplicationStreamCollection_.Initialize;
begin
  inherited Initialize;
  m_ApplicationStreamCollection := TApplicationStreamCollection.Create;
end;

initialization
  TAutoObjectFactory_.Create(ComServer, TSkype, CLASS_Skype,
    ciMultiInstance, tmSingle);

  TAutoObjectFactory.Create(ComServer,
    TApplicationStreamCollection_, CLASS_ApplicationStreamCollection,
    ciMultiInstance, tmSingle);

finalization
  ComServer.UpdateRegistry(FALSE);

end.
