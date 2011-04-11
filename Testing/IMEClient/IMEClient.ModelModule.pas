////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit IMEClient.ModelModule;

interface

uses
  SysUtils, Classes, IdBaseComponent, IdComponent, IdTCPConnection,
  IdTCPClient, IdException,
  //
  IMEClient.Headers,
  IMEClient.PluginSurrogate, ExtCtrls;

type
  TModelModule = class(TDataModule, IViewEvents)
    TCPClient: TIdTCPClient;
    UnloadPluginTimer: TTimer;
    procedure TCPClientConnected(Sender: TObject);
    procedure TCPClientDisconnected(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure UnloadPluginTimerTimer(Sender: TObject);
  private
    m_View: IView;

    m_strTCPClientData: string;

    m_strlContacts: TStringList;

    m_iHandleID: integer;
    m_strHandleName: string;
    m_strSendText: string;

    m_iContactHandleID: integer;

    m_PluginSurrogate: TPluginSurrogate;

    procedure FInitializeView;
    procedure FSynchronizeView;
    procedure FSendConnect;
    function FSendMessage(const strMsg: string; iHandleID: integer): boolean;

    function FParseCommands(strData: string): boolean;
    function FParseCommandEnters(strData: string): boolean;
    function FParseCommandMessage(strData: string): boolean;
    function FParseCommandLeaves(strData: string): boolean;
    function FParsePluginData(strData: string; iFromHandleID: integer): boolean;

    procedure FOnTCPClientDataReceived;
    procedure FDisConnect;

    procedure FSetConnectedToView(bConnected: boolean);

    function FGetHandleID: integer;
    procedure FSetHandleID(Value: integer);
    procedure FSetHandleIDToView(iHandleID: integer);

    function FGetHandleName: string;
    procedure FSetHandleName(const strValue: string);
    procedure FSetHandleNameToView(const strHandleName: string);

    function FGetSendText: string;
    procedure FSetSendText(const strValue: string);
    procedure FSetSendTextToView(const strValue: string);

    procedure FLoadSettings;
    procedure FSaveSettings;

    procedure FSetSendMessageToView(const strValue: string);

    procedure FOutputErrorToView(const strError: string);

    procedure FAddContact(iHandleID: integer; const strHandleName: string);
    procedure FAddContactToView(iHandleID: integer; const strHandleName: string);
    procedure FDeleteContact(iHandleID: integer; const strHandleName: string);
    procedure FDeleteContactFromView(iHandleID: integer; const strHandleName: string);
    procedure FClearPluginData;

    procedure FSetInMessageToView(iFromHandleID: integer; const strFromHandleName, strMsg: string);
    procedure FSetServerMessageToView(const strData: string);

    function FGetIniFileName: string;

    function FGetPluginSurrogate: TPluginSurrogate;

    function FContactAvailable(iHandleID: integer): boolean;
    function FIsConnected: boolean;

    procedure FAddPluginDataToView(iHandleID: integer; const strHandleName, strData: string; bReceived: boolean);

    property HandleID: integer read FGetHandleID write FSetHandleID;
    property HandleName: string read FGetHandleName write FSetHandleName;
    property SendText: string read FGetSendText write FSetSendText;

    property ContactHandleID: integer read m_iContactHandleID;
    property IniFileName: string read FGetIniFileName;

    property PluginSurrogate: TPluginSurrogate read FGetPluginSurrogate;

  protected
    procedure IViewEvents.OnClose = ROnClose;
    procedure ROnClose;
    procedure IViewEvents.OnSend = ROnSend;
    procedure ROnSend;
    procedure IViewEvents.OnConnect = ROnConnect;
    procedure ROnConnect;
    procedure IViewEvents.OnDisconnect = ROnDisconnect;
    procedure ROnDisconnect;
    procedure IViewEvents.OnChangeHandleID = ROnChangeHandleID;
    procedure ROnChangeHandleID(iNewHandleID: integer);
    procedure IViewEvents.OnChangeHandleName = ROnChangeHandleName;
    procedure ROnChangeHandleName(const strNewHandleName: string);
    procedure IViewEvents.OnChangeSendText = ROnChangeSendText;
    procedure ROnChangeSendText(const strNewSendText: string);
    procedure IViewEvents.OnChangeContactHandleID = ROnChangeContactHandleID;
    procedure ROnChangeContactHandleID(iNewContactHandleID: integer);
    procedure IViewEvents.OnStartPlugin = ROnStartPlugin;
    procedure ROnStartPlugin;
    procedure IViewEvents.OnClearPluginData = ROnClearPluginData;
    procedure ROnClearPluginData;

  public
    procedure SetView(Value: IView);
    function SendPluginData(const strData: string; iContactHandleID: integer): boolean;
    function GetHandleName: string;
    function GetContactHandleName(iContactHandleID: integer): string;
    procedure CanUnloadPlugin;
  end;

implementation

{$R *.dfm}

uses
  Forms, StrUtils, HTTPApp, IniFiles,
  //
  IMEClient.PluginSurrogate.MI;

type
  TCPClientListenerThread = class(TThread)
  private
    m_ModelModule: TModelModule;
    procedure FOnTCPClientDataReceived(const strData: string);
  protected
    procedure Execute; override;
  public
    constructor Create(AModelModule: TModelModule);
  end;

const
  INI_SECTION_SETTINGS = 'Settings';
  INI_HANDLE_ID_IDENT = 'HandleID';
  INI_HANDLE_NAME_IDENT = 'HandleName';

function GetNextToken(var strData: string; chDelim: char = ' '): string;
var
  iPos: integer;
begin
  strData := TrimLeft(strData);
  iPos := Pos(string(chDelim), strData);
  if (iPos > 0) then
  begin
    Result := LeftStr(strData, Pred(iPos));
    strData := Copy(strData, Succ(iPos), MaxInt);
  end
  else
  begin
    Result := strData;
    strData := '';
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TCPClientListenerThread

constructor TCPClientListenerThread.Create(AModelModule: TModelModule);
begin
  inherited Create(TRUE);
  m_ModelModule := AModelModule;
  FreeOnTerminate := TRUE;
  Resume;
end;


procedure TCPClientListenerThread.Execute;
var
  strData: string;
begin
  while (m_ModelModule.FIsConnected) do
  begin
    try
      strData := m_ModelModule.TCPClient.ReadLn;
    except
      on EIdNotConnected do
        ;
      on EIdConnClosedGracefully do
        ;
      on Exception do
        raise;
    end;
    if (strData <> '') then
    begin
      FOnTCPClientDataReceived(strData);
      strData := '';
    end;
    Sleep(1);
  end;
end;


procedure TCPClientListenerThread.FOnTCPClientDataReceived(const strData: string);
begin
  m_ModelModule.m_strTCPClientData := strData;
  Synchronize(m_ModelModule.FOnTCPClientDataReceived);
end;

////////////////////////////////////////////////////////////////////////////////
// TModelModule

procedure TModelModule.SetView(Value: IView);
begin
  if (Assigned(m_View)) then
    m_View.SetEvents(nil);

  m_View := Value;
  if (Assigned(m_View)) then
  begin
    m_View.SetEvents(self);
    FInitializeView;
  end;
end;


function TModelModule.SendPluginData(const strData: string; iContactHandleID: integer): boolean;
begin
  Result := FSendMessage(strData, iContactHandleID);
  if (Result) then
    FAddPluginDataToView(iContactHandleID, GetContactHandleName(iContactHandleID), strData, FALSE);
end;


procedure TModelModule.FAddPluginDataToView(iHandleID: integer; const strHandleName, strData: string; bReceived: boolean);
begin
  if (Assigned(m_View)) then
    m_View.AddPluginData(iHandleID, strHandleName, strData, bReceived);
end;


procedure TModelModule.FInitializeView;
begin
  FSynchronizeView;
end;


procedure TModelModule.FSynchronizeView;
begin
  FSetHandleIDToView(m_iHandleID);
  FSetHandleNameToView(m_strHandleName);
  FSetSendText(m_strSendText); 
end;


procedure TModelModule.FSetConnectedToView(bConnected: boolean);
begin
  if (Assigned(m_View)) then
    m_View.SetConnected(bConnected);
end;


procedure TModelModule.TCPClientConnected(Sender: TObject);
begin
  FSetConnectedToView(FIsConnected);
  TCPClientListenerThread.Create(self);
  FSendConnect;
end;


function TModelModule.FIsConnected: boolean;
begin
  Result := TCPClient.Connected;
end;


procedure TModelModule.TCPClientDisconnected(Sender: TObject);
begin
  FSetConnectedToView(FIsConnected);
end;


function TModelModule.FSendMessage(const strMsg: string; iHandleID: integer): boolean;
begin
  Result := (FIsConnected and FContactAvailable(iHandleID));
  if (Result) then
    TCPClient.WriteLn(Format('>message %d %s', [iHandleID, HTTPEncode(strMsg)]));
end;


procedure TModelModule.FSendConnect;
var
  strHTTPHN, strCmd: string;
begin
  strHTTPHN := HttpEncode(HandleName);
  strCmd := Format('>connect %s %d', [strHTTPHN, HandleID]);
  TCPClient.WriteLn(strCmd);
end;


procedure TModelModule.ROnClose;
begin


  if (FIsConnected) then
    TCPClient.Disconnect;
//  if (Assigned(m_TCPClientListener)) then
//    m_TCPClientListener.WaitFor;
end;


procedure TModelModule.ROnSend;
begin
  Assert(ContactHandleID > 0);

  if (SendText <> '') then
  begin
    if (FSendMessage(SendText, ContactHandleID)) then
      FSetSendMessageToView('')
    else
      FOutputErrorToView('Message couldn''t be sent!');
  end;
end;


procedure TModelModule.FDisConnect;
begin
  m_iContactHandleID := 0;
  if (FIsConnected) then
    TCPClient.Disconnect
  else
  begin
    try
      TCPClient.Connect;
    except
      on EIdSocketError do
        FOutputErrorToView('Cannot connect to server!');
    end;
  end;
end;


procedure TModelModule.FOutputErrorToView(const strError: string);
begin
  if (Assigned(m_View)) then
    m_View.OutputError(strError);
end;


procedure TModelModule.FAddContact(iHandleID: integer; const strHandleName: string);
begin
  m_strlContacts.AddObject(strHandleName, Pointer(iHandleID));
  FAddContactToView(iHandleID, strHandleName);
end;


procedure TModelModule.FAddContactToView(iHandleID: integer; const strHandleName: string);
begin
  if (Assigned(m_View)) then
    m_View.AddContact(iHandleID, strHandleName);
end;


procedure TModelModule.FDeleteContact(iHandleID: integer; const strHandleName: string);
var
  iIndex: integer;
begin
  iIndex := m_strlContacts.IndexOfObject(Pointer(iHandleID));
  if (iIndex >= 0) then
    m_strlContacts.Delete(iIndex);

  FDeleteContactFromView(iHandleID, strHandleName);
end;


procedure TModelModule.FDeleteContactFromView(iHandleID: integer; const strHandleName: string);
begin
  if (Assigned(m_View)) then
    m_View.DeleteContact(iHandleID, strHandleName);
end;


procedure TModelModule.FClearPluginData;
begin
  if (Assigned(m_View)) then
    m_View.ClearPluginData;
end;


function TModelModule.FParseCommandEnters(strData: string): boolean;
var
  strHandleName: string;
  iHandleID: integer;
begin
  strHandleName := HTTPDecode(GetNextToken(strData));
  iHandleID := StrToIntDef(GetNextToken(strData), 0);
  Result := ((strHandleName <> '') and (iHandleID > 0));
  if (Result) then
    FAddContact(iHandleID, strHandleName);
end;


function TModelModule.FParsePluginData(strData: string; iFromHandleID: integer): boolean;
begin
  if (Assigned(PluginSurrogate)) then
    Result := PluginSurrogate.SendData(strData, iFromHandleID)
  else
    Result := FALSE;

  if (Result) then
    FAddPluginDataToView(iFromHandleID, self.GetContactHandleName(iFromHandleID), strData, TRUE);
end;


function TModelModule.FParseCommandMessage(strData: string): boolean;
var
  strMsg: string;
  iFromHandleID: integer;
  iIndex: integer;
begin
  Result := FALSE;
  iFromHandleID := StrToIntDef(GetNextToken(strData), 0);
  strMsg := HTTPDecode(GetNextToken(strData));
  if (strMsg <> '') then
  begin
    Result := FParsePluginData(strMsg, iFromHandleID);
    if (Result) then
      exit;

    iIndex := m_strlContacts.IndexOfObject(Pointer(iFromHandleID));

    if (iIndex >= 0) then
      FSetInMessageToView(iFromHandleID, m_strlContacts[iIndex], strMsg)
    else
      FSetInMessageToView(0, '', strMsg);

    Result := TRUE;
  end;
end;


function TModelModule.FParseCommandLeaves(strData: string): boolean;
var
  strHandleName: string;
  iHandleID: integer;
begin
  strHandleName := GetNextToken(strData);
  iHandleID := StrToIntDef(GetNextToken(strData), 0);
  Result := ((strHandleName <> '') and (iHandleID > 0));
  if (Result) then
    FDeleteContact(iHandleID, strHandleName);
end;


function TModelModule.FParseCommands(strData: string): boolean;
var
  strCmd: string;
begin
  strCmd := GetNextToken(strData);
  if (strCmd = '>message') then
    Result := FParseCommandMessage(strData)
  else if (strCmd = '>enters') then
    Result := FParseCommandEnters(strData)
  else if (strCmd = '>leaves') then
    Result := FParseCommandLeaves(strData)
  else
    Result := FALSE;
end;


procedure TModelModule.FSetInMessageToView(iFromHandleID: integer; const strFromHandleName, strMsg: string);
begin
  if (Assigned(m_View)) then
    m_View.SetInMessage(iFromHandleID, strFromHandleName, strMsg);
end;


procedure TModelModule.FSetServerMessageToView(const strData: string);
begin
  if (Assigned(m_View)) then
    m_View.SetServerMessage(strData);
end;


procedure TModelModule.FOnTCPClientDataReceived;
var
  strData: string;
begin
  if (m_strTCPClientData <> '') then
  begin
    strData := m_strTCPClientData;
    m_strTCPClientData := '';
    if (not FParseCommands(strData)) then
      FSetServerMessageToView(strData);
  end;
end;


procedure TModelModule.ROnConnect;
begin
  FDisConnect;
end;


procedure TModelModule.ROnDisconnect;
begin
  FDisConnect;
end;


function TModelModule.FGetHandleID: integer;
begin
  Result := m_iHandleID;
end;


procedure TModelModule.FSetHandleID(Value: integer);
begin
  m_iHandleID := Value;
  FSetHandleIDToView(m_iHandleID);
end;


function TModelModule.FGetHandleName: string;
begin
  Result := m_strHandleName;
end;


function TModelModule.GetHandleName: string;
begin
  Result := FGetHandleName;
end;


function TModelModule.GetContactHandleName(iContactHandleID: integer): string;
var
  iIndex: integer;
begin
  iIndex := self.m_strlContacts.IndexOfObject(Pointer(iContactHandleID));
  if (iIndex >= 0) then
    Result := m_strlContacts[iIndex]
  else
    Result := '<Unknown name>';
end;


procedure TModelModule.FSetHandleName(const strValue: string);
begin
  if (strValue <> '') then
    m_strHandleName := strValue
  else
    m_strHandleName := IntToStr(HandleID);
  FSetHandleNameToView(m_strHandleName);
end;


procedure TModelModule.FLoadSettings;
begin
  with TIniFile.Create(IniFileName) do
  try
    HandleID := ReadInteger(INI_SECTION_SETTINGS, INI_HANDLE_ID_IDENT, 1234);
    HandleName := ReadString(INI_SECTION_SETTINGS, INI_HANDLE_NAME_IDENT, 'HandleName' + IntToStr(HandleID));
  finally
    Free;
  end;
end;


procedure TModelModule.FSaveSettings;
begin
  with TIniFile.Create(IniFileName) do
  try
    WriteInteger(INI_SECTION_SETTINGS, INI_HANDLE_ID_IDENT, HandleID);
    WriteString(INI_SECTION_SETTINGS, INI_HANDLE_NAME_IDENT, HandleName);
  finally
    Free;
  end;
end;

procedure TModelModule.DataModuleCreate(Sender: TObject);
begin
  m_strlContacts := TStringList.Create;
  FLoadSettings;
end;


procedure TModelModule.DataModuleDestroy(Sender: TObject);
begin
  m_PluginSurrogate.Free;
  
  FSaveSettings;
  m_strlContacts.Free;

  m_View.SetEvents(nil);
end;


procedure TModelModule.FSetSendMessageToView(const strValue: string);
begin
  if (Assigned(m_View)) then
    m_View.SetSendText(strValue);
end;


function TModelModule.FGetIniFileName: string;
begin
  Result := ChangeFileExt(Application.ExeName, '.ini');
end;


procedure TModelModule.FSetHandleIDToView(iHandleID: integer);
begin
  if (Assigned(m_View)) then
    m_View.SetHandleID(iHandleID);
end;


procedure TModelModule.FSetHandleNameToView(const strHandleName: string);
begin
  if (Assigned(m_View)) then
    m_View.SetHandleName(strHandleName);
end;


procedure TModelModule.ROnChangeHandleID(iNewHandleID: integer);
begin
  m_iHandleID := iNewHandleID;
end;


procedure TModelModule.ROnChangeHandleName(const strNewHandleName: string);
begin
  m_strHandleName := strNewHandleName;
end;


procedure TModelModule.ROnChangeSendText(const strNewSendText: string);
begin
  m_strSendText := strNewSendText;
end;


function TModelModule.FGetSendText: string;
begin
  Result := m_strSendText;
end;


procedure TModelModule.FSetSendText(const strValue: string);
begin
  m_strSendText := strValue;
  FSetSendTextToView(strValue);
end;


procedure TModelModule.FSetSendTextToView(const strValue: string);
begin
  if (Assigned(m_View)) then
    m_View.SetSendText(strValue);
end;


procedure TModelModule.ROnChangeContactHandleID(iNewContactHandleID: integer);
begin
  m_iContactHandleID := iNewContactHandleID;
end;


procedure TModelModule.ROnStartPlugin;
begin
  PluginSurrogate.StartPlugin(ContactHandleID);
end;


function TModelModule.FGetPluginSurrogate: TPluginSurrogate;
begin
  if (not Assigned(m_PluginSurrogate)) then
  begin
    UnloadPluginTimer.Enabled := FALSE;
    m_PluginSurrogate := TPluginSurrogateMI.Create(self);
  end;
  Result := m_PluginSurrogate;
end;


function TModelModule.FContactAvailable(iHandleID: integer): boolean;
var
  iIndex: integer;
begin
  iIndex := m_strlContacts.IndexOfObject(Pointer(iHandleID));
  Result := (iIndex >= 0);
end;


procedure TModelModule.CanUnloadPlugin;
begin
  UnloadPluginTimer.Enabled := TRUE;
end;


procedure TModelModule.UnloadPluginTimerTimer(Sender: TObject);
begin
  UnloadPluginTimer.Enabled := FALSE;
  FreeAndNil(m_PluginSurrogate);
end;


procedure TModelModule.ROnClearPluginData;
begin
  FClearPluginData;
end;

end.
