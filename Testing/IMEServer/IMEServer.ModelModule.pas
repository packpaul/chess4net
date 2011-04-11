////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit IMEServer.ModelModule;

interface

uses
  SysUtils, Classes,
  //
  IMEServer.Headers, IdThreadMgr, IdThreadMgrDefault, IdBaseComponent,
  IdComponent, IdTCPServer, ExtCtrls;

type
  PClientRecord = ^TClientRecord;
  TClientRecord = record
    iHandleID: integer;
    strHandleName: string;
    PT: TIdPeerThread;
  end;

  TModelModule = class(TDataModule, IViewEvents)
    DelayedStepTimer: TTimer;
    TCPServer: TIdTCPServer;
    IdThreadMgrDefault: TIdThreadMgrDefault;
    procedure DataModuleDestroy(Sender: TObject);
    procedure DelayedStepTimerTimer(Sender: TObject);
    procedure TCPServerExecute(AThread: TIdPeerThread);
    procedure TCPServerDisconnect(AThread: TIdPeerThread);
    procedure TCPServerException(AThread: TIdPeerThread;
      AException: Exception);
  private
    m_View: IView;

    m_arrClients: array of TClientRecord;
    m_arrDelayedMessages: array of record
      iToHandleID,
      iFromHandleID: integer;
      strMsg: string;
      lwSendTickCount: LongWord;
    end;

    m_dbDelay: Double;

    procedure FInitializeView;

    procedure FDelayedSend(const strMsg: string; pToClient, pFromClient: PClientRecord);
    procedure FRelayData(const strData: string; AFromClient, AToClient: PClientRecord);
    function FParseCommands(strData: string; AThread: TIdPeerThread): boolean;
    procedure FAddDelayedMessage(const strAMsg: string; iAToHandleID, iAFromHandleID, lwASendTickCount: LongWord);
    procedure FDisconnectClient(AClient: PClientRecord);
    procedure FDisconnectThread(APT: TIdPeerThread);
    function FHandleExists(iHandleID: integer): boolean;
    function FAddClient(iAHandleID: integer; const strAHandleName: string; APT: TIdPeerThread): boolean;
    procedure FSendMessage(const strMsg: string; pToClient, pFromClient: PClientRecord);
    function FIsDelayedSend: boolean;
    function FGetClientByHandleID(iHandleID: integer): PClientRecord;
    function FGetClientByThread(AThread: TIdPeerThread): PClientRecord;
    procedure FAddToLog(const strData: string; LogEntryType: TLogEntryType);

    procedure FSetPortToView(iValue: integer);
    procedure FAddClientToView(AClient: PClientRecord);
    procedure FRemoveClientFromView(AClient: PClientRecord);
    procedure FSetDelayToView(dbDelay: Double; dbMaxDelay: Double = 0.0);

  public
    procedure SetView(View: IView);

  protected
    procedure IViewEvents.OnDelayChanged = ROnDelayChanged;
    procedure ROnDelayChanged(dbNewValue: Double);
  end;

implementation

{$R *.dfm}

uses
  Windows, HTTPApp, StrUtils;

const
  MAX_DELAY = 3.0;

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
// TModelModule

procedure TModelModule.SetView(View: IView);
begin
  if (Assigned(m_View)) then
    m_View.SetEvents(nil);
  m_View := View;
  if (Assigned(View)) then
    View.SetEvents(self);

  FInitializeView;
end;


procedure TModelModule.DataModuleDestroy(Sender: TObject);
begin
  SetView(nil);
end;


procedure TModelModule.FInitializeView;
begin
  FSetPortToView(TCPServer.DefaultPort);
  FSetDelayToView(0.0, MAX_DELAY);
end;


procedure TModelModule.FSetPortToView(iValue: integer);
begin
  if (Assigned(m_View)) then
    m_View.SetPort(iValue);
end;


procedure TModelModule.FDelayedSend(const strMsg: string; pToClient, pFromClient: PClientRecord);
var
  iSendTickCount: integer;
begin
  if (not (Assigned(pToClient) and Assigned(pFromClient))) then
    exit;

  iSendTickCount := GetTickCount + Round(1000 * m_dbDelay);
  FAddDelayedMessage(strMsg, pToClient.iHandleID, pFromClient.iHandleID, iSendTickCount);
  DelayedStepTimer.Enabled := TRUE;
end;


procedure TModelModule.DelayedStepTimerTimer(Sender: TObject);
var
  i: integer;
  bStopTimer: boolean;
begin
  bStopTimer := TRUE;
  for i := Low(m_arrDelayedMessages) to High(m_arrDelayedMessages) do
  with m_arrDelayedMessages[i] do
  begin
    if (lwSendTickCount > 0) then
    begin
      bStopTimer := FALSE;
      if (lwSendTickCount <= GetTickCount) then
      begin
        FSendMessage(strMsg, FGetClientByHandleID(iToHandleID), FGetClientByHandleID(iFromHandleID));
        lwSendTickCount := 0;
      end;
    end;
  end;

  DelayedStepTimer.Enabled := (not bStopTimer);
end;


procedure TModelModule.FRelayData(const strData: string; AFromClient, AToClient: PClientRecord);
begin
  if (FIsDelayedSend) then
    FDelayedSend(strData, AToClient, AFromClient)
  else
    FSendMessage(strData, AToClient, AFromClient);
end;


function TModelModule.FParseCommands(strData: string; AThread: TIdPeerThread): boolean;

  function NParseCommandConnect(strData: string): boolean;
  var
    strHandleName: string;
    iHandleID: integer;
  begin
    Result := TRUE;

    strHandleName := HTTPDecode(GetNextToken(strData));
    iHandleID := StrToIntDef(GetNextToken(strData), 0);

    if ((strHandleName = '') or (iHandleID = 0)) then
    begin
      AThread.Connection.WriteLn('-Error on connection to server!');
      FDisconnectThread(AThread);
      exit;
    end;

    if (FHandleExists(iHandleID)) then
    begin
      AThread.Connection.WriteLn(Format('-Handle %d is already connected to server.', [iHandleID]));
      FDisconnectThread(AThread);
      exit;
    end;

    if (FAddClient(iHandleID, strHandleName, AThread)) then
    begin
      AThread.Connection.WriteLn('-Connection to server established.');
    end
    else
    begin
      AThread.Connection.WriteLn('-Connection rejected by server.');
      FDisconnectThread(AThread);
    end;
  end;


  function NParseCommandMessage(strData: string): boolean;
  var
    iHandleID: integer;
    strMsg: string;
  begin
    Result := FALSE;
    iHandleID := StrToIntDef(GetNextToken(strData), 0);
    strMsg := HTTPDecode(GetNextToken(strData));
    if (strMsg <> '') then
    begin
      FRelayData(strMsg, FGetClientByThread(AThread), FGetClientByHandleID(iHandleID));
      Result := TRUE;
    end;
  end;

var
  strCmd: string;
begin // TMainForm.FParseCommands
  strCmd := GetNextToken(strData);
  if (strCmd = '>message') then
    Result := NParseCommandMessage(strData)
  else if (strCmd = '>connect') then
    Result := NParseCommandConnect(strData)
  else
    Result := FALSE;
end;

procedure TModelModule.TCPServerExecute(AThread: TIdPeerThread);
var
  strData: string;
begin
  strData := AThread.Connection.ReadLn;
  if (not FParseCommands(strData, AThread)) then
    FAddToLog(strData, letWrongCommand)
end;


function TModelModule.FGetClientByHandleID(iHandleID: integer): PClientRecord;
var
  i: integer;
begin
  Result := nil;
  for i := Low(m_arrClients) to High(m_arrClients) do
  begin
    if (m_arrClients[i].iHandleID = iHandleID) then
      Result := @m_arrClients[i];
  end;
end;


function TModelModule.FGetClientByThread(AThread: TIdPeerThread): PClientRecord;
var
  i: integer;
begin
  Result := nil;
  for i := Low(m_arrClients) to High(m_arrClients) do
  begin
    if (m_arrClients[i].PT = AThread) then
      Result := @m_arrClients[i];
  end;
end;


procedure TModelModule.FAddDelayedMessage(const strAMsg: string; iAToHandleID, iAFromHandleID, lwASendTickCount: LongWord);
var
  i: integer;
  iFreePos: integer;
begin
  Assert(lwASendTickCount <> 0);

  iFreePos := -1;

  for i := Low(m_arrDelayedMessages) to High(m_arrDelayedMessages) do
  begin
    if (m_arrDelayedMessages[i].lwSendTickCount = 0) then
    begin
      iFreePos := i;
      break;
    end;
  end;

  if (iFreePos < 0) then
  begin
    iFreePos := Length(m_arrDelayedMessages);
    SetLength(m_arrDelayedMessages, Succ(iFreePos));
  end;

  with m_arrDelayedMessages[iFreePos] do
  begin
    iToHandleID := iAToHandleID;
    iFromHandleID := iAFromHandleID;
    strMsg := strAMsg;
    lwSendTickCount := lwASendTickCount;
  end;

end;


procedure  TModelModule.FSendMessage(const strMsg: string; pToClient, pFromClient: PClientRecord);
var
  iFromClientHandleID: integer;
begin
  if (Assigned(pToClient) and Assigned(pToClient.PT)) then
  begin
    if (Assigned(pFromClient)) then
      iFromClientHandleID := pFromClient.iHandleID
    else
      iFromClientHandleID := 0;
    pToClient.PT.Connection.WriteLn(Format('>message %d %s', [iFromClientHandleID, HTTPEncode(strMsg)]));
  end;
end;


procedure TModelModule.FDisconnectThread(APT: TIdPeerThread);
var
  Client: PClientRecord;
begin
  if (not Assigned(APT)) then
    exit;
  Client := FGetClientByThread(APT);
  if (Assigned(Client)) then
    FDisconnectClient(Client)
  else
  begin
    if (APT.Connection.Connected) then
      APT.Connection.Disconnect;
  end;
end;


procedure TModelModule.FDisconnectClient(AClient: PClientRecord);
var
  i: integer;
  strLeavesCmd: string;
begin
  if (not Assigned(AClient)) then
    exit;

  if (AClient.PT.Connection.Connected) then
    AClient.PT.Connection.Disconnect;

  FRemoveClientFromView(AClient);

  strLeavesCmd := Format('>leaves %s %d', [HttpEncode(AClient.strHandleName), AClient.iHandleID]);
  for i := Low(m_arrClients) to High(m_arrClients) do
  begin
    if (m_arrClients[i].iHandleID = AClient.iHandleID) then
    begin
      m_arrClients[i].iHandleID := 0;
      m_arrClients[i].PT := nil;
      break;
    end;
  end;

  // Notify other clients
  if (strLeavesCmd <> '') then
  begin
    for i := Low(m_arrClients) to High(m_arrClients) do
      if (m_arrClients[i].iHandleID > 0) then
        m_arrClients[i].PT.Connection.WriteLn(strLeavesCmd);
  end;
end;


procedure TModelModule.TCPServerDisconnect(AThread: TIdPeerThread);
begin
  FDisconnectClient(FGetClientByThread(AThread));
end;

procedure TModelModule.TCPServerException(AThread: TIdPeerThread;
  AException: Exception);
begin
  FDisconnectThread(AThread);
end;


function TModelModule.FHandleExists(iHandleID: integer): boolean;
var
  i: integer;
begin
  Result := FALSE;
  for i := Low(m_arrClients) to High(m_arrClients) do
  begin
    Result := (m_arrClients[i].iHandleID = iHandleID);
    if (Result) then
      break;
  end;
end;


function TModelModule.FAddClient(iAHandleID: integer;
  const strAHandleName: string; APT: TIdPeerThread): boolean;
var
  i: integer;
  iFreePos: integer;
  strEntersCmd: string;
begin
//  Result := FALSE;

  // Add data to array
  iFreePos := -1;
  for i := Low(m_arrClients) to High(m_arrClients) do
    if (m_arrClients[i].iHandleID = 0) then
    begin
      iFreePos := i;
      break;
    end;

  if (iFreePos < 0) then
  begin
    iFreePos := Length(m_arrClients);
    SetLength(m_arrClients, Succ(iFreePos));
  end;

  with m_arrClients[iFreePos] do
  begin
    iHandleID := iAHandleID;
    strHandleName := strAHandleName;
    PT := APT;
  end;

  FAddClientToView(@m_arrClients[iFreePos]);

  // Notify this contact
  for i := Low(m_arrClients) to High(m_arrClients) do
  begin
    if ((m_arrClients[i].iHandleID > 0) and (m_arrClients[i].PT <> APT)) then
    begin
      strEntersCmd := Format('>enters %s %d',
        [HttpEncode(m_arrClients[i].strHandleName), m_arrClients[i].iHandleID]);
      APT.Connection.WriteLn(strEntersCmd);
    end;
  end;

  // Notify other contacts
  strEntersCmd := Format('>enters %s %d', [HttpEncode(strAHandleName), iAHandleID]);
  for i := Low(m_arrClients) to High(m_arrClients) do
  begin
    if ((m_arrClients[i].iHandleID > 0) and (m_arrClients[i].PT <> APT)) then
      m_arrClients[i].PT.Connection.WriteLn(strEntersCmd);
  end;

  Result := TRUE;
end;


function TModelModule.FIsDelayedSend: boolean;
begin
  Result := (m_dbDelay > 0.0);
end;


procedure TModelModule.ROnDelayChanged(dbNewValue: Double);
begin
  if (m_dbDelay = dbNewValue) then
    exit;
  m_dbDelay := dbNewValue;
end;


procedure TModelModule.FRemoveClientFromView(AClient: PClientRecord);
begin
  if (Assigned(m_View)) then
    m_View.RemoveClient(AClient.strHandleName, AClient.iHandleID);
end;


procedure TModelModule.FSetDelayToView(dbDelay: Double; dbMaxDelay: Double = 0.0);
begin
  if (Assigned(m_View)) then
    m_View.SetDelay(dbDelay, dbMaxDelay);
end;


procedure TModelModule.FAddClientToView(AClient: PClientRecord);
begin
  if (Assigned(m_View)) then
    m_View.AddClient(AClient.strHandleName, AClient.iHandleID);
end;


procedure TModelModule.FAddToLog(const strData: string; LogEntryType: TLogEntryType);
begin
  if (Assigned(m_View)) then
    m_View.AddToLog(strData, LogEntryType); 
end;

end.
