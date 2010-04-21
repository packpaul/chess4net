unit IMEServer.MainForm;

interface

uses
  SysUtils, Classes, Forms, ExtCtrls, Controls, ComCtrls, StdCtrls,
  IdThreadMgr, IdThreadMgrDefault, IdBaseComponent, IdComponent,
  IdTCPServer;

type
  PClientRecord = ^TClientRecord;
  TClientRecord = record
    iHandleID: integer;
    strHandleName: string;
    PT: TIdPeerThread;
  end;

  TLogEntryType = (letNo, letWrongCommand);

  TMainForm = class(TForm)
    PortLabel: TLabel;
    Label2: TLabel;
    DelayTrackBar: TTrackBar;
    DelayLabel: TLabel;
    DelayedStepTimer: TTimer;
    TCPServer: TIdTCPServer;
    IdThreadMgrDefault: TIdThreadMgrDefault;
    ClientsListBox: TListBox;
    LogMemo: TMemo;
    Label1: TLabel;

    procedure FormCreate(Sender: TObject);
    procedure DelayTrackBarChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure DelayedStepTimerTimer(Sender: TObject);
    procedure TCPServerConnect(AThread: TIdPeerThread);
    procedure TCPServerDisconnect(AThread: TIdPeerThread);
    procedure TCPServerExecute(AThread: TIdPeerThread);
    procedure TCPServerException(AThread: TIdPeerThread;
      AException: Exception);
  private
    m_arrClients: array of TClientRecord;
    m_arrDelayedMessages: array of record
      iToHandleID,
      iFromHandleID: integer;
      strMsg: string;
      lwSendTickCount: LongWord;
    end;

    client1StrList, client2StrList: TStringList;

    procedure FDelayedSend(const strMsg: string; pToClient, pFromClient: PClientRecord);
    procedure FAddDelayedMessage(const strAMsg: string; iAToHandleID, iAFromHandleID, lwASendTickCount: LongWord);

    function FGetDelay: Double;
    procedure FDisconnectClient(AClient: PClientRecord);
    procedure FDisconnectThread(APT: TIdPeerThread);
    function FHandleExists(iHandleID: integer): boolean;
    function FAddClient(iAHandleID: integer; const strAHandleName: string; APT: TIdPeerThread): boolean;
    procedure FRelayData(const strData: string; AFromClient, AToClient: PClientRecord);
    function FParseCommands(strData: string; AThread: TIdPeerThread): boolean;
    procedure FSendMessage(const strMsg: string; pToClient, pFromClient: PClientRecord);
    procedure FAddToLog(const strData: string; LogEntryType: TLogEntryType);
    function FIsDelayedSend: boolean;
    function FGetClientByHandleID(iHandleID: integer): PClientRecord;
    function FGetClientByThread(AThread: TIdPeerThread): PClientRecord;
  end;

implementation

{$R *.dfm}

uses
  Windows, HTTPApp, StrUtils;

const
  MAX_DELAY = 3.0;
  LOG_ENTRY_TYPE_STR: array[TLogEntryType] of string =(
    '',
    'Wrong command'
  );

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


function TMainForm.FGetDelay: Double;
begin
  Result := MAX_DELAY * DelayTrackBar.Position / DelayTrackBar.Max; 
end;


procedure TMainForm.FormCreate(Sender: TObject);
begin
  DelayTrackBarChange(Sender);
  client1StrList := TStringList.Create;
  client2StrList := TStringList.Create;
  PortLabel.Caption := 'Port#: ' + IntToStr(TCPServer.DefaultPort);
end;


procedure TMainForm.DelayTrackBarChange(Sender: TObject);
begin
  DelayLabel.Caption := FloatToStrF(FGetDelay, ffFixed, 1, 1) + ' sec.';
end;


procedure TMainForm.FormDestroy(Sender: TObject);
begin
  Client1StrList.Free;
  Client2StrList.Free;
end;


procedure TMainForm.FDelayedSend(const strMsg: string; pToClient, pFromClient: PClientRecord);
var
  iSendTickCount: integer;
begin
  if (not (Assigned(pToClient) and Assigned(pFromClient))) then
    exit;

  iSendTickCount := GetTickCount + Round(1000 * FGetDelay);
  FAddDelayedMessage(strMsg, pToClient.iHandleID, pFromClient.iHandleID, iSendTickCount);
  DelayedStepTimer.Enabled := TRUE;
end;


function TMainForm.FGetClientByHandleID(iHandleID: integer): PClientRecord;
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


function TMainForm.FGetClientByThread(AThread: TIdPeerThread): PClientRecord;
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


procedure TMainForm.FAddDelayedMessage(const strAMsg: string; iAToHandleID, iAFromHandleID, lwASendTickCount: LongWord);
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


procedure TMainForm.DelayedStepTimerTimer(Sender: TObject);
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

procedure TMainForm.FSendMessage(const strMsg: string; pToClient, pFromClient: PClientRecord);
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


procedure TMainForm.TCPServerConnect(AThread: TIdPeerThread);
begin
   //
end;


procedure TMainForm.FDisconnectThread(APT: TIdPeerThread);
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


procedure TMainForm.FDisconnectClient(AClient: PClientRecord);
var
  i: integer;
  iIndex: integer;
  strLeavesCmd: string;
begin
  if (not Assigned(AClient)) then
    exit;

  if (AClient.PT.Connection.Connected) then
    AClient.PT.Connection.Disconnect;

  iIndex := ClientsListBox.Items.IndexOfObject(Pointer(AClient.iHandleID));
  if (iIndex >= 0) then
  begin
    ClientsListBox.Items.Objects[iIndex] := nil;
    ClientsListBox.Items.Delete(iIndex);
  end;

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


procedure TMainForm.TCPServerDisconnect(AThread: TIdPeerThread);
begin
  FDisconnectClient(FGetClientByThread(AThread));
end;


procedure TMainForm.TCPServerExecute(AThread: TIdPeerThread);
var
  strData: string;
begin
  strData := AThread.Connection.ReadLn;
  if (not FParseCommands(strData, AThread)) then
    FAddToLog(strData, letWrongCommand)
end;


procedure TMainForm.FAddToLog(const strData: string; LogEntryType: TLogEntryType);
var
  strFormatedEntry: string;
begin
  if (LogEntryType = letNo) then
    strFormatedEntry := Format('%s: %s ', [TimeToStr(Now), strData])
  else
    strFormatedEntry := Format('%s: (%s) %s ', [TimeToStr(Now), LOG_ENTRY_TYPE_STR[LogEntryType], strData]);

  LogMemo.Lines.Add(strFormatedEntry);
  LogMemo.ScrollBy(0, MaxInt);
end;


function TMainForm.FParseCommands(strData: string; AThread: TIdPeerThread): boolean;

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


procedure TMainForm.TCPServerException(AThread: TIdPeerThread;
  AException: Exception);
begin
  FDisconnectThread(AThread);
end;


function TMainForm.FHandleExists(iHandleID: integer): boolean;
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


function TMainForm.FAddClient(iAHandleID: integer;
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

  // To display
  ClientsListBox.AddItem(Format('%s (%d)', [strAHandleName, iAHandleID]), Pointer(iAHandleID));

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


procedure TMainForm.FRelayData(const strData: string; AFromClient, AToClient: PClientRecord);
begin
  if (FIsDelayedSend) then
    FDelayedSend(strData, AToClient, AFromClient)
  else
    FSendMessage(strData, AToClient, AFromClient);
end;


function TMainForm.FIsDelayedSend: boolean;
begin
  Result := (FGetDelay > 0.0);
end;

end.
