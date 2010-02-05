unit ConnectorUnit;

interface

uses
  Classes, TntClasses, ExtCtrls
{$IFDEF TESTING}
  , SkypeTS_TLB
{$ELSE}
  , SKYPE4COMLib_TLB
{$ENDIF}
  ;

type
  TConnectorEvent = (ceConnected, ceDisconnected, ceData, ceError, ceSkypeError, ceShowConnectableUsers);

  TConnectorHandler = procedure(ce: TConnectorEvent; d1: pointer = nil;
                                d2: pointer = nil) of object;

  TSkypeState = (sAttaching, sAttached, sUserConnecting, sUserConnected);
  TSkypeStates = set of TSkypeState;

  TConnector = class(TDataModule)
    sendTimer: TTimer;
    showContactsTimer: TTimer;
    userConnectingTimer: TTimer;
    procedure sendTimerTimer(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
    procedure showContactsTimerTimer(Sender: TObject);
    procedure userConnectingTimerTimer(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    _connected : boolean;

    // Skype 
    m_SkypeStates: TSkypeStates;
    m_ConnectableUserCollection: IUserCollection;
    m_wstrContactHandle: WideString;
    m_wstrlInBuffer: TTntStringList;

{$IFDEF DEBUG_LOG}
    _logFile: Text;

    procedure InitLog;
    procedure WriteToLog(const s: string);
    procedure CloseLog;
{$ENDIF}

    procedure FSendInvitation;
    function FFilterMsg(wstrMsg: WideString): boolean;    

    // Skype
    function FGetSkype: TSkype;
    function FGetSkypeApplication: IApplication;

    procedure FOnMessageStatus(ASender: TObject; const pMessage: IChatMessage; Status: TChatMessageStatus);
    procedure FOnSkypeAttachmentStatus(ASender: TObject; Status: TAttachmentStatus);
    procedure FOnSkypeApplicationDatagram(ASender: TObject; const pApp: IApplication;
      const pStream: IApplicationStream; const Text: WideString);
    procedure FShowSkypeConnectableUsers;
    procedure FConnectIfNotConnected;
    function FIsUserConnected: boolean;
    procedure FSendCommand(const wstrCommand: WideString);
    function FGetUserHandle: WideString;

    property Skype: TSkype read FGetSkype;
    property SkypeApplication: IApplication read FGetSkypeApplication;

  public
    property connected: boolean read _connected;
    procedure Close;
    procedure SendData(const d: string);
    procedure Free;
    class function Create(h: TConnectorHandler): TConnector; reintroduce;
    procedure ConnectToContact(iContactIndex: integer);
    property UserHandle: WideString read FGetUserHandle;
    property ContactHandle: WideString read m_wstrContactHandle;
  end;

implementation

{$R *.dfm}

{$J+} {$I-}

uses
  Forms, SysUtils, StrUtils, Windows,
  GlobalsLocalUnit;

var
  g_Skype: TSkype = nil;
  g_handler: TConnectorHandler;
  connector: TConnector = nil; // singleton
  msg_sending, unformated_msg_sending: string; // отсылаемое сообщение

  // cntrMsgIn и cntrMsgOut были введены для преодоления бага с зависающими сообщениями
  cntrMsgIn: integer;  // счётчик входящих сообщений
  cntrMsgOut: integer; // счётчик исходящих сообщений
  msg_buf: string; // буфер сообщений

const
  CONNECTOR_INSTANCES: integer = 0;
  // <сообщение> ::= PROMPT_HEAD <номер сообщения> PROMPT_TAIL <сообщение>
  PROMPT_HEAD = 'Ch4N:';
  PROMPT_TAIL = '>';
  MSG_CLOSE = 'ext';
  MAX_RESEND_TRYS = 9; // максимальное количество попыток пересыла


// деформатирование входящих сообщений. TRUE - если декодирование удалось
function DeformatMsg(var wstrMsg: WideString; var n: integer): boolean;
var
  l: integer;
begin
  result := FALSE;
  if LeftStr(wstrMsg, length(PROMPT_HEAD)) = PROMPT_HEAD then
    begin
{$IFDEF DEBUG_LOG}
      connector.WriteToLog('>> ' + wstrMsg);
{$ENDIF}
      wstrMsg := RightStr(wstrMsg, length(wstrMsg) - length(PROMPT_HEAD));
      l := pos(PROMPT_TAIL, wstrMsg);
      if l = 0 then exit;
      try
        n := StrToInt(LeftStr(wstrMsg, l - 1));
      except
        on Exception do exit;
      end;
      wstrMsg := AnsiReplaceStr(RightStr(wstrMsg, length(wstrMsg) - l), '&amp;', '&'); // TODO: WideString
      result := TRUE;
    end;
end;


function NotifySender(const wstrMsg: WideString): boolean;
begin
  if (not Assigned(connector)) then
  begin
    Result := FALSE;
    exit;
  end;
  if (not connector.connected) and (wstrMsg = MSG_INVITATION) then
  begin
    Result := TRUE;
    exit;
  end;

  Result := (msg_sending = wstrMsg);
  if (not Result) then
    exit;

{$IFDEF DEBUG_LOG}
   connector.WriteToLog('<< ' + msg_sending);
{$ENDIF}
   msg_sending := '';
   unformated_msg_sending := '';
   inc(cntrMsgOut);
end;

// форматирование исходящих сообщений
function FormatMsg(const msg: string): string;
begin
  Result := PROMPT_HEAD + IntToStr(cntrMsgOut) + PROMPT_TAIL + msg;
end;


procedure TConnector.Close;
begin
  if _connected then
  begin
    msg_sending := FormatMsg(MSG_CLOSE);
    connector.FSendCommand(msg_sending);
    sendTimer.Enabled := FALSE;
    _connected := FALSE;
    g_handler(ceDisconnected);
  end;
{$IFDEF DEBUG_LOG}
  CloseLog;
{$ENDIF}
end;


function TConnector.FFilterMsg(wstrMsg: WideString): boolean;
var
  cntrMsg: integer;
  strMsg: string;
begin
  if (not connected) then
  begin
    if (not Assigned(m_wstrlInBuffer)) then
      m_wstrlInBuffer := TTntStringList.Create;
    m_wstrlInBuffer.Add(wstrMsg);
    Result := FALSE;
    exit;
  end;
  begin
    if (msg_sending <> '') then
    begin
      msg_sending := '';
      unformated_msg_sending := '';
      inc(cntrMsgOut);
    end;
    if (DeformatMsg(wstrMsg, cntrMsg)) then
    begin
      Result := TRUE;
      if (cntrMsg > cntrMsgIn) then
      begin
        inc(cntrMsgIn);
        if (cntrMsg > cntrMsgIn) then
        begin
          g_handler(ceError); // пакет исчез
          exit;
        end;
      end
      else
        exit; // пропуск пакетов с более низкими номерами
      if (wstrMsg = MSG_CLOSE) then
      begin
        g_handler(ceDisconnected);
        _connected := FALSE;
      end
      else
      begin
        strMsg := wstrMsg;
        g_handler(ceData, @strMsg);
      end
    end
    else
      Result := FALSE;
  end;
end;


procedure TConnector.SendData(const d: string);
begin
  if (d = MSG_CLOSE) then
    g_handler(ceError)
  else
  begin
    msg_buf := msg_buf + d;
    sendTimer.Enabled := TRUE; // Отослать сообщение с некоторой оттяжкой -> всё одним пакетом
  end; { if d = MSG_CLOSE }
end;


class function TConnector.Create(h: TConnectorHandler): TConnector;
begin
  if CONNECTOR_INSTANCES > 0 then
    raise Exception.Create('No more than 1 instance of TConnector is possible!');

  Result := nil;
  g_handler := h;

  try
    g_Skype := TSkype.Create(nil);
{$IFDEF TESTING}
    g_Skype.Connect;
{$ENDIF}
  except
    g_handler(ceSkypeError);
    exit;
  end;

  if (not g_Skype.Client.IsRunning) then
    g_Skype.Client.Start(TRUE, TRUE);
  // TODO: handle here cases when Skype cannot start

  if (not Assigned(connector)) then
  begin
    connector := inherited Create(nil);
    inc(CONNECTOR_INSTANCES);
  end;

  cntrMsgIn := 0;
  cntrMsgOut := 1;
  msg_sending := '';
  unformated_msg_sending := '';
  msg_buf := '';

  Result := connector;

{$IFDEF DEBUG_LOG}
  connector.InitLog;
{$ENDIF}
end;


procedure TConnector.Free;
begin
  Close;
//  DisableBroadcast;
  dec(CONNECTOR_INSTANCES);
  // TODO: убрать connector из хэша
  if CONNECTOR_INSTANCES = 0 then
    begin
      inherited;
      connector := nil;
    end;
end;

{$IFDEF DEBUG_LOG}
procedure TConnector.InitLog;
begin
  AssignFile(_logFile, Chess4NetPath + 'Chess4Net_CONNECTORLOG.txt');
  Append(_logFile);
  if IOResult <> 0 then
    begin
      Rewrite(_logFile);
      if IOResult <> 0 then
        begin
          AssignFile(_logFile, Chess4NetPath + 'Chess4Net_CONNECTORLOG~.txt');
          Append(_logFile);
          if IOResult <> 0 then Rewrite(_logFile);
        end;
    end;

   WriteToLog('[' + DateTimeToStr(Now) + ']');
end;


procedure TConnector.WriteToLog(const s: string);
begin
  writeln(_logFile, s);
  Flush(_logFile);
end;


procedure TConnector.CloseLog;
begin
  CloseFile(_logFile);
end;
{$ENDIF}

procedure TConnector.sendTimerTimer(Sender: TObject);
const
  RESEND_COUNT : integer = 0;
begin
  if (msg_sending = '') then
  begin
    sendTimer.Enabled := FALSE;
    if (msg_buf <> '') then
    begin
      unformated_msg_sending := msg_buf;
      msg_sending := FormatMsg(msg_buf);
      msg_buf := '';
      connector.FSendCommand(msg_sending);
    end;
  end
  else
  begin
{$IFDEF DEBUG_LOG}
    WriteToLog('resend: ' + msg_sending);
{$ENDIF}
    inc(RESEND_COUNT);
    if RESEND_COUNT = MAX_RESEND_TRYS then
    begin
      RESEND_COUNT := 0;
      connector.FSendCommand(msg_sending);
    end;
  end;
end;


procedure MessageSent(const wstrMsg: WideString);
begin
  NotifySender(wstrMsg);
  // TODO: если сообщение ушло по основному каналу - подавить сообщение
end;


procedure TConnector.DataModuleCreate(Sender: TObject);
begin
  if (Assigned(g_Skype)) then
  begin
    Skype.OnAttachmentStatus := FOnSkypeAttachmentStatus;
    Skype.OnMessageStatus := FOnMessageStatus;
    Skype.OnApplicationDatagram := FOnSkypeApplicationDatagram;
  end;

  m_SkypeStates := [sAttaching];
  Skype.Attach(5, FALSE);

  showContactsTimer.Enabled := TRUE;
end;


function TConnector.FGetSkype: TSkype;
begin
  Result := g_Skype;
end;


function TConnector.FGetSkypeApplication: IApplication;
begin
  Result := g_Skype.Application[SKYPE_APP_NAME];
end;


procedure TConnector.FOnMessageStatus(ASender: TObject; const pMessage: IChatMessage; Status: TChatMessageStatus);
var
  wstrMessage: WideString;
begin
  case Status of
    cmsReceived:
    begin
      if (pMessage.Sender.Handle = m_wstrContactHandle) then
      begin
        wstrMessage := pMessage.Body;
//        Log('Received message: ' + wstrMessage);
        if (wstrMessage = MSG_INVITATION) then
        begin
//          Log('invitation received');
          FConnectIfNotConnected;
        end;
      end;
    end;
  end; // case
end;


procedure TConnector.FOnSkypeAttachmentStatus(ASender: TObject; Status: TAttachmentStatus);
begin
// Log(Skype.Convert.AttachmentStatusToText(Status));
  case Status of
    apiAttachAvailable:
    begin
      Skype.Attach(5, FALSE);
//      Log('attaching Skype');
    end;

    apiAttachSuccess:
    begin
      m_SkypeStates := [sAttached];
//      Log('Skype attached');      
    end;
  end; // case
end;


procedure TConnector.FOnSkypeApplicationDatagram(ASender: TObject; const pApp: IApplication;
  const pStream: IApplicationStream; const Text: WideString);
begin
  if (pApp.Name = SKYPE_APP_NAME) then
  begin
    FConnectIfNotConnected;
    FFilterMsg(Text);
//    Log(WideFormat('FSkypeApplicationDatagram(): Command - %s', [Text]));
  end;
end;


procedure TConnector.showContactsTimerTimer(Sender: TObject);
begin
  if (sAttached in m_SkypeStates) then
  begin
    ShowContactsTimer.Enabled := FALSE;
    SkypeApplication.Create;
    FShowSkypeConnectableUsers;
  end;
end;


procedure TConnector.FShowSkypeConnectableUsers;
var
  i: integer;
  wstrlConnectableUsers: TTntStringList;
  wstrUserName: WideString;
begin
  wstrlConnectableUsers := TTntStringList.Create;
  try
    m_ConnectableUserCollection := SkypeApplication.ConnectableUsers;

    for i := 1 to m_ConnectableUserCollection.Count do
    begin
      wstrUserName := m_ConnectableUserCollection.Item[i].DisplayName;
      if (Trim(wstrUserName) = '') then
        wstrUserName := m_ConnectableUserCollection.Item[i].FullName;
      if (Trim(wstrUserName) = '') then
        wstrUserName := m_ConnectableUserCollection.Item[i].Handle;

      wstrlConnectableUsers.AddObject(wstrUserName, TObject(i));
    end;

    g_handler(ceShowConnectableUsers, wstrlConnectableUsers);

  finally
    wstrlConnectableUsers.Free;
  end;
end;


procedure TConnector.ConnectToContact(iContactIndex: integer);
begin
  m_wstrContactHandle := m_ConnectableUserCollection.Item[iContactIndex].Handle;
  m_ConnectableUserCollection := nil;
  FSendInvitation;
end;


procedure TConnector.FSendInvitation;
begin
  if (m_wstrContactHandle <> '') then
    Skype.SendMessage(m_wstrContactHandle, MSG_INVITATION);
  // Log('invitation sent.');
end;


procedure TConnector.FConnectIfNotConnected;
begin
  if (not ((sUserConnecting in m_SkypeStates) or FIsUserConnected)) then
  begin
    Include(m_SkypeStates, sUserConnecting);
    SkypeApplication.Connect(m_wstrContactHandle, FALSE);
    userConnectingTimer.Enabled := TRUE;
//    Log('Connecting user started.');
  end;  
end;


function TConnector.FIsUserConnected: boolean;

  function NUserInConnecting(const wstrUserHandle: WideString): boolean;
  var
    i: integer;
    UserCollection: IUserCollection;
  begin
    Result := FALSE;
    UserCollection := SkypeApplication.ConnectingUsers;
    for i := 1 to UserCollection.Count do
      if (UserCollection.Item[i].Handle = wstrUserHandle) then
      begin
        Result := TRUE;
        exit;
      end;
  end;

begin // TConnector.FIsUserConnected
  Result := (sUserConnected in m_SkypeStates) or
    ((sUserConnecting in m_SkypeStates) and (not NUserInConnecting(self.m_wstrContactHandle)));
end;


procedure TConnector.userConnectingTimerTimer(Sender: TObject);
var
  i: integer;
begin
  if (FIsUserConnected) then
  begin
    userConnectingTimer.Enabled := FALSE;
    Exclude(m_SkypeStates, sUserConnecting);
    Include(m_SkypeStates, sUserConnected);

    FSendInvitation;

    _connected := TRUE;
    g_handler(ceConnected);

    if (Assigned(m_wstrlInBuffer)) then
    begin
      for i := 0 to m_wstrlInBuffer.Count - 1 do
        FFilterMsg(m_wstrlInBuffer[i]);
      FreeAndNil(m_wstrlInBuffer);
    end;
  end;
end;

procedure TConnector.DataModuleDestroy(Sender: TObject);
begin
  m_wstrlInBuffer.Free;
  SkypeApplication.Delete;
{$IFDEF TESTING}
  Skype.Disconnect;
{$ENDIF}
  FreeAndNil(g_Skype);
end;


procedure TConnector.FSendCommand(const wstrCommand: WideString);
begin
  SkypeApplication.SendDatagram(wstrCommand, SkypeApplication.Streams);
//  Log('Command ' + wstrCommand);
  NotifySender(wstrCommand); // TODO: move to handler
end;


function TConnector.FGetUserHandle: WideString;
begin
  Result := Skype.CurrentUserHandle;
end;

initialization

finalization
  if (Assigned(g_Skype)) then
  begin
{$IFDEF TESTING}
    g_Skype.Disconnect;
{$ENDIF}    
    FreeAndNil(g_Skype);
  end;

end.
