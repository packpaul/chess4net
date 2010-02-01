unit ConnectorUnit;

interface

uses
  Classes, ExtCtrls
{$IFDEF TESTING}
  , SkypeTS_TLB
{$ELSE}
  , SKYPE4COMLib_TLB
{$ENDIF}
  ;

type
  TConnectorEvent = (ceConnected, ceDisconnected, ceData, ceError, ceSkypeError);

  TConnectorHandler = procedure(ce: TConnectorEvent; d1: pointer = nil;
                                d2: pointer = nil) of object;

  TConnector = class(TDataModule)
    sendTimer: TTimer;
    procedure sendTimerTimer(Sender: TObject);
  private
    _connected : boolean;

{$IFDEF DEBUG_LOG}
    _logFile: Text;

    procedure InitLog;
    procedure WriteToLog(const s: string);
    procedure CloseLog;
{$ENDIF}

  public
    property connected: boolean read _connected;
    procedure Close;
    procedure SendData(const d: string);
    procedure Free;
    class function Create(h: TConnectorHandler): TConnector; reintroduce;
  end;

procedure MessageGot(const msg: string; const accName: WideString; protoDllHandle: integer);
procedure MessageSent(const msg: string);
procedure ErrorSendingMessage(const msg: string);

implementation

{$R *.dfm}

{$J+} {$I-}

uses
  SysUtils, StrUtils, Windows,
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


procedure SendMessage(const vMessage: string);
begin
//  QIPSendMessage(vMessage, gwAccName, giProtoDllHandle);
  // TODO:
end;


// деформатирование входящих сообщений. TRUE - если декодирование удалось
function DeformatMsg(var msg: string; var n: integer): boolean;
var
  l: integer;
begin
  result := FALSE;
  if LeftStr(msg, length(PROMPT_HEAD)) = PROMPT_HEAD then
    begin
{$IFDEF DEBUG_LOG}
      connector.WriteToLog('>> ' + msg);
{$ENDIF}
      msg := RightStr(msg, length(msg) - length(PROMPT_HEAD));
      l := pos(PROMPT_TAIL, msg);
      if l = 0 then exit;
      try
        n := StrToInt(LeftStr(msg, l-1));
      except
        on Exception do exit;
      end;
      msg := AnsiReplaceStr(RightStr(msg, length(msg) - l), '&amp;', '&');
      result := TRUE;
    end;
end;


function FilterMsg(msg: string): boolean;
var
  cntrMsg: integer;
begin
  if not connector.connected then
    begin
      if msg = MSG_INVITATION then
        begin
          SendMessage(MSG_INVITATION);
          connector._connected := TRUE;
          g_handler(ceConnected);
          Result := TRUE;
        end
      else
        Result := FALSE;
    end
  else // connector.connected
    begin
      if msg_sending <> '' then
        begin
          msg_sending := '';
          unformated_msg_sending := '';
          inc(cntrMsgOut);
        end;
      if DeformatMsg(msg, cntrMsg) then
        begin
          Result := TRUE;
          if cntrMsg > cntrMsgIn then
            begin
              inc(cntrMsgIn);
              if cntrMsg > cntrMsgIn then
                begin
                  g_handler(ceError); // пакет исчез
                  exit;
                end
            end
          else
            exit; // пропуск пакетов с более низкими номерами
          if msg = MSG_CLOSE then
            begin
              g_handler(ceDisconnected);
              connector._connected := FALSE;
            end
          else
            g_handler(ceData, @msg);
        end
      else
        Result := FALSE;
    end;
end;

function NotifySender(const vMessage: string): boolean;
begin
  if not Assigned(connector) then
    begin
      Result := FALSE;
      exit;
    end;
  if (not connector.connected) and (vMessage = MSG_INVITATION) then
    begin
      Result := TRUE;
      exit;
    end;

  Result := (msg_sending = vMessage);
  if not Result then
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
      SendMessage(msg_sending);
      sendTimer.Enabled := FALSE;
      _connected := FALSE;
      g_handler(ceDisconnected);
    end;
{$IFDEF DEBUG_LOG}
  CloseLog;
{$ENDIF}
end;


procedure TConnector.SendData(const d: string);
begin
  if d = MSG_CLOSE then
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

  g_handler := h;

  g_Skype := TSkype.Create(nil);
{$IFDEF TESTING}
  m_Skype.Connect;
{$ENDIF}
  // TODO: handle here cases when Skype4COM is not registered

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
  if msg_sending = '' then
    begin
      sendTimer.Enabled := FALSE;
      if msg_buf <> '' then
        begin
          unformated_msg_sending := msg_buf;
          msg_sending := FormatMsg(msg_buf);
          msg_buf := '';
          SendMessage(msg_sending);
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
          SendMessage(msg_sending);
        end;
    end;
end;


procedure MessageGot(const msg: string; const accName: WideString; protoDllHandle: integer);
begin
  // TODO:
{
  if not Assigned(connector) then
    exit;
  if (protoDllHandle <> giProtoDllHandle) or (gwAccName <> accName) then
    exit;

  FilterMsg(msg);
  // TODO: подавление сообщения при обмене через основной канал
}
end;


procedure MessageSent(const msg: string);
begin
  NotifySender(msg);
  // TODO: если сообщение ушло по основному каналу - подавить сообщение
end;


procedure ErrorSendingMessage(const msg: string);
begin
  if (not connector.connected) and (msg = MSG_INVITATION) then
    g_handler(ceSkypeError);
end;

end.
