unit ConnectorUnit;

interface

uses
  Classes,
  m_globaldefs, m_api, ExtCtrls;

type
  TConnectorEvent = (ceConnected, ceDisconnected, ceData, ceError);

  TConnectorHandler = procedure(ce: TConnectorEvent; d1: pointer = nil;
                                d2: pointer = nil) of object;

  TConnector = class(TDataModule)
    sendTimer: TTimer;
    procedure sendTimerTimer(Sender: TObject);
  private
    _connected : boolean;
    _handler: TConnectorHandler; // ***
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
    class function Create(hContact: THandle; h: TConnectorHandler): TConnector; reintroduce;
  end;

implementation

{$R *.dfm}

{$J+} {$I-}

uses
  SysUtils, StrUtils,  
  GlobalsLocalUnit;

var
  connector: TConnector; // singleton
  hCurrContact: THandle; // ***
  msg_sending, unformated_msg_sending: string; // отсылаемое сообщение
  hFilterMsg: THandle;
  hNotifySender: THandle;
  hMsg: THandle;
  // cntrMsgIn и cntrMsgOut были введены для преодоления бага с зависающими сообщениями
  cntrMsgIn: integer;  // счётчик входящих сообщений
  cntrMsgOut: integer; // счётчик исходящих сообщений
  msg_buf: string; // буфер сообщений

const
  CONNECTOR_INSTANCES: integer = 0;
  M_CHESS4NET = 'Chess4Net';
  // <сообщение> ::= PROMPT_HEAD <номер сообщения> PROMPT_TAIL <сообщение>
  PROMPT_HEAD = 'Ch4N:';
  PROMPT_TAIL = '>';
  MSG_CLOSE = 'ext';
  MAX_MSG_TRYS = 3; // максимальное количество попыток отсыла сообщения
  MAX_RESEND_TRYS = 9; // максимальное количество попыток пересыла

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
      msg := RightStr(msg, length(msg) - l);
      result := TRUE;
    end;
end;

// форматирование исходящих сообщений
function FormatMsg(const msg: string): string;
begin
  Result := PROMPT_HEAD + IntToStr(cntrMsgOut) + PROMPT_TAIL + msg;
end;

function FilterMsg(wParam: WPARAM; lParam_: LPARAM): int; cdecl;
var
  msg: string;
  hContact: THandle;
  cntrMsg: integer;
begin
  msg := string(PPROTORECVEVENT(PCCSDATA(lParam_).lParam).szMessage);
  hContact := PCCSDATA(lParam_).hContact;
  // TODO: Handle msg;
  if not connector.connected then
    begin
      if msg = MSG_INVITATION then
        begin
          connector._connected := TRUE;
          hMsg := CallContactService(hContact, PSS_MESSAGE, 0, LPARAM(PChar(MSG_INVITATION)));
          connector._handler(ceConnected);
          Result := 0;
        end
      else
        Result := CallService(MS_PROTO_CHAINRECV, wParam, lParam_);
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
          Result := 0;
          if cntrMsg > cntrMsgIn then
            begin
              inc(cntrMsgIn);
              if cntrMsg > cntrMsgIn then
                begin
                  connector._handler(ceError); // пакет исчез
                  exit;
                end
            end
          else
            exit; // пропуск пакетов с более низкими номерами
          if msg = MSG_CLOSE then
            begin
              connector._handler(ceDisconnected);
              connector._connected := FALSE;
            end
          else
            connector._handler(ceData, @msg);
        end
      else
        Result := CallService(MS_PROTO_CHAINRECV, wParam, lParam_);
    end;
end;

function NotifySender(wParam: WPARAM; lParam_: LPARAM): int; cdecl;
const
  MSG_TRYS: integer = 1;
begin
  Result := 0;

  if (PACKDATA(lParam_).type_ <> ACKTYPE_MESSAGE) or (msg_sending = '') then exit;
  case PACKDATA(lParam_)^.result_ of
    ACKRESULT_SUCCESS:
      begin
{$IFDEF DEBUG_LOG}
        connector.WriteToLog('<< ' + msg_sending);
{$ENDIF}
        MSG_TRYS := 1;
        msg_sending := '';
        unformated_msg_sending := '';
        inc(cntrMsgOut);
      end;
    ACKRESULT_FAILED:
      begin
{$IFDEF DEBUG_LOG}
        connector.WriteToLog('XX ' + msg_sending);
{$ENDIF}
        inc(MSG_TRYS);
        if MSG_TRYS <= MAX_MSG_TRYS then
          begin
            msg_buf := unformated_msg_sending + msg_buf;
            connector.sendTimer.Enabled := TRUE;
          end
        else
          connector._handler(ceError); // все попытки отправить сообщение провалились - выход из пр.
      end;
  end;
end;

procedure TConnector.Close;
begin
  if _connected then
    begin
      msg_sending := FormatMsg(MSG_CLOSE);
      hMsg := CallContactService(hCurrContact, PSS_MESSAGE, 0, LPARAM(PChar(msg_sending)));
      sendTimer.Enabled := FALSE;
      _connected := FALSE;
      _handler(ceDisconnected);
    end;
{$IFDEF DEBUG_LOG}
  CloseLog;
{$ENDIF}
end;


procedure TConnector.SendData(const d: string);
begin
  if d = MSG_CLOSE then
    connector._handler(ceError)
  else
    begin
      msg_buf := msg_buf + d;
      sendTimer.Enabled := TRUE; // Отослать сообщение с некоторой оттяжкой -> всё одним пакетом
    end; { if d = MSG_CLOSE }
end;


class function TConnector.Create(hContact: THandle; h: TConnectorHandler): TConnector;
begin
  if CONNECTOR_INSTANCES > 0 then
    raise Exception.Create('No more than 1 instance of TConnector is possible!'); // ***
  if connector = nil then
    begin
      connector := inherited Create(nil); // Owner - ?
      connector._handler := h; // ***
      // TODO: добавить connector в список
      hFilterMsg := CreateProtoServiceFunction(M_CHESS4NET, PSR_MESSAGE, FilterMsg);
      if CallService(MS_PROTO_ISPROTOONCONTACT, hContact,
                                       LPARAM(PChar(M_CHESS4NET))) = 0 then
        CallService(MS_PROTO_ADDTOCONTACT, hContact, LPARAM(PChar(M_CHESS4NET)));
      hNotifySender := HookEvent(ME_PROTO_ACK, NotifySender);
	  CallContactService(hContact, PSS_MESSAGE, 0, LPARAM(PChar(MSG_INVITATION)));
    end;
    
  hCurrContact := hContact; // ***
  cntrMsgIn := 0;
  cntrMsgOut := 1;
  msg_sending := '';
  unformated_msg_sending := '';
  msg_buf := '';
  inc(CONNECTOR_INSTANCES);
  result := connector;

{$IFDEF DEBUG_LOG}
  connector.InitLog;
{$ENDIF}
end;


procedure TConnector.Free;
begin
  try
    Close;
    dec(CONNECTOR_INSTANCES);
    // TODO: убрать connector из хэша
    if CONNECTOR_INSTANCES = 0 then
      begin
        if hNotifySender <> 0 then UnhookEvent(hNotifySender);
        if CallService(MS_PROTO_ISPROTOONCONTACT, hCurrContact,
                                       LPARAM(PChar(M_CHESS4NET))) <> 0 then
          CallService(MS_PROTO_REMOVEFROMCONTACT, hCurrContact, LPARAM(PChar(M_CHESS4NET))); // ***
        PluginLink.DestroyServiceFunction(hFilterMsg);
        inherited;
        connector := nil;
      end;
  except
    on Exception do ; // ловля для выхода из осн. прошраммы при открытом плагине
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
          hMsg := CallContactService(hCurrContact, PSS_MESSAGE, 0, LPARAM(PChar(msg_sending)));
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
          hMsg := CallContactService(hCurrContact, PSS_MESSAGE, 0, LPARAM(PChar(msg_sending)));
        end;
    end;
end;

end.
