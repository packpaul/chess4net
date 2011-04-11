////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit ConnectorUnit;

interface

uses
  Classes, ExtCtrls;

type
  TConnectorEvent = (ceConnected, ceDisconnected, ceData, ceError);

  TConnectorHandler = procedure(ce: TConnectorEvent; d1: pointer = nil;
                                d2: pointer = nil) of object;

  TConnector = class(TDataModule)
    sendTimer: TTimer;
    procedure sendTimerTimer(Sender: TObject);
  private
    _connected : boolean;
    _handler: TConnectorHandler;

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
    class function Create(UIN: integer; h: TConnectorHandler): TConnector; reintroduce;
  end;

function MessageGot(data: Pointer): Pointer;
function MessageSent(data: Pointer): Pointer;

implementation

{$R *.dfm}

{$J+} {$I-}

uses
  SysUtils, StrUtils, Windows,
  CallExec, plugin, pluginutil,
  GlobalsLocalUnit;

var
  connector: TConnector; // singleton

  gUIN: integer;

  msg_sending, unformated_msg_sending: string; // отсылаемое сообщение

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
  MAX_RESEND_TRYS = 9; // максимальное количество попыток пересыла


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


procedure SendMessage(const vMessage: string);
begin
  if AndRQVersion >= $090704  then
    begin
      RQ_SendHiddenMsg(gUIN, 0, vMessage); // это может не работать для RnQ
      NotifySender(vMessage);
    end
  else // = $090703
    RQ_SendMsg(gUIN, 0, vMessage);
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
          connector._handler(ceConnected);
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
        Result := FALSE;
    end;
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


class function TConnector.Create(UIN: integer; h: TConnectorHandler): TConnector;
begin
  if CONNECTOR_INSTANCES > 0 then
    raise Exception.Create('No more than 1 instance of TConnector is possible!');

  gUIN := UIN;

  if not Assigned(connector) then
    begin
      connector := inherited Create(nil); // Owner - ?
      connector._handler := h;
      // TODO: добавить connector в список
      SendMessage(MSG_INVITATION);
    end;

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


function MessageGot(data: Pointer) : Pointer;
var
  UIN, flags: integer;
  date: TDateTime;
  msg: string;
begin
  Result := nil;
  if not Assigned(connector) then
    exit;
  RQ__ParseMsgGotString(data, UIN, flags, date, msg);
  if UIN <> gUIN then
    exit;

  if FilterMsg(msg) then
    begin
      if AndRQVersion >= $090704 then
        Result := str2comm(char(PM_ABORT)) // подавление сообщения
      else
        Result := str2comm(char(PM_DATA) + _istring(#0));
    end;
end;

procedure ProcData(data: pointer);

  function __istring_at(p: pointer; ofs: integer): string;
  begin
    inc(integer(p), ofs);
    Result := PString(p)^;
  end;

var
  msg: string;
begin
  msg := __istring_at(data, 14);
  msg := ' ';
end;


function MessageSent(data: Pointer): Pointer;
var
  UIN, flags: integer;
  msg: string;
begin // AndRQVersion = $090703
  Result := nil;
  if not Assigned(connector) then
    exit;
  RQ__ParseMsgSentString(data, UIN, flags, msg);
  if UIN <> gUIN then exit;

  NotifySender(msg);
end;


end.
