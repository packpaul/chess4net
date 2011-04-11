////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit ConnectorUnit;

interface

uses
  SysUtils, Classes, ScktComp, ExtCtrls;

type
  TConnectionState = set of (csServer, csClient);

  TConnectorEvent = (ceConnected, ceDisconnected, ceData, ceError);

  TConnectorHandler = procedure(ce: TConnectorEvent; d1: pointer = nil;
                                d2: pointer = nil) of object;

  TConnector = class(TDataModule)
    Client: TClientSocket;
    Server: TServerSocket;
    sendTimer: TTimer;
    procedure ClientConnect(Sender: TObject; Socket: TCustomWinSocket);
    procedure ClientError(Sender: TObject; Socket: TCustomWinSocket;
      ErrorEvent: TErrorEvent; var ErrorCode: Integer);
    procedure ServerClientError(Sender: TObject; Socket: TCustomWinSocket;
      ErrorEvent: TErrorEvent; var ErrorCode: Integer);
    procedure ClientRead(Sender: TObject; Socket: TCustomWinSocket);
    procedure ServerClientRead(Sender: TObject; Socket: TCustomWinSocket);
    procedure ClientDisconnect(Sender: TObject; Socket: TCustomWinSocket);
    procedure ServerClientConnect(Sender: TObject;
      Socket: TCustomWinSocket);
    procedure ServerClientDisconnect(Sender: TObject;
      Socket: TCustomWinSocket);
    procedure sendTimerTimer(Sender: TObject);
  private
    Handler: TConnectorHandler;
    prt: integer;
    sendTextBuffer: string;

    procedure SetPort(const p: integer);

  public
    State: TConnectionState;
    ClientConnected: boolean;
    constructor Create(Owner: TComponent;  h: TConnectorHandler); reintroduce;
    property Port: integer read prt write SetPort;
    function IP: string; overload;
    function IP(const cl: pointer): string; overload;
    procedure OpenServer(port: integer = 0);
    procedure OpenClient(host: string; port: integer = 0);
    procedure Close; overload;
    procedure Close(const cs: TConnectionState); overload;
    procedure Close(const cl: pointer); overload;
    procedure SendData(const d: string);
  end;

implementation

{$R *.dfm}

function TConnector.IP: string; // ?????
begin
  if Server.Active then Result:= Server.Socket.LocalAddress // не работает
    else
  if Client.Active then Result:= Client.Socket.LocalAddress
    else Result:= '';
end;

function TConnector.IP(const cl: pointer): string;
begin
  if (csServer in State) then
    try
      Result:= TCustomWinSocket(cl).RemoteAddress;
    except
      on Exception do Handler(ceError); // Клиент cl не существует
    end
  else
    Result:= '';
end;

procedure TConnector.SetPort(const p: integer);
begin
  if State = [] then prt:= p
    else Exception.Create('Cannot assign port');
end;

constructor TConnector.Create(Owner: TComponent; h: TConnectorHandler);
begin
  inherited Create(Owner);
  Handler:= h;
end;

procedure TConnector.OpenServer(port: integer);
  begin
    try
      if Server.Active then Exception.Create('Server is already running');
      if port <> 0 then SetPort(port);
      Server.Port:= prt;
      State:= State + [csServer];
      Server.Open;
    except
      on Exception do
        begin
          Handler(ceError); // Ошибка открытия сервера
          State:= State - [csServer];
        end;
    end;
  end;

procedure TConnector.OpenClient(host: string; port: integer);
  function IsIP(ipstr: string): boolean;
  var
    i: integer;
    s: string;
  begin
    Result:= FALSE;
    ipstr:= ipstr + '.';
    for i:= 1 to 4 do
      try
        s:= copy(ipstr, 1, pos('.', ipstr) - 1);
        if not ((StrToInt(s) >= 0) and (StrToInt(s) <= 255)) then exit;
        Delete(ipstr, 1, length(s) + 1);
      except
        on Exception do exit;
      end;
    if ipstr = '' then Result:= TRUE;
  end;
  begin
    try
      if Client.Active then Exception.Create('Client is already created');
      if (host = '') or ((csServer in State) and (host <> IP)) then //????
        Exception.Create('Wrong host name');
      if isIP(host) then Client.Address:= host
        else Client.Host:= host;
      if port <> 0 then SetPort(port);
      Client.Port:= prt;
      State:= State + [csClient]; ClientConnected:= FALSE;
      Client.Open;
    except
      on Exception do
        begin
          State:= State - [csClient];
          Handler(ceError); // Ошибка открытия клиента
        end;
    end;
  end;

procedure TConnector.Close(const cs: TConnectionState);
  begin
    if csServer in cs then
      begin
        State:= State - [csServer];
        Server.Close;
      end;
    if csClient in cs then
      begin
        State:= State - [csClient];
        Client.Close;
        ClientConnected:= TRUE;
      end;
  end;

procedure TConnector.SendData(const d: string);
  begin
    sendTextBuffer := sendTextBuffer + d;
    sendTimer.Enabled := TRUE;
  end;

procedure TConnector.ClientConnect(Sender: TObject; Socket: TCustomWinSocket);
begin
  ClientConnected:= TRUE;
{
  State:= State + [csClient];
}
  Handler(ceConnected);
end;

procedure TConnector.ClientError(Sender: TObject; Socket: TCustomWinSocket;
  ErrorEvent: TErrorEvent; var ErrorCode: Integer);
begin
  // Обработка ошибок со стороны клиента
  case ErrorEvent of
    eeConnect:
      if not ClientConnected then Client.Open; // Клиент ожидает сервер
    eeLookup: Client.Close; // Неправильный адрес хоста
  else
    Handler(ceError);
  end;
  ErrorCode:= 0;
end;

procedure TConnector.ServerClientError(Sender: TObject;
  Socket: TCustomWinSocket; ErrorEvent: TErrorEvent;
  var ErrorCode: Integer);
begin
  // Обработка ошибок со стороны сервера
  Handler(ceError);
end;

procedure TConnector.ClientRead(Sender: TObject; Socket: TCustomWinSocket);
var
  data: string;
begin
  data:= Socket.ReceiveText;
  Handler(ceData, @data);
end;

procedure TConnector.ServerClientRead(Sender: TObject; Socket: TCustomWinSocket);
var
  data: string;
begin
  data:= Socket.ReceiveText;
  Handler(ceData, @data, Socket);
end;

procedure TConnector.ClientDisconnect(Sender: TObject;
  Socket: TCustomWinSocket);
begin
  Handler(ceDisconnected);
end;

procedure TConnector.ServerClientConnect(Sender: TObject;
                                         Socket: TCustomWinSocket);
begin
  Handler(ceConnected, Socket);
end;

procedure TConnector.ServerClientDisconnect(Sender: TObject;
  Socket: TCustomWinSocket);
begin
  Handler(ceDisconnected, Socket);
end;

procedure TConnector.Close(const cl: pointer);
begin
  if not (csServer in State) then exit;
  try
    TCustomWinSocket(cl).Close;
  except
    on Exception do Handler(ceError); // Клиент cl не существует
  end;
end;


procedure TConnector.Close;
begin
  Close(State);
end;


procedure TConnector.sendTimerTimer(Sender: TObject);
begin
  sendTimer.Enabled := FALSE;
  try
    if csServer in State then
      Server.Socket.Connections[0].SendText(sendTextBuffer)
    else
    if csClient in State then
      Client.Socket.SendText(sendTextBuffer);
    sendTextBuffer := '';
  except
    on Exception do Handler(ceError); // Ошибка отрпавления сообщения клиентом
  end;
end;

end.
