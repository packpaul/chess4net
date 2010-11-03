unit ConnectorSocketUnit;

{$MODE Delphi}

interface

uses
  SysUtils, Classes, ExtCtrls, LResources, lNetComponents, lNet;

type
  TConnectionState = set of (csServer, csClient);

  TConnectorEvent = (ceConnected, ceDisconnected, ceData, ceError);

  TConnectorHandler = procedure(ce: TConnectorEvent; d1: pointer = nil;
                                d2: pointer = nil) of object;

  { TConnector }

  TConnector = class(TDataModule)

    LTCPComponent: TLTCPComponent;
    sendTimer: TTimer;

    procedure LTCPComponentAccept(aSocket: TLSocket);
    procedure LTCPComponentConnect(aSocket: TLSocket);
    procedure LTCPComponentDisconnect(aSocket: TLSocket);
    procedure LTCPComponentError(const msg: string; aSocket: TLSocket);
    procedure LTCPComponentReceive(aSocket: TLSocket);
    procedure sendTimerTimer(Sender: TObject);

  private
    Handler: TConnectorHandler;
    sendTextBuffer: string;
    _socket: TLSocket;
    _state: TConnectionState;
    _host: string;
    _port: word;
    function DoOpenClient: boolean;

  public
    constructor Create(Owner: TComponent;  h: TConnectorHandler); reintroduce;
    procedure OpenServer(port: word = 5555);
    procedure OpenClient(const host: string; port: word = 5555);
    procedure Close; overload;
    procedure SendData(const d: string);
    property State: TConnectionState read _state;
  end;

implementation

type
  TOpenClientOperator = class(TThread)
  private
    _connector: TConnector;
    _clientOpened: boolean;
    procedure OpenClientOperatorTerminated(Sender: TObject);
  protected
    procedure Execute; override;
  public
    constructor Create(connector: TConnector);
  end;

{------------------------ TConnector ------------------------------}

constructor TConnector.Create(Owner: TComponent; h: TConnectorHandler);
begin
  inherited Create(Owner);
  _socket := nil;
  _state := [];
  Handler := h;
end;

procedure TConnector.OpenServer(port: word = 5555);
begin
  try
    if csServer in _state then
      Exception.Create('Server is already running');
    Assert(port > 0);
    _state := [csServer];
    LTCPComponent.Listen(port);
  except
    _state := [];
    Handler(ceError);
  end;
end;


function TConnector.DoOpenClient: boolean;
begin
  Result := LTCPComponent.Connect(_host, _port);
end;


procedure TConnector.OpenClient(const host: string; port: word = 5555);
begin
  Assert(port > 0);

  _host := host;
  _port := port;
  
  try
    _state := [];
    if csClient in _state then
      Exception.Create('Client is already created');
    if _host = '' then
      Exception.Create('Wrong host name');
{$IFDEF WINDOWS}
    if (not DoOpenClient) then
      Exception.Create('Cannot open client');
{$ENDIF}
{$IFDEF UNIX}
    TOpenClientOperator.Create(self); // Otherwise windows repaint is not working in Linux
{$ENDIF}
    _state := [csClient];
  except
    Handler(ceError);
  end;
end;


procedure TConnector.SendData(const d: string);
begin
  if _state = [] then
    Exception.Create('Cannont send data to closed connector');
  sendTextBuffer := sendTextBuffer + d;
  sendTimer.Enabled := TRUE;
end;

procedure TConnector.LTCPComponentAccept(aSocket: TLSocket);
begin
  if Assigned(_socket) then
  begin
    aSocket.Disconnect;
    exit;
  end;
  _socket := aSocket;
  sendTextBuffer := '';
  Handler(ceConnected, _socket);
end;

procedure TConnector.LTCPComponentError(const msg: string; aSocket: TLSocket);
begin
  if msg = 'Error on connect: connection refused' then
    LTCPComponent.Connect
  else
    Handler(ceError);
end;

procedure TConnector.LTCPComponentConnect(aSocket: TLSocket);
begin
  _socket := aSocket;
  _state := [csClient];
  sendTextBuffer := '';
  Handler(ceConnected, _socket);
end;

procedure TConnector.LTCPComponentDisconnect(aSocket: TLSocket);
begin
  Handler(ceDisconnected, _socket);
end;

procedure TConnector.LTCPComponentReceive(aSocket: TLSocket);
var
  data: string;
begin
  _socket.GetMessage(data);
  if length(data) > 0 then
    Handler(ceData, @data);
end;

procedure TConnector.Close;
begin
  _socket := nil;
  LTCPComponent.Disconnect;
  _state := []
end;


procedure TConnector.sendTimerTimer(Sender: TObject);
var
  res: integer;
begin
  if (_socket.SendMessage(sendTextBuffer) = -1) then
    Handler(ceError)
  else
    sendTextBuffer := '';
  // иначе сообщение пересылается
  sendTimer.Enabled := FALSE;
end;

{------------------------- TOpenClientOperator --------------------------}

constructor TOpenClientOperator.Create(connector: TConnector);
begin
  _connector := connector;

  inherited Create(TRUE);
  Priority := tpLower;
  FreeOnTerminate := TRUE;
  OnTerminate := OpenClientOperatorTerminated;
  Resume;
end;


procedure TOpenClientOperator.Execute;
begin
  Sleep(500);
  _clientOpened := _connector.DoOpenClient;
end;
  
  
procedure TOpenClientOperator.OpenClientOperatorTerminated(Sender: TObject);
begin
  if (not _clientOpened) then
  begin
    _connector._state := [];
    _connector.Handler(ceError);
  end;
end;


initialization
  {$i ConnectorSocketUnit.lrs}

end.
