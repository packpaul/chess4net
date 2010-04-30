unit ManagerUnit.MI;

interface

uses
  SysUtils,
  //
  ManagerUnit, ControlUnit, ConnectorUnit;

type
  EManagerMI = class(Exception);
  TManagerMI = class(TManager, IMirandaPlugin)
  protected
    procedure Start;
    procedure ROnCreate; override;
    procedure ROnDestroy; override;
    procedure ConnectorHandler(e: TConnectorEvent;
      d1: pointer = nil; d2: pointer = nil); override;
    procedure RSetOpponentClientVersion(lwVersion: LongWord); override;
    procedure RSendData(const cmd: string); override;
  public
    constructor Create(Connector: TConnector); reintroduce;
  end;

implementation

uses
  Types, StrUtils,
  //
  LocalizerUnit;

////////////////////////////////////////////////////////////////////////////////
// TManagerMI

constructor TManagerMI.Create(Connector: TConnector);
begin
  self.Connector := Connector;
  RCreate;
end;


procedure TManagerMI.Start;
begin
  if (Assigned(ChessBoard)) then
  begin
    Show;
    exit;
  end;

//  RCreateChessBoardAndDialogs;

  try
    if (not Connector.Open(FALSE)) then
      raise EManagerMI.Create('ERROR: Cannot open connector!');

    RCreateChessBoardAndDialogs;
    RSetChessBoardToView;

    RSetPrivateSettings;

    RShowConnectingForm;

  except
    if (Assigned(ChessBoard)) then
    begin
      ChessBoard.Release;
      ChessBoard := nil;
    end;
    raise;
  end;
end;


procedure TManagerMI.ROnCreate;
begin
  RCreateAndPopulateExtBaseList;

  // Nicks initialization
  PlayerNick := Connector.OwnerNick;

  OpponentNick := Connector.ContactNick;
  OpponentId := IntToStr(Connector.ContactID);
//  OpponentNickId := OpponentNick + OpponentId;
//    connectionOccured := FALSE;

  TLocalizer.Instance.AddSubscriber(self);
  RLocalize;
end;


procedure TManagerMI.ROnDestroy;
begin
  if (Assigned(Connector)) then
  begin
    Connector.Free;
    Connector := nil;
  end;

  inherited ROnDestroy;
end;


procedure TManagerMI.ConnectorHandler(e: TConnectorEvent;
  d1: pointer = nil; d2: pointer = nil);
var
  iData: integer;
  strCmd: string;
  strLeft: string;
begin
  case e of
    ceError:
    begin
      Connector.Close;
      inherited ConnectorHandler(e, d1, d2);      
    end;

    ceData:
    begin
      Assert(High(TStringDynArray(d1)) >= 0);
      iData := 0;
      repeat
        strLeft := TStringDynArray(d1)[iData];
        inc(iData);
        strCmd := IfThen((iData <= High(TStringDynArray(d1))), '*');

        RHandleConnectorDataCommand(strLeft);
      until (strCmd = '');

    end; // ceData

  else
    inherited ConnectorHandler(e, d1, d2);
  end; // case
end;


procedure TManagerMI.RSetOpponentClientVersion(lwVersion: LongWord);
begin
  inherited RSetOpponentClientVersion(lwVersion);

  if (lwVersion >= 200901) then
    Connector.MultiSession := TRUE;
end;


procedure TManagerMI.RSendData(const cmd: string);
begin
  Connector.SendData(cmd);
end;

end.
