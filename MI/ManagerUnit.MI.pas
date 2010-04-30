unit ManagerUnit.MI;

interface

uses
  ManagerUnit, ControlUnit, ConnectorUnit;

type
  TManagerMI = class(TManager, IMirandaPlugin)
    constructor Create(Connector: TConnector); reintroduce;
    procedure Start;
  end;

implementation

uses
  SysUtils;

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

  RCreateChessBoardAndDialogs;

  try
    RSetChessBoardToView;
    RSetPrivateSettings;    

    RShowConnectingForm;

    if (not Connector.Open(FALSE)) then
      raise Exception.Create('ERROR: Cannot open connector!');
  except
    ChessBoard.Release;
    ChessBoard := nil;
    raise;
  end;
end;

end.
