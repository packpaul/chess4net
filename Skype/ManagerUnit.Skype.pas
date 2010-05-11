unit ManagerUnit.Skype;

interface

uses
  ManagerUnit;

type
  TManagerSkype = class(TManager)
  protected
    procedure ROnCreate; override;
    procedure ROnDestroy; override;
    procedure RSendData(const cmd: string = ''); override;
  end;

implementation

{$J+}

uses
  LocalizerUnit, ConnectorUnit;

////////////////////////////////////////////////////////////////////////////////
// TManagerSkype

procedure TManagerSkype.ROnCreate;
begin
  try
    TLocalizer.Instance.AddSubscriber(self);
    
    RCreateChessBoardAndDialogs;

    RLocalize;

    RSetChessBoardToView;
    RSetPrivateSettings;
        
    Connector := TConnector.Create(ConnectorHandler);

    RCreateAndPopulateExtBaseList;

    // Nick initialization
    if (not SkypeConnectionError) then
    begin
      RShowConnectingForm;
    end;

  except
    Release;
    raise;
  end;
end;


procedure TManagerSkype.ROnDestroy;
begin
  if (Assigned(Connector)) then
  begin
    Connector.Close;
  end;

  inherited ROnDestroy;
end;


procedure TManagerSkype.RSendData(const cmd: string = '');
const
  last_cmd: string = '';
begin
  if (cmd = '') then
    exit;
  last_cmd := cmd + CMD_DELIMITER;
  Connector.SendData(last_cmd);
end;

end.
