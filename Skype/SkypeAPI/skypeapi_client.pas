unit SkypeAPI_Client;

interface

uses
  SkypeAPI_Skype;

type
  TClient = class(TInterfacedObject, IClient)
  private
    function GetIsRunning: Boolean;
  public
    destructor Destroy; override;
    procedure Start(Minimized: Boolean; Nosplash: Boolean);
    property IsRunning: Boolean read GetIsRunning;
  end;

implementation

////////////////////////////////////////////////////////////////////////////////
// TClient

destructor TClient.Destroy;
begin
  inherited;
end;


function TClient.GetIsRunning: Boolean;
begin
  Result := FALSE;
end;


procedure TClient.Start(Minimized: Boolean; Nosplash: Boolean);
begin
  // TODO:
end;

end.