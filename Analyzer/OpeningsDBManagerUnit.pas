unit OpeningsDBManagerUnit;

interface

uses
  Classes,
  //
  NonRefInterfacedObjectUnit, OpeningsDBManagerFormUnit;

type
  TOpeningsDBManager = class(TNonRefInterfacedObject, IOpeningsDBManagerProvider)
  private
    m_bDBEnabled: boolean;
    FOnChanged: TNotifyEvent;
    function FGetDBName: string;
    procedure FDoChanged;

    function IOpeningsDBManagerProvider.GetDBEnabled = FGetDBEnabled;
    function FGetDBEnabled: boolean;

    procedure IOpeningsDBManagerProvider.SetDBEnabled = FSetDBEnabled;
    procedure FSetDBEnabled(bValue: boolean);

  public
    constructor Create;
    property DBName: string read FGetDBName;
    property DBEnabled: boolean read m_bDBEnabled;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

implementation

////////////////////////////////////////////////////////////////////////////////
// TOpeningsDBManager

constructor TOpeningsDBManager.Create;
begin
  inherited Create;
  m_bDBEnabled := TRUE;
end;


function TOpeningsDBManager.FGetDBName: string;
begin
  Result := 'Sicilian';
  // TODO:
end;


procedure TOpeningsDBManager.FDoChanged;
begin
  if (Assigned(FOnChanged)) then
    FOnChanged(self);
end;


function TOpeningsDBManager.FGetDBEnabled: boolean;
begin
  Result := m_bDBEnabled;
end;


procedure TOpeningsDBManager.FSetDBEnabled(bValue: boolean);
begin
  if (m_bDBEnabled = bValue) then
    exit;

  m_bDBEnabled := bValue;

  FDoChanged;
end;

end.
