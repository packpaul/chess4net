unit OpeningsDBManagerUnit;

interface

uses
  Classes, SysUtils,
  //
  NonRefInterfacedObjectUnit, OpeningsDBManagerFormUnit;

type
  TOpeningsDBManager = class(TNonRefInterfacedObject, IOpeningsDBManagerProvider)
  private
    m_bEnabled: boolean;
    FOnChanged: TNotifyEvent;
    m_strlDBs: TStringList;
    m_iDBIndex: integer;

    procedure FDoChanged;

    procedure FLoadSettings;
    procedure FSaveSettings;

    function IOpeningsDBManagerProvider.GetDBIndex = FGetDBIndex;
    function FGetDBIndex: integer;

    procedure IOpeningsDBManagerProvider.SetDBIndex = FSetDBIndex;
    procedure FSetDBIndex(iIndex: integer);

    function IOpeningsDBManagerProvider.GetDBCount = FGetDBCount;
    function FGetDBCount: integer;

    function IOpeningsDBManagerProvider.GetDBs = FGetDBs;
    function FGetDBs(iIndex: integer): TFileName;

    function IOpeningsDBManagerProvider.GetDBNames = FGetDBNames;
    function FGetDBNames(iIndex: integer): WideString;

    function FGetDB: TFileName;

    function IOpeningsDBManagerProvider.GetEnabled = FGetEnabled;
    function FGetEnabled: boolean;

    procedure IOpeningsDBManagerProvider.SetEnabled = FSetEnabled;
    procedure FSetEnabled(bValue: boolean);

    function IOpeningsDBManagerProvider.AddDB = FAddDB;
    function FAddDB(const DB: TFileName): boolean;

    function IOpeningsDBManagerProvider.RemoveDB = FRemoveDB;
    function FRemoveDB(iIndex: integer): boolean;

  public
    constructor Create;
    destructor Destroy; override;

    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;

    property Enabled: boolean read m_bEnabled;

    property DB: TFileName read FGetDB; // Selected DB
    property DBIndex: integer read FGetDBIndex;
    property DBCount: integer read FGetDBCount;
    property DBs[iIndex: integer]: TFileName read FGetDBs;
  end;

implementation

uses
  IniSettingsUnit;

////////////////////////////////////////////////////////////////////////////////
// TOpeningsDBManager

constructor TOpeningsDBManager.Create;
begin
  inherited Create;
  m_bEnabled := TRUE;
  m_iDBIndex := -1;
  m_strlDBs := TStringList.Create;

  FLoadSettings;
end;


destructor TOpeningsDBManager.Destroy;
begin
  FSaveSettings;
  
  m_strlDBs.Free;

  inherited;
end;


function TOpeningsDBManager.FGetDB: TFileName;
begin
  Result := DBs[DBIndex];
end;


function TOpeningsDBManager.FGetDBIndex: integer;
begin
  Result := m_iDBIndex;
end;


procedure TOpeningsDBManager.FDoChanged;
begin
  if (Assigned(FOnChanged)) then
    FOnChanged(self);
end;


function TOpeningsDBManager.FGetEnabled: boolean;
begin
  Result := m_bEnabled;
end;


procedure TOpeningsDBManager.FSetEnabled(bValue: boolean);
begin
  if (m_bEnabled = bValue) then
    exit;

  m_bEnabled := bValue;

  FDoChanged;
end;


function TOpeningsDBManager.FAddDB(const DB: TFileName): boolean;
begin
  Result := FALSE;

  if (m_strlDBs.IndexOf(DB) >= 0) then
    exit;

  // TODO: Validate data

  m_strlDBs.Append(DB);

  if (m_strlDBs.Count = 1) then
  begin
    m_iDBIndex := 0;
    FDoChanged;
  end;

  FSaveSettings;

  Result := TRUE;
end;


function TOpeningsDBManager.FRemoveDB(iIndex: integer): boolean;
begin
  Result := FALSE;

  if (not ((iIndex >= 0) and (iIndex < m_strlDBs.Count))) then
    exit;

  m_strlDBs.Delete(iIndex);

  if (m_iDBIndex >= m_strlDBs.Count) then
    m_iDBIndex := m_strlDBs.Count - 1;

  FDoChanged;

  FSaveSettings;

  Result := TRUE;
end;


function TOpeningsDBManager.FGetDBCount: integer;
begin
  Result := m_strlDBs.Count;
end;


procedure TOpeningsDBManager.FSetDBIndex(iIndex: integer);
begin
  if ((iIndex = m_iDBIndex) or
      (not ((iIndex >= 0) and (iIndex < m_strlDBs.Count)))) then
    exit;

  m_iDBIndex := iIndex;
  
  FDoChanged;
end;


function TOpeningsDBManager.FGetDBs(iIndex: integer): TFileName;
begin
  if ((iIndex >= 0) and (iIndex < m_strlDBs.Count)) then
    Result := m_strlDBs[iIndex]
  else
    Result := '';
end;


function TOpeningsDBManager.FGetDBNames(iIndex: integer): WideString;

  function NConstructDBName(ADB: TFileName): WideString;
  begin
    Result := ExtractFileName(ADB);
  end;

var
  iNum: integer;
  i: integer;
begin // .FGetDBNames
  Result := NConstructDBName(DBs[iIndex]);

  if (Result = '') then
    exit;

  iNum := 1;
  for i := 0 to iIndex - 1 do
  begin
    if (Result = NConstructDBName(DBs[i])) then
      inc(iNum);
  end;

  if (iNum > 1) then
    Result := Result + ' (' + IntToStr(iNum) + ')';
end;


procedure TOpeningsDBManager.FLoadSettings;
begin
  with TIniSettings.Instance do
  begin
    GetDBs(TStrings(m_strlDBs), m_iDBIndex);
    // TODO: Validate data
    m_bEnabled := DBEnabled;
  end;
end;


procedure TOpeningsDBManager.FSaveSettings;
begin
  with TIniSettings.Instance do
  begin
    SetDBs(m_strlDBs, m_iDBIndex);
    DBEnabled := m_bEnabled;
  end;
end;

end.
