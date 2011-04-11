////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit IniSettingsUnit;

interface

uses
  Classes, IniFiles;

type
  TIniSettingsID = (isidDontShowLastVersion, isidDB, isidDBIndex, isidDBEnabled,
    isidOpeningDBRequestShow);

  TIniSettings = class
  private
    m_IniFile: TIniFile;

    constructor FCreate;

    function FGetIniFileName: string;

    function FGetIntegerValue(ID: TIniSettingsID): integer;
    procedure FSetIntegerValue(ID: TIniSettingsID; iValue: integer);

    function FGetBooleanValue(ID: TIniSettingsID): boolean;
    procedure FSetBooleanValue(ID: TIniSettingsID; bValue: boolean);

    class function FGetIdentName(ID: TIniSettingsID; iIndex: integer = 0): string;
    class function FGetDefaultIdentValue(ID: TIniSettingsID): Variant;

  public
    constructor Create;
    destructor Destroy; override;
    class function Instance: TIniSettings;
    class procedure FreeInstance; reintroduce;

    procedure GetDBs(var Data: TStrings; var iDBIndex: integer);
    procedure SetDBs(const Data: TStrings; iDBIndex: integer);

    property DontShowLastVersion: integer index isidDontShowLastVersion
      read FGetIntegerValue write FSetIntegerValue;
    property DBEnabled: boolean index isidDBEnabled
      read FGetBooleanValue write FSetBooleanValue;
    property OpeningDBRequestShow: boolean index isidOpeningDBRequestShow
      read FGetBooleanValue write FSetBooleanValue;
  end;

implementation

uses
  Forms, SysUtils, Variants,
  //
  GlobalsLocalUnit;

const
  DEFAULT_SECTION = 'Settings';
  DBS_SECTION = 'DBs';

////////////////////////////////////////////////////////////////////////////////
// TIniSettingsID

constructor TIniSettings.Create;
begin
  raise Exception.Create('TIniSettings cannot be instantiated directly!');
end;

constructor TIniSettings.FCreate;
begin
  inherited Create;
  m_IniFile := TIniFile.Create(FGetIniFileName);
end;

var
  g_Instance: TIniSettings = nil;

class function TIniSettings.Instance: TIniSettings;
begin
  if (not Assigned(g_Instance)) then
    g_Instance := TIniSettings.FCreate;
  Result := g_Instance;
end;


class procedure TIniSettings.FreeInstance;
begin
  FreeAndNil(g_Instance);
end;


destructor TIniSettings.Destroy;
begin
  m_IniFile.Free;
  inherited;
end;


function TIniSettings.FGetIniFileName: string;
begin
  Result := ChangeFileExt(Application.ExeName, '.ini');
end;


function TIniSettings.FGetIntegerValue(ID: TIniSettingsID): integer;
begin
  Result := m_IniFile.ReadInteger(DEFAULT_SECTION, FGetIdentName(ID), FGetDefaultIdentValue(ID));
end;


procedure TIniSettings.FSetIntegerValue(ID: TIniSettingsID; iValue: integer);
begin
  m_IniFile.WriteInteger(DEFAULT_SECTION, FGetIdentName(ID), iValue);
  m_IniFile.UpdateFile;
end;


function TIniSettings.FGetBooleanValue(ID: TIniSettingsID): boolean;
begin
  Result := m_IniFile.ReadBool(DEFAULT_SECTION, FGetIdentName(ID), FGetDefaultIdentValue(ID));
end;


procedure TIniSettings.FSetBooleanValue(ID: TIniSettingsID; bValue: boolean);
begin
  m_IniFile.WriteBool(DEFAULT_SECTION, FGetIdentName(ID), bValue);
  m_IniFile.UpdateFile;
end;


class function TIniSettings.FGetIdentName(ID: TIniSettingsID; iIndex: integer = 0): string;
begin
  case ID of
    isidDontShowLastVersion:
      Result := 'DontShowLastVersion';
    isidDB:
      Result := 'DB' + IntToStr(iIndex);
    isidDBIndex:
      Result := 'DBIndex';
    isidDBEnabled:
      Result := 'DBEnabled';
    isidOpeningDBRequestShow:
      Result := 'OpeningDBRequestShow';
  else
    Result := '';
  end;

  Assert(Result <> '');
end;


class function TIniSettings.FGetDefaultIdentValue(ID: TIniSettingsID): Variant;
begin
  case ID of
    isidDontShowLastVersion:
      Result := CHESS4NET_VERSION;
    isidDBEnabled, isidOpeningDBRequestShow:
      Result := TRUE;
  else
    Result := Unassigned;
  end;
end;


procedure TIniSettings.GetDBs(var Data: TStrings; var iDBIndex: integer);
var
  iNum: integer;
begin
  if (not Assigned(Data)) then
    exit;

  if (not m_IniFile.SectionExists(DBS_SECTION)) then
  begin
    iDBIndex := Data.Add(ExtractFilePath(Application.ExeName) + 'DBs\Sicilian');
    exit;
  end;

  Data.Clear;

  iNum := 0;

  while (m_IniFile.ValueExists(DBS_SECTION, FGetIdentName(isidDB, iNum))) do
  begin
    Data.Append(m_IniFile.ReadString(DBS_SECTION, FGetIdentName(isidDB, iNum), ''));
    inc(iNum);
  end;

  iDBIndex := m_IniFile.ReadInteger(DBS_SECTION, FGetIdentName(isidDBIndex), -1);
end;


procedure TIniSettings.SetDBs(const Data: TStrings; iDBIndex: integer);
var
  i: integer;
begin
  if (not Assigned(Data)) then
    exit;

  m_IniFile.EraseSection(DBS_SECTION);

  for i := 0 to Data.Count - 1 do
    m_IniFile.WriteString(DBS_SECTION, FGetIdentName(isidDB, i), Data[i]);

  m_IniFile.WriteInteger(DBS_SECTION, FGetIdentName(isidDBIndex), iDBIndex);

  m_IniFile.UpdateFile;
end;

initialization

finalization
  TIniSettings.FreeInstance;

end.
