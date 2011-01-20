unit IniSettingsUnit;

interface

uses
  IniFiles;

type
  TIniSettingsID = (isidDontShowLastVersion);

  TIniSettings = class
  private
    m_IniFile: TIniFile;

    function FGetIniFileName: string;

    function FGetIntegerValue(ID: TIniSettingsID): integer;
    procedure FSetIntegerValue(ID: TIniSettingsID; iValue: integer);
    class function FGetIdentName(ID: TIniSettingsID): string;
    class function FGetDefaultIdentValue(ID: TIniSettingsID): Variant;
  public
    constructor Create;
    destructor Destroy; override;
    property DontShowLastVersion: integer index isidDontShowLastVersion
      read FGetIntegerValue write FSetIntegerValue;
  end;

implementation

uses
  Forms, SysUtils, Variants,
  //
  GlobalsLocalUnit;

const
  DEFAULT_SECTION = 'Settings';

////////////////////////////////////////////////////////////////////////////////
// TIniSettingsID

constructor TIniSettings.Create;
begin
  inherited Create;
  m_IniFile := TIniFile.Create(FGetIniFileName);
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
end;


class function TIniSettings.FGetIdentName(ID: TIniSettingsID): string;
begin
  case ID of
    isidDontShowLastVersion:
      Result := 'DontShowLastVersion';
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
  else
    Result := Unassigned;
  end;
end;

end.
