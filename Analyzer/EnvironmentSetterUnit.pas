////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit EnvironmentSetterUnit;

interface

uses
  SysUtils;

type
  TEnvironmentSetter = class
  private
    constructor FCreate;

    function FGetApplicationPath: string;
    function FGetUserDataPath: string;
    function FGetIniFilePath: string;

    function FCreateLink(AFileOrDir, ADestinationDir, ALinkName: TFileName): boolean;

    function FIsRunFromInstalledApplication: boolean;

    procedure FSetEnvironment;
    procedure FCreateEnvironment;

  public
    constructor Create;
    class procedure SetEnvironment;
  end;

implementation

uses
  Forms, ShlObj, Windows, ComObj, ActiveX, Registry,
  //
  GlobalsUnit;

const
  PATH_ADDON = 'Chess4Net\Analyzer\';
  REGISTRY_KEY = 'Software\Chess4Net\Analyzer';

  C4N_FILE = 'R.Fischer-B.Larsen (Portoroz Interzonal).c4n';
  PGN_FILE = 'R.Fischer-B.Larsen (Portoroz Interzonal).pgn';
  TRAINING_DIR = 'Training';

function GetSpecialFolderPath(out strPath: string; iFolderCSIDL: integer): boolean;
var
  arrPathCh: array[0..MAX_PATH] of char;
begin
  FillChar(arrPathCh, Length(arrPathCh), 0);
  Result := SHGetSpecialFolderPath(Application.Handle, arrPathCh, iFolderCSIDL, TRUE);
  if (Result) then
    strPath := arrPathCh;
end;

////////////////////////////////////////////////////////////////////////////////
// TEnvironmentSetter


constructor TEnvironmentSetter.Create;
begin
  raise Exception.Create('TEnvironmentSetter cannot be instantiated directly!')
end;


constructor TEnvironmentSetter.FCreate;
begin
  inherited Create;
end;


class procedure TEnvironmentSetter.SetEnvironment;
begin
  with TEnvironmentSetter.FCreate do
  try
    FSetEnvironment;
  finally
    Free;
  end;
end;


procedure TEnvironmentSetter.FSetEnvironment;
const
  ENVIRONMENT_SET: boolean = FALSE;
var
  bFirstTimeRun: boolean;
begin
  if (ENVIRONMENT_SET) then
    exit;

  Chess4NetPath := FGetApplicationPath;

  if (FIsRunFromInstalledApplication) then
  begin
    Chess4NetIniFilePath := FGetIniFilePath;

    if (ForceDirectories(Chess4NetIniFilePath)) then
      bFirstTimeRun := (not FileExists(Chess4NetIniFilePath + '*.ini'))
    else
    begin
      bFirstTimeRun := FALSE;
      Chess4NetIniFilePath := Chess4NetPath;
    end;

    Chess4NetUserDataPath := FGetUserDataPath;
    if (ForceDirectories(Chess4NetUserDataPath)) then
    begin
      if (bFirstTimeRun) then
        FCreateEnvironment;
    end
    else
      Chess4NetUserDataPath := Chess4NetPath;
  end
  else
  begin
    Chess4NetIniFilePath := Chess4NetPath;
    Chess4NetUserDataPath := Chess4NetPath;
  end;

  ENVIRONMENT_SET := TRUE;
end;


procedure TEnvironmentSetter.FCreateEnvironment;
begin
  FCreateLink(Chess4NetPath + C4N_FILE, Chess4NetUserDataPath, C4N_FILE);
  FCreateLink(Chess4NetPath + PGN_FILE, Chess4NetUserDataPath, PGN_FILE);
  FCreateLink(Chess4NetPath + TRAINING_DIR, Chess4NetUserDataPath, TRAINING_DIR);
end;


function TEnvironmentSetter.FIsRunFromInstalledApplication: boolean;
var
  strInstalledApplicationPath: string;
begin
  Result := FALSE;

  with TRegistry.Create(KEY_READ) do
  try
    RootKey := HKEY_LOCAL_MACHINE;
    if (not OpenKeyReadOnly(REGISTRY_KEY)) then
      exit;
    strInstalledApplicationPath := ReadString('');
  finally
    Free;
  end;

  Result := SameFileName(Application.ExeName, strInstalledApplicationPath);
end;


function TEnvironmentSetter.FGetApplicationPath: string;
begin
  Result := ExtractFilePath(Application.ExeName);
end;


function TEnvironmentSetter.FGetUserDataPath: string;
begin
  if (GetSpecialFolderPath(Result, CSIDL_PERSONAL)) then
    Result := IncludeTrailingPathDelimiter(Result) + PATH_ADDON
  else
    Result := '';
end;


function TEnvironmentSetter.FGetIniFilePath: string;
begin
  if (GetSpecialFolderPath(Result, CSIDL_APPDATA)) then
    Result := IncludeTrailingPathDelimiter(Result) + PATH_ADDON
  else
    Result := '';
end;


function TEnvironmentSetter.FCreateLink(AFileOrDir, ADestinationDir, ALinkName: TFileName): boolean;
const
  LNK_EXT = '.lnk';
var
  IObject: IUnknown;
  ISLink: IShellLink;
  IPFile: IPersistFile;
  wstrLinkName: WideString;
  bIsFile: boolean;
begin
  Result := FALSE;

  AFileOrDir := ExpandFileName(AFileOrDir);

  if (FileExists(AFileOrDir)) then
    bIsFile := TRUE
  else if (DirectoryExists(AFileOrDir)) then
    bIsFile := FALSE
  else
    exit;

  ADestinationDir := IncludeTrailingPathDelimiter(ADestinationDir);
  if (not SameText(ExtractFileExt(ALinkName), LNK_EXT)) then
    ALinkName := ALinkName + LNK_EXT;

  wstrLinkName := ADestinationDir + ALinkName;

  if (FileExists(wstrLinkName)) then
    exit;

  IObject := CreateComObject(CLSID_ShellLink);
  ISLink := IObject as IShellLink;
  IPFile := IObject as IPersistFile;

  ISLink.SetPath(PChar(AFileOrDir));
  if (bIsFile) then
    ISLink.SetWorkingDirectory(PChar(ExtractFilePath(AFileOrDir)));

  if (not ForceDirectories(ADestinationDir)) then
    exit;

  IPFile.Save(PWChar(wstrLinkName), FALSE);

  Result := TRUE;
end;

end.
