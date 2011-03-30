; Actual for Inno Setup Compiler 5.4.2(u)

#define MyAppName "Chess4Net Analyzer"
#define MyAppVersion "2011.2"
#define MyAppPublisher "Chess4Net"
#define MyAppURL "http://chess4net.ru"
#define MyAppExeName "Chess4Net_Analyzer.exe"
#define MyAppIniName "Chess4Net_Analyzer.ini"
#define MyAppId "{{A8662FB1-EF31-4087-BAD5-25ECF2A11F05}"

[Setup]
; NOTE: The value of AppId uniquely identifies this application.
; Do not use the same AppId value in installers for other applications.
; (To generate a new GUID, click Tools | Generate GUID inside the IDE.)
AppId={#MyAppId}
AppName={#MyAppName}
AppVersion={#MyAppVersion}
AppVerName={#MyAppName} {#MyAppVersion}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName={pf}\Chess4Net\Analyzer
DefaultGroupName=Chess4Net\Analyzer
DisableProgramGroupPage=yes
SourceDir=..\..\bin\Chess4Net_Analyzer
;InfoAfterFile=ReadMe.txt
OutputDir=..\..\bin
OutputBaseFilename=Chess4Net_Analyzer_2011.2
Compression=lzma
SolidCompression=yes
ChangesAssociations=yes

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"

[Files]
Source: "Chess4Net_Analyzer.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "DBs\*"; DestDir: "{app}\DBs"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "Engines\*"; DestDir: "{app}\Engines"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "R.Fischer-B.Larsen (Portoroz Interzonal).c4n"; DestDir: "{app}"; Flags: ignoreversion
Source: "R.Fischer-B.Larsen (Portoroz Interzonal).pgn"; DestDir: "{app}"; Flags: ignoreversion
Source: "ReadMe.txt"; DestDir: "{app}"; Flags: ignoreversion isreadme
; NOTE: Don't use "Flags: ignoreversion" on any shared system files

[Icons]
Name: "{group}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"
Name: "{group}\ReadMe"; Filename: "{app}\ReadMe.txt"
Name: "{group}\{cm:UninstallProgram,{#MyAppName}}"; Filename: "{uninstallexe}"
Name: "{commondesktop}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; Tasks: desktopicon

[Registry]
Root: HKCR; Subkey: ".c4n"; ValueType: string; ValueName: ""; ValueData: "{#MyAppId}"; Flags: uninsdeletevalue
Root: HKCR; Subkey: ".pgn"; ValueType: string; ValueName: ""; ValueData: "{#MyAppId}"; Flags: uninsdeletevalue
Root: HKCR; Subkey: "{#MyAppId}"; ValueType: string; ValueName: ""; ValueData: "{#MyAppName}"; Flags: uninsdeletevalue
Root: HKCR; Subkey: "{#MyAppId}\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\{#MyAppExeName},0"
Root: HKCR; Subkey: "{#MyAppId}\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#MyAppExeName}"" ""%1"""

[Run]
Filename: "{app}\{#MyAppExeName}"; Description: "{cm:LaunchProgram,{#StringChange(MyAppName, "&", "&&")}}"; Flags: nowait postinstall skipifsilent

[Code]

function InitializeUninstall(): Boolean;
var
  IniFileName: string;
begin
  IniFileName := ExpandConstant('{app}\{#MyAppIniName}');
  DeleteFile(IniFileName);
  Result := TRUE;
end;

