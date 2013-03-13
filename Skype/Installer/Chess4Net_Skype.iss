; Actual for Inno Setup Compiler 5.4.2(u)

#define MyAppName "Chess4Net Skype"
#define MyAppVersion "2013.1"
#define MyAppPublisher "Chess4Net"
#define MyAppURL "http://chess4net.ru"
#define MyAppExeName "Chess4Net_Skype.exe"
#define MyAppIniName "Chess4Net.ini"
#define MyAppId "{{6529028E-CAC9-4039-A101-8A7131F50C43}"

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
DefaultDirName={pf}\Chess4Net\Skype
DefaultGroupName=Chess4Net\Skype
DisableProgramGroupPage=yes
SourceDir=..\..\bin\Chess4Net_Skype
;InfoAfterFile=Readme.txt
OutputDir=..\..\bin
OutputBaseFilename=Chess4Net_2013.1b_Skype
Compression=lzma
SolidCompression=yes
ChangesAssociations=yes

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"

[Files]
Source: "Chess4Net_Skype.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "Lang.ini"; DestDir: "{app}"; Flags: ignoreversion
;Source: "Chess4Net_GAMELOG.txt"; DestDir: "{app}"; Flags: ignoreversion
Source: "*.pos"; DestDir: "{app}"; Flags: ignoreversion
Source: "*.mov"; DestDir: "{app}"; Flags: ignoreversion
Source: "Readme.txt"; DestDir: "{app}"; Flags: ignoreversion isreadme
Source: "Readme_RU.txt"; DestDir: "{app}"; Flags: ignoreversion

; NOTE: Don't use "Flags: ignoreversion" on any shared system files

[Icons]
Name: "{group}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"
Name: "{group}\Readme"; Filename: "{app}\Readme.txt"
Name: "{group}\{cm:UninstallProgram,{#MyAppName}}"; Filename: "{uninstallexe}"
Name: "{commondesktop}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; Tasks: desktopicon

[Registry]
Root: HKLM; Subkey: "Software\Chess4Net"; Flags: uninsdeletekeyifempty
Root: HKLM; Subkey: "Software\Chess4Net\Skype"; ValueType: string; ValueName: ""; ValueData: "{app}\{#MyAppExeName}"; Flags: uninsdeletekey

[Run]
Filename: "{app}\{#MyAppExeName}"; Description: "{cm:LaunchProgram,{#StringChange(MyAppName, "&", "&&")}}"; Flags: nowait postinstall skipifsilent

[UninstallDelete]
Type: filesandordirs; Name: "{userprograms}\Chess4Net\Skype"
Type: dirifempty; Name: "{userprograms}\Chess4Net"
Type: filesandordirs; Name: "{userappdata}\Chess4Net\Skype"
Type: dirifempty; Name: "{userappdata}\Chess4Net"