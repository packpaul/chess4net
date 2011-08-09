////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit GlobalsLocalUnit;

// Модуль глобальных переменных и констант версии для QIP

interface

uses
  Graphics;

procedure InitGlobals(pluginFileName: WideString);
procedure DeinitGlobals;

const
  CHESS4NET = 'Chess4Net';
  CHESS4NET_VERSION = 200901; // 2009.1
  CHESS4NET_TITLE = 'Chess4Net 2009.1 [QIP] (http://chess4net.ru)';
  MSG_INVITATION = 'Wellcome to Chess4Net. If you don''t have it, please download it from http://chess4net.ru';
  DIALOG_CAPTION = CHESS4NET;

  PLUGIN_NAME: WideString = CHESS4NET;
  PLUGIN_AUTHOR: WideString = 'Pavel Perminov';
  PLUGIN_VER_MAJOR = 2009;
  PLUGIN_VER_MINOR = 1;

  PLUGIN_WRONG_SDK_VERSION = 'Current version of SDK is less than 1.3.0' + #13#10 +
                             'Update your QIP! Chess4Net plugin won''t be available.';
  PLUGIN_WRONG_PROTOCOL = 'Chess4Net may not be functioning properly with protocols other than ICQ.' + #13#10 +
                          'Continue anyway?';

  PLUGIN_PLAYING_OVER = 'Plugin for playing chess over QIP Infium';                          
  PLUGIN_INFO_NAME = 'Chess4Net 2009.1';
  PLUGIN_URL = 'http://chess4net.ru';
  PLUGIN_EMAIL = 'packpaul@mail.ru';

var
  QIPPluginsPath, Chess4NetPath: WideString;
  Chess4NetIniFilePath: WideString;
  Chess4NetGamesLogPath: WideString;

  Chess4NetIcon, pluginIcon: TIcon;

implementation

uses
  SysUtils, Forms, Windows;


procedure InitGlobals(pluginFileName: WideString);
const
  ICON_16_FILE_NAME = 'Chess4Net_16.ico';
begin
  QIPPluginsPath := ExtractFilePath(pluginFileName);
  Chess4NetPath := QIPPluginsPath + 'Chess4Net\';
  if not DirectoryExists(Chess4NetPath) then
    CreateDir(Chess4NetPath);
  Chess4NetIniFilePath := Chess4NetPath;
  Chess4NetGamesLogPath := Chess4NetPath;

  Chess4NetIcon := TIcon.Create;
  Chess4NetIcon.Handle := LoadIcon(hInstance, 'MAINICON');
  pluginIcon := TIcon.Create;
//  pluginIcon.Handle := LoadIconW(hInstance, 'MAINICON_16'); // TODO: load icon from resource - it doesn't loads 16x16 icons for now
  if FileExists(QIPPluginsPath + ICON_16_FILE_NAME) then
    pluginIcon.LoadFromFile(QIPPluginsPath + ICON_16_FILE_NAME);
end;


procedure DeinitGlobals;
begin
  pluginIcon.Free;
  Chess4NetIcon.Free;
end;

end.

