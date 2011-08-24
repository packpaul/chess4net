////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit GlobalsLocalUnit;

// Модуль глобальных переменных и констант версии для &RQ

interface

uses
  Graphics;

const
  CHESS4NET = 'Chess4Net';
  CHESS4NET_VERSION = 200901; // 2009.1
  CHESS4NET_TITLE = 'Chess4Net 2009.1 (http://chess4net.ru)';
  MSG_INVITATION = 'Wellcome to Chess4Net. If you don''t have it, please download it from http://chess4net.ru';
  DIALOG_CAPTION = CHESS4NET;

  PLUGIN_NAME = CHESS4NET;
  PLUGIN_VERSION = CHESS4NET_VERSION;
  PLUGIN_PLAYING_OVER = 'Plugin for playing chess over &&RQ';
  PLUGIN_INFO_NAME = 'Chess4Net 2009.1.0';
  PLUGIN_URL = 'http://chess4net.ru';
  PLUGIN_EMAIL = 'packpaul@mail.ru';

var
  AndRQPluginsPath: string;
  Chess4NetIcon, pluginIcon: TIcon;
  AndRQVersion: integer;

implementation

uses
  SysUtils, Forms, Windows,
  //
  GlobalsUnit;

initialization
  AndRQPluginsPath := ExtractFileDir(Application.ExeName) + '\Plugins\';
  Chess4NetPath := AndRQPluginsPath + 'Chess4Net\';
  if (not DirectoryExists(Chess4NetPath)) then
    CreateDir(Chess4NetPath);
  Chess4NetIniFilePath := Chess4NetPath;
  Chess4NetGamesLogPath := Chess4NetPath;
  Chess4NetIcon := TIcon.Create;
  Chess4NetIcon.Handle := LoadIcon(hInstance, 'MAINICON');
  pluginIcon := Chess4NetIcon;

finalization
  Chess4NetIcon.Free;

end.

