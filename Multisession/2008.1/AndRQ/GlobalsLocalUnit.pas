unit GlobalsLocalUnit;

// Модуль глобальных переменных и констант версии для &RQ

interface

uses
  Graphics;

const
  CHESS4NET = 'Chess4Net';
  CHESS4NET_VERSION = 200801; // 2008.1
  CHESS4NET_TITLE = 'Chess4Net 2008.1 (http://chess4net.ru)';
  MSG_INVITATION = 'Wellcome to Chess4Net. If you don''t have it, please download it from http://chess4net.ru';
  DIALOG_CAPTION = CHESS4NET;

  PLUGIN_NAME = CHESS4NET;
  PLUGIN_VERSION = CHESS4NET_VERSION;
  PLUGIN_PLAYING_VIA = 'Plugin for playing chess via &&RQ';
  PLUGIN_INFO_NAME = 'Chess4Net 2008.1.0';
  PLUGIN_URL = 'http://chess4net.ru';
  PLUGIN_EMAIL = 'packpaul@mail.ru';

var
  AndRQPluginsPath, Chess4NetPath: string;
  Chess4NetIcon, pluginIcon: TIcon;
  AndRQVersion: integer;

implementation

uses
  SysUtils, Forms, Windows;

initialization
  AndRQPluginsPath := ExtractFileDir(Application.ExeName) + '\Plugins\';
  Chess4NetPath := AndRQPluginsPath + 'Chess4Net\';
  if not DirectoryExists(Chess4NetPath) then
    CreateDir(Chess4NetPath);
  Chess4NetIcon := TIcon.Create;
  Chess4NetIcon.Handle := LoadIcon(hInstance, 'MAINICON');
  pluginIcon := Chess4NetIcon;

finalization
  Chess4NetIcon.Free;

end.

