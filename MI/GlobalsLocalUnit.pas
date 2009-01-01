unit GlobalsLocalUnit;

// Модуль для глобальных переменных и констант версии для Миранды

interface

uses
  Graphics;

const
  CHESS4NET = 'Chess4Net';
  CHESS4NET_VERSION = 200901; // 2009.1
  CHESS4NET_TITLE = 'Chess4Net 2009.1 (http://chess4net.ru)';
  MSG_INVITATION = 'Wellcome to Chess4Net. If you don''t have it, please download it from http://chess4net.ru';
  PROMPT_HEAD = 'Ch4N';
  MSG_DATA_SEPARATOR = '&&';

  PLUGIN_NAME = CHESS4NET;
  PLUGIN_VERSION = CHESS4NET_VERSION;
  PLUGIN_PLAYING_VIA = 'Plugin for playing chess via Miranda';
  PLUGIN_INFO_NAME = 'Chess4Net 2009.1.2';
  PLUGIN_URL = 'http://chess4net.ru';
  PLUGIN_EMAIL = 'packpaul@mail.ru';

var
  Chess4NetPath: string;
  Chess4NetIcon, pluginIcon: TIcon;

implementation

end.
