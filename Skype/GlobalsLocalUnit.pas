unit GlobalsLocalUnit;

interface

uses
  Graphics;

const
  CHESS4NET = 'Chess4Net';

  CHESS4NET_VERSION = 201001; // 2010.1
  CHESS4NET_TITLE = 'Chess4Net 2010.1 [Cveti] (http://chess4net.ru)';
  MSG_INVITATION = 'Wellcome to Chess4Net. If you don''t have it, please download it from http://chess4net.ru';

  DIALOG_CAPTION = CHESS4NET;

  PLUGIN_PLAYING_OVER = 'Plugin for playing chess over Skype';

  PLUGIN_INFO_NAME = 'Chess4Net 2010.1.2 [Cveti]';
  PLUGIN_URL = 'http://chess4net.ru';
  CVETI_URL = 'http://cvetichess.ru';
  PLUGIN_EMAIL = 'packpaul@mail.ru';

  SKYPE_APP_NAME = 'Chess4Net_Skype';

var
  Chess4NetPath: string;
  Chess4NetIcon: TIcon;  

implementation

uses
  Forms, SysUtils;

initialization
  Chess4NetIcon := Application.Icon;
  Chess4NetPath := ExtractFilePath(Application.ExeName);

end.
