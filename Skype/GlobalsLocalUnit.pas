unit GlobalsLocalUnit;

interface

uses
  Graphics;

const
  CHESS4NET = 'Chess4Net';

  CHESS4NET_VERSION = 201001; // 2010.1
  CHESS4NET_TITLE = 'Chess4Net 2010.1 [Skype] (http://chess4net.ru)';
  MSG_INVITATION = 'Wellcome to Chess4Net. If you don''t have it, please download it from http://chess4net.ru';

  DIALOG_CAPTION = CHESS4NET;

  PLUGIN_PLAYING_VIA = 'Plugin for playing chess via Skype';

  PLUGIN_INFO_NAME = 'Chess4Net 2009.1.0';
  PLUGIN_URL = 'http://chess4net.ru';
  PLUGIN_EMAIL = 'packpaul@mail.ru';


var
  Chess4NetPath: string;
  Chess4NetIcon: TIcon;  

implementation

uses
  Forms;

initialization
  Chess4NetIcon := Application.Icon;

end.
