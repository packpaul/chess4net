////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit GlobalsLocalUnit;

interface

uses
  Graphics;

const
  CHESS4NET = 'Chess4Net';

  CHESS4NET_VERSION = 201301; // 2013.1
  CHESS4NET_TITLE = 'Chess4Net 2013.1b [Skype] (http://chess4net.ru)';
  MSG_INVITATION = 'Wellcome to Chess4Net. If you don''t have it, please download it from http://chess4net.ru';

  DIALOG_CAPTION = CHESS4NET;

  PLUGIN_PLAYING_OVER = 'Plugin for playing chess over Skype';

  PLUGIN_INFO_NAME = 'Chess4Net 2013.1b';
  PLUGIN_URL = 'http://chess4net.ru';
  PLUGIN_EMAIL = 'packpaul@mail.ru';

  SKYPE_APP_NAME = 'Chess4Net_Skype';

var
  Chess4NetIcon: TIcon;

procedure CreateLinkForGameLogFile;

implementation

uses
  Forms,
  //
  EnvironmentSetterUnit;

procedure CreateLinkForGameLogFile;
begin
  TEnvironmentSetter.CreateLinkForGameLogFile
end;

initialization
  Chess4NetIcon := Application.Icon;
  TEnvironmentSetter.SetEnvironment;

end.
