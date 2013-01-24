////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit GlobalsUnit;

interface

const
  CHESS4NET_VERSION = 201301; // 2013.1
  CHESS4NET_VERSION_TXT = '2013.1';
  COPYRIGHT_TXT = #169' 2007-2013 no rights reserved';

  CHESS4NET_URL = 'http://chess4net.ru';
  EMAIL_ADRESS = 'packpaul@mail.ru';

var
  Chess4NetPath: string;
  Chess4NetIniFilePath: string;
  Chess4NetUserDataPath: string;

implementation

uses
  EnvironmentSetterUnit;

end.
