unit GlobalsSocketUnit;

{$MODE Delphi}

interface

const
  CHESS4NET_VERSION = 200706; // 2007.6
{$IFDEF LCLwin32}
  CHESS4NET_TITLE = 'Chess4Net 2007.6 (http://chess4net.ru)';
{$ENDIF}
{$IFDEF LCLgtk2}
  CHESS4NET_TITLE = 'Chess4Net 2007.6';
{$ENDIF}
  DEFAULT_PORT = 5555;

var
  Chess4NetPath: string;

implementation

initialization
{$IFDEF WINDOWS}
//  Chess4NetPath := ExtractFileDir(Application.ExeName) + DirectorySeparator;
  Chess4NetPath := '.' + DirectorySeparator;
{$ELSE} {$IFDEF UNIX}
//  Chess4NetPath := '~/.Chess4Net/';
  Chess4NetPath := '.' + DirectorySeparator;
{$ELSE}
  Assert(FALSE);
{$ENDIF} {$ENDIF}

end.
