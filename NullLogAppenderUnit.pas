////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit NullLogAppenderUnit;

interface

uses
  LoggerUnit;

type
  TNullLogAppender = class(TInterfacedObject, ILogAppender)
  public
    procedure DoAppend(const Level: TLogLevel; const strMsg: string);
  end;

implementation

////////////////////////////////////////////////////////////////////////////////
// TNullLogAppender

procedure TNullLogAppender.DoAppend(const Level: TLogLevel; const strMsg: string);
begin
  ;
end;

end.
