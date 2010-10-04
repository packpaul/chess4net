unit SkypeAPI_Command;

interface

type
  TCommand = class
    class function Protocol(iVer: integer): WideString;
  end;

implementation

uses
  SysUtils;

////////////////////////////////////////////////////////////////////////////////
// TCommand

class function TCommand.Protocol(iVer: integer): WideString;
begin
  Result := 'PROTOCOL ' + IntToStr(iVer);
end;

end.