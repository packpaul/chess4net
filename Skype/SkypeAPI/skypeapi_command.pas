unit SkypeAPI_Command;

interface

type
  TCommand = class
  private
    m_bHasResponse: boolean;
  protected
    function RGetCommand: WideString; virtual; abstract;
    function RProcessResponse(const wstrCommand: WideString): boolean; virtual; abstract;
    class procedure RSplitCommandToHeadAndBody(const wstrCommand: WideString;
      out wstrHead, wstrBody: WideString);
    class function RNextToken(wstr: WideString; out wstrTail: WideString): WideString;
  public
    procedure ProcessResponse(const wstrCommand: WideString);
    property Command: WideString read RGetCommand;
    property HasResponse: boolean read m_bHasResponse;
  end;

implementation

uses
  SysUtils;

////////////////////////////////////////////////////////////////////////////////
// TCommand

procedure TCommand.ProcessResponse(const wstrCommand: WideString);
begin
  m_bHasResponse := RProcessResponse(wstrCommand);
end;


class procedure TCommand.RSplitCommandToHeadAndBody(const wstrCommand: WideString;
  out wstrHead, wstrBody: WideString);
var
  iPos: integer;
begin
  wstrHead := UpperCase(TCommand.RNextToken(wstrCommand, wstrBody));
  wstrBody := TrimLeft(wstrBody);
end;


class function TCommand.RNextToken(wstr: WideString; out wstrTail: WideString): WideString;
var
  iPos: integer;
begin
  wstr := TrimLeft(wstr);

  iPos := Pos(' ', wstr);
  if (iPos > 0) then
  begin
    Result := Copy(wstr, 1, Pred(iPos));
    wstrTail := Copy(wstr, iPos, MaxInt);
  end
  else
  begin
    Result := wstr;
    wstrTail := '';
  end;
end;

end.
