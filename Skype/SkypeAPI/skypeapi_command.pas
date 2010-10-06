unit SkypeAPI_Command;

interface

type
  TCommandBase = class
  protected
    class procedure RSplitCommandToHeadAndBody(const wstrCommand: WideString;
      var wstrHead, wstrBody: WideString);
    class function RNextToken(wstr: WideString; var wstrTail: WideString): WideString;
  end;
  

  TCommand = class(TCommandBase) // blocking command
  private
    m_bHasResponse: boolean;
  protected
    function RGetCommand: WideString; virtual; abstract;
    function RProcessResponse(const wstrCommand: WideString): boolean; virtual; abstract;
  public
    procedure ProcessResponse(const wstrCommand: WideString);
    property Command: WideString read RGetCommand;
    property HasResponse: boolean read m_bHasResponse;
  end;

  TListener = class(TCommandBase) // non-blocking command
  private
    m_bProcessingCommandFlag: boolean;
  protected
    function RProcessCommand(const wstrCommand: WideString): boolean; virtual; abstract;  
  public
    function ProcessCommand(const wstrCommand: WideString): boolean;
  end;

implementation

uses
  SysUtils;

////////////////////////////////////////////////////////////////////////////////
// TCommandBase

class procedure TCommandBase.RSplitCommandToHeadAndBody(const wstrCommand: WideString;
  var wstrHead, wstrBody: WideString);
begin
  wstrHead := UpperCase(TCommandBase.RNextToken(wstrCommand, wstrBody));
  wstrBody := TrimLeft(wstrBody);
end;


class function TCommandBase.RNextToken(wstr: WideString; var wstrTail: WideString): WideString;
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

////////////////////////////////////////////////////////////////////////////////
// TCommand

procedure TCommand.ProcessResponse(const wstrCommand: WideString);
begin
  m_bHasResponse := RProcessResponse(wstrCommand);
end;

////////////////////////////////////////////////////////////////////////////////
// TListener

function TListener.ProcessCommand(const wstrCommand: WideString): boolean;
begin
  Result := FALSE;

  if (m_bProcessingCommandFlag) then
    exit;

  m_bProcessingCommandFlag := TRUE;
  try
    Result := RProcessCommand(wstrCommand);
  finally
    m_bProcessingCommandFlag := FALSE;
  end;
end;

end.
