unit SkypeAPI_Command;

interface

uses
  ExtCtrls;

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
    m_NotifyTimer: TTimer;
    procedure FCreateNotifyTimer;
    procedure FDestroyNotifyTimer;
    procedure FOnNotifyTimerTimer(Sender: TObject);
  protected
    function RProcessCommand(const wstrCommand: WideString): boolean; virtual; abstract;
    procedure RDoNotify; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;
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
  wstrHead := UpperCase(RNextToken(wstrCommand, wstrBody));
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
    wstrTail := Copy(wstr, Succ(iPos), MaxInt);
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

constructor TListener.Create;
begin
  inherited Create;
  FCreateNotifyTimer;
end;


destructor TListener.Destroy;
begin
  FDestroyNotifyTimer;
  inherited;
end;


function TListener.ProcessCommand(const wstrCommand: WideString): boolean;
begin
  Result := FALSE;

  if (m_bProcessingCommandFlag) then
    exit;

  m_bProcessingCommandFlag := TRUE;
  try
    Result := RProcessCommand(wstrCommand);
    if (Result) then
      m_NotifyTimer.Enabled := TRUE
    else
      m_bProcessingCommandFlag := FALSE;
  except
    m_bProcessingCommandFlag := FALSE;
    raise;
  end;
end;


procedure TListener.FCreateNotifyTimer;
begin
  m_NotifyTimer := TTimer.Create(nil);
  m_NotifyTimer.Enabled := FALSE;
  m_NotifyTimer.Interval := 1;
  m_NotifyTimer.OnTimer := FOnNotifyTimerTimer;
end;


procedure TListener.FOnNotifyTimerTimer(Sender: TObject);
begin
  m_NotifyTimer.Enabled := FALSE;
  m_bProcessingCommandFlag := FALSE;
  RDoNotify;
end;


procedure TListener.FDestroyNotifyTimer;
begin
  FreeAndNil(m_NotifyTimer);
end;

end.
