unit SkypeAPI_Command;

interface

uses
  Classes, SysUtils, ExtCtrls, Contnrs;

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

  EListener = class(Exception);

  TListenerProcessCommandResult = (lpcrFalse, lpcrTrue, lpcrPending);

  TListener = class(TCommandBase) // non-blocking command
  private
    m_bProcessingCommandFlag: boolean;
    m_lwLastAnalyzedCmdID: LongWord;
    function FProcessCommand(const wstrCommand: WideString; lwCmdID: LongWord): TListenerProcessCommandResult;
    procedure FProcessNotification;
  protected
    constructor RCreate; virtual;
    function RParseCommand(const wstrCommand: WideString): boolean; virtual; abstract;
    function RProcessCommand(const wstrCommand: WideString): boolean; virtual; abstract;
    procedure RDoNotify; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TListenerClass = class of TListener;

  TListenersManager = class
  private
    m_Listeners: TObjectList;
    m_NotifyTimer: TTimer;
    m_lwCmdCounter: LongWord;
    m_PendingCommands: TStringList;
    procedure FCreateNotifyTimer;
    procedure FDestroyNotifyTimer;
    procedure FOnNotifyTimerTimer(Sender: TObject);
    procedure FProcessPendingCommands;
  public
    constructor Create;
    destructor Destroy; override;
    function CreateListener(AListenerClass: TListenerClass): TListener;
    procedure DestroyListener(var AListener: TListener);
    procedure ProcessCommand(const wstrCommand: WideString);
  end;

implementation

uses
  SkypeAPI_Skype;

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
  raise EListener.Create('This object cannot be instantiated directly!');
end;


constructor TListener.RCreate;
begin
  inherited Create;
end;


destructor TListener.Destroy;
begin
  inherited;
end;


function TListener.FProcessCommand(const wstrCommand: WideString; lwCmdID: LongWord): TListenerProcessCommandResult;
var
  bRes: boolean;
begin
  Result := lpcrFalse;

  if (lwCmdID <= m_lwLastAnalyzedCmdID) then
    exit;

  if (m_bProcessingCommandFlag) then
  begin
    bRes := RParseCommand(wstrCommand);
    if (bRes) then
      Result := lpcrPending
    else
    begin
      if (m_lwLastAnalyzedCmdID = (lwCmdID - 1)) then
        m_lwLastAnalyzedCmdID := lwCmdID;
    end;

    exit;
  end;

  m_bProcessingCommandFlag := TRUE;
  try
    bRes := RProcessCommand(wstrCommand);
    if (bRes) then
    begin
      Result := lpcrTrue;
//      TSkype.Instance.Log(Format('Listener command processing (%s), %d: %s',
//        [ClassName, lwCMDID, wstrCommand]))
    end
    else
      m_bProcessingCommandFlag := FALSE;

    m_lwLastAnalyzedCmdID := lwCmdID;

  except
    m_bProcessingCommandFlag := FALSE;
    raise;
  end;
  
end;


procedure TListener.FProcessNotification;
begin
  if (not m_bProcessingCommandFlag) then
    exit;

  RDoNotify;

  m_bProcessingCommandFlag := FALSE;

//  TSkype.Instance.Log(Format('Listener command processed (%s), <=%d',
//    [ClassName, m_lwLastAnalyzedCmdID]));
end;

////////////////////////////////////////////////////////////////////////////////
// TListenersManager

constructor TListenersManager.Create;
begin
  inherited Create;
  m_PendingCommands := TStringList.Create;
  m_Listeners := TObjectList.Create;
  FCreateNotifyTimer;
end;


destructor TListenersManager.Destroy;
begin
  FDestroyNotifyTimer;
  m_Listeners.Free;
  m_PendingCommands.Free;
  inherited;
end;


function TListenersManager.CreateListener(AListenerClass: TListenerClass): TListener;
begin
  Result := AListenerClass.RCreate;
  m_Listeners.Add(Result);
end;


procedure TListenersManager.DestroyListener(var AListener: TListener);
begin
  if (m_Listeners.Remove(AListener) >= 0) then
    AListener := nil;
end;


procedure TListenersManager.ProcessCommand(const wstrCommand: WideString);
var
  Listener: TListener;
  i: integer;
  Res: TListenerProcessCommandResult;
  bAddToPending: boolean;
begin
  inc(m_lwCmdCounter);

  bAddToPending := FALSE;  

  for i := 0 to m_Listeners.Count - 1 do
  begin
    Listener := m_Listeners[i] as TListener;
    Res := Listener.FProcessCommand(wstrCommand, m_lwCmdCounter);
    if (Res in [lpcrTrue, lpcrPending]) then
      m_NotifyTimer.Enabled := TRUE;
    if (Res = lpcrPending) then
      bAddToPending := TRUE;
  end;

  if (bAddToPending) then
    m_PendingCommands.AddObject(UTF8Encode(wstrCommand), Pointer(m_lwCmdCounter));
end;


procedure TListenersManager.FOnNotifyTimerTimer(Sender: TObject);
var
  i: integer;
  Listener: TListener;
begin
  m_NotifyTimer.Enabled := FALSE;

  for i := 0 to m_Listeners.Count - 1 do
  begin
    Listener := m_Listeners[i] as TListener;
    Listener.FProcessNotification;
  end;

  FProcessPendingCommands;

  if (m_PendingCommands.Count > 0) then
    m_NotifyTimer.Enabled := TRUE;
end;


procedure TListenersManager.FProcessPendingCommands;

  procedure NProcess;
  var
    i, j: integer;
    wstrCommand: WideString;
    lwCmdID: LongWord;
    Listener: TListener;
    bDeleteCommand: boolean;
  begin
    i := 0;
    while (i < m_PendingCommands.Count) do
    begin
      wstrCommand := UTF8Decode(m_PendingCommands[0]);
      lwCmdID := LongWord(m_PendingCommands.Objects[0]);

      bDeleteCommand := TRUE;
      for j := 0 to m_Listeners.Count - 1 do
      begin
        Listener := m_Listeners[j] as TListener;
        if (Listener.FProcessCommand(wstrCommand, lwCmdID) = lpcrPending) then
          bDeleteCommand := FALSE;
      end;

      if (bDeleteCommand) then
      begin
        m_PendingCommands.Delete(i);
        continue;
      end;

      inc(i);
    end;

  end;

const
  IN_METHOD: boolean = FALSE;
begin // TListenersManager.FProcessPendingCommands
  if (IN_METHOD) then
    exit;

  IN_METHOD := TRUE;
  try
    NProcess;
  finally
    IN_METHOD := FALSE;
  end;

end;


procedure TListenersManager.FDestroyNotifyTimer;
begin
  FreeAndNil(m_NotifyTimer);
end;


procedure TListenersManager.FCreateNotifyTimer;
begin
  m_NotifyTimer := TTimer.Create(nil);
  m_NotifyTimer.Enabled := FALSE;
  m_NotifyTimer.Interval := 1;
  m_NotifyTimer.OnTimer := FOnNotifyTimerTimer;
end;

end.
