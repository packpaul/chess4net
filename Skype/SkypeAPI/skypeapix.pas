////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit SkypeAPIX;

interface

uses
  xlib, x, xatom,
  //
  Classes, ExtCtrls,
  //
  SkypeAPI;

type
  ESkypeAPIX = class(ESkypeAPI);

  { TSkypeAPIX }

  TSkypeAPIX = class(TSkypeAPI)
  private
    FDisplay: PDisplay;
    FSkypeWin: TWindow;
    FRootWin: TWindow;
    FSelfWin: TWindow;

    FAtomMsgBegin, FAtomMsg: TAtom;

    FAttaching: boolean;
    FEventThread: TThread;

    FFriendlyName: string;

    procedure FStartEventThread;
    procedure FStopEventThread(Wait: boolean = TRUE);

    function FGetSkypeWin: TWindow;

    procedure FStartAttaching;

    class function FSendMessage(w: TWindow; const msg: string; mask: LongWord;
      disp: PDisplay; atom1, atom2: TAtom; handle: TWindow): boolean;

    procedure FOnMessageReceived(const Msg: UTF8String);

  protected
    constructor RCreate; override;
    procedure RSendCommand(const Command: UTF8String); override;
  public
    destructor Destroy; override;
    procedure Attach; override;
    property FriendlyName: string read FFriendlyName write FFriendlyName;
  end;

implementation

uses
  ctypes,
  //
  SysUtils;

type

  { TSkypeAPIXEventThread }

  TSkypeAPIXEventThread = class(TThread)
  private
    FSkypeAPIX: TSkypeAPIX;
    FDataBuffer: UTF8String;

    FNewAttachmentStatus: TAttachmentStatus;

    procedure FProcessEvent(ev: PXEvent);
    procedure FProcessClientMessageEvent(const XClientEvent: TXClientMessageEvent);
    procedure FProcessPropertyEvent(const XPropEvent: TXPropertyEvent);

    procedure FDoMessageReceived;
    procedure FDoChangeAttachmentStatus;
  protected
    procedure Execute; override;
  public
    constructor Create(ASkypeAPIX: TSkypeAPIX);
    destructor Destroy; override;
  end;

var
  old_handler: TXErrorHandler = nil; // for intercepting X Server error codes from XSendEvent
  xerror: cuchar;

function xmerrhandler(dpy: PDisplay; err: PXErrorEvent): cint; cdecl;
begin
  dpy := dpy;
  xerror := err.error_code;
  Result := 0;
end;

procedure TrapErrors;
begin
  xerror := 0;
  old_handler := XSetErrorHandler(xmerrhandler);
end;

function UntrapErrors: boolean;
begin
  XSetErrorHandler(old_handler);
  Result := ((xerror <> BadValue) and (xerror <> BadWindow));
end;

const
  SKYPECONTROLAPI_MESSAGE = 'SKYPECONTROLAPI_MESSAGE';
  SKYPECONTROLAPI_MESSAGE_BEGIN = SKYPECONTROLAPI_MESSAGE + '_BEGIN';

{ TSkypeAPIX }

constructor TSkypeAPIX.RCreate;
begin
  inherited RCreate;

  FDisplay := XOpenDisplay(nil);
  if (not Assigned(FDisplay)) then
    raise ESkypeAPIX.Create('Could not open XDisplay');

  FRootWin := XDefaultRootWindow(FDisplay);
  FSelfWin := XCreateSimpleWindow(FDisplay, FRootWin,
    100, 100, 100, 100, 1, 0, 0);

  XSelectInput(FDisplay, FRootWin, PropertyChangeMask);

  FSkypeWin := FGetSkypeWin;

  FAtomMsgBegin := XInternAtom(FDisplay, SKYPECONTROLAPI_MESSAGE_BEGIN, FALSE);
  FAtomMsg := XInternAtom(FDisplay, SKYPECONTROLAPI_MESSAGE, FALSE);
end;


destructor TSkypeAPIX.Destroy;
begin
  FStopEventThread;

  if (Assigned(FDisplay)) then
  begin
    XDestroyWindow(FDisplay, FSelfWin);
    XCloseDisplay(FDisplay);
  end;

  inherited;
end;


procedure TSkypeAPIX.RSendCommand(const Command: UTF8String);
var
  Res: boolean;
begin
  Res := TSkypeAPIX.FSendMessage(FSkypeWin, Command, 0, FDisplay, FAtomMsgBegin, FAtomMsg,
    FSelfWin);
end;


class function TSkypeAPIX.FSendMessage(w: TWindow; const msg: string; mask: LongWord;
  disp: PDisplay; atom1, atom2: TAtom; handle: TWindow): boolean;
var
  e: TXEvent;
  len: integer;
  i: integer;
  pos: integer;
begin
  e.xclient._type := ClientMessage;
  e.xclient.message_type := atom1; // leading message
  e.xclient.display := disp;
  e.xclient.window := handle;
  e.xclient.format := 8;

  TrapErrors;
  try
    pos := 0;
    len := Length(msg);

    repeat
      i := 0;
      while ((i < 20) and (pos + i <= len)) do
      begin
        e.xclient.data.b[i] := cchar(msg[Succ(pos + i)]);
        inc(i);
      end;

      XSendEvent(disp, w, FALSE, mask, @e);

      e.xclient.message_type := atom2; // following message

      inc(pos, i);

    until (pos > len);

  finally
    Result := UntrapErrors;
  end;
end;


procedure TSkypeAPIX.Attach;
begin
  if (AttachmentStatus = asAttachSuccess) then
    exit;
  if (FAttaching) then
    exit;

  if (FSkypeWin = TWindow(0)) then
    FSkypeWin := FGetSkypeWin;

  if (FSkypeWin = TWindow(0)) then
  begin
    RDoAttachmentStatus(asAttachNotAvailable);
    exit;
  end;

  FStartEventThread;
  FStartAttaching;
end;


procedure TSkypeAPIX.FStartEventThread;
begin
  if (not Assigned(FEventThread)) then
    FEventThread := TSkypeAPIXEventThread.Create(self);
end;


procedure TSkypeAPIX.FStopEventThread(Wait: boolean = TRUE);
begin
  if (Assigned(FEventThread)) then
  begin
    FEventThread.Terminate;
    if (Wait) then
      ; //FEventThread.WaitFor;
    FEventThread := nil;
  end;
end;


procedure TSkypeAPIX.FStartAttaching;
begin
  if (FFriendlyName = '') then
    raise ESkypeAPIX.Create('Friendly name must be set!');

  FAttaching := TRUE;
  RSendCommand('NAME ' + FFriendlyName);
end;


procedure TSkypeAPIX.FOnMessageReceived(const Msg: UTF8String);

  procedure NProcessAttaching;
  var
    NewAttachmentStatus: TAttachmentStatus;
  begin
    if (Msg = 'OK') then
      NewAttachmentStatus := asAttachSuccess
    else
    begin
      FStopEventThread(FALSE);
      if (Msg = 'ERROR 68') then
        NewAttachmentStatus := asAttachRefused
      else
        NewAttachmentStatus := asAttachNotAvailable;
    end;
    RDoAttachmentStatus(NewAttachmentStatus);
    exit;
  end;


begin // TSkypeAPIX.FOnMessageReceived
  if (FAttaching) then
  begin
    FAttaching := FALSE;
    NProcessAttaching;
    exit;
  end;

  RDoCommandReceived(Msg);
end;


function TSkypeAPIX.FGetSkypeWin: TWindow;
var
  SkypeInst, TypeRet: TAtom;
  WinP: Pcuchar;
  BytesAfterRet, NitemsRet: culong;
  FormatRet: cint;
  Status: cint;
begin
  Result := TWindow(0);

  SkypeInst := XInternAtom(FDisplay, '_SKYPE_INSTANCE', TRUE);

  if (SkypeInst = 0) then
    exit;

  Status := XGetWindowProperty(FDisplay, FRootWin, SkypeInst, 0, 1,
    FALSE, XA_WINDOW, @TypeRet, @FormatRet, @NitemsRet, @BytesAfterRet, @WinP);

  if ((Status = Success) and (FormatRet = 32) and (NitemsRet = 1)) then
    Result := PWindow(WinP)^;
end;

{ TSkypeAPIXEventThread }

constructor TSkypeAPIXEventThread.Create(ASkypeAPIX: TSkypeAPIX);
begin
  inherited Create(TRUE);
  FreeOnTerminate := TRUE;

  FSkypeAPIX := ASkypeAPIX;

  Resume;
end;


destructor TSkypeAPIXEventThread.Destroy;
begin
  FSkypeAPIX.FEventThread := nil;
  inherited;
end;


procedure TSkypeAPIXEventThread.Execute;
var
  Event: TXEvent;
begin
  while (not Terminated) do
  begin
    while (XPending(FSkypeAPIX.FDisplay) > 0) do
    begin
      XNextEvent(FSkypeAPIX.FDisplay, @Event);
      FProcessEvent(@Event);
    end;
    Sleep(0);
  end;
end;


procedure TSkypeAPIXEventThread.FProcessEvent(ev: PXEvent);
begin
  // Events we get here are already prefiltered by the predicate function
  case ev._type of
    ClientMessage:
      FProcessClientMessageEvent(ev.xclient);
    PropertyNotify:
      FProcessPropertyEvent(ev.xproperty);
  end;
end;


procedure TSkypeAPIXEventThread.FProcessClientMessageEvent(const XClientEvent: TXClientMessageEvent);

  function NGetDataStr: string;
  var
    i: integer;
  begin
    Result := StringOfChar(' ', 20);
    i := 0;
    while ((i < 20) and (XClientEvent.data.b[i] <> 0)) do
    begin
      Result[Succ(i)] := char(XClientEvent.data.b[i]);
      inc(i);
    end;
    Result := LeftStr(Result, i);
  end;

var
  Data: UTF8String;
begin // TSkypeAPIXEventThread.FProcessClientMessageEvent
  if (XClientEvent.format <> 8) then
    exit;

  Data := NGetDataStr;

  if (XClientEvent.message_type = FSkypeAPIX.FAtomMsgBegin) then
    FDataBuffer := Data
  else if (XClientEvent.message_type = FSkypeAPIX.FAtomMsg) then
  begin
    if (FDataBuffer <> '') then
      FDataBuffer := FDataBuffer + Data
    else
      raise ESkypeAPIX.Create('Middle of Skype X11 message received with no beginning!');
  end
  else
    exit;

  if ((Length(Data) < 20) and (FDataBuffer <> '')) then // Last message fragment
  begin
    Synchronize(FDoMessageReceived);
    FDataBuffer := '';
  end;
end;


procedure TSkypeAPIXEventThread.FDoMessageReceived;
var
  Msg: UTF8String;
begin
  Msg := FDataBuffer;
  FSkypeAPIX.FOnMessageReceived(Msg);
end;


procedure TSkypeAPIXEventThread.FProcessPropertyEvent(const XPropEvent: TXPropertyEvent);
var
  NameP: PChar;
  IsInstance: boolean;
begin
  NameP := XGetAtomName(FSkypeAPIX.FDisplay, XPropEvent.atom);
  IsInstance := (string(NameP) = '_SKYPE_INSTANCE');
  XFree(NameP);

  if (not IsInstance) then
    exit;

  if (XPropEvent.state = PropertyNewValue) then
  begin
    { changing attachment status can cause an event handler to be fired, in
      turn it could try to call Attach() and doing this immediately seems to
      confuse Skype (command '#0 NAME xxx' returns '#0 CONNSTATUS OFFLINE' :D);
      to fix this, we give Skype some time to initialize itself }
    Sleep(1000);
    FNewAttachmentStatus := asAttachAvailable;
    if (FNewAttachmentStatus <> FSkypeAPIX.AttachmentStatus) then
      Synchronize(FDoChangeAttachmentStatus);
  end
  else
  begin
    FNewAttachmentStatus := asAttachNotAvailable;
    if (FNewAttachmentStatus <> FSkypeAPIX.AttachmentStatus) then
      Synchronize(FDoChangeAttachmentStatus);
  end;
end;


procedure TSkypeAPIXEventThread.FDoChangeAttachmentStatus;
begin
  case FNewAttachmentStatus of
    asAttachAvailable:
    begin
      FSkypeAPIX.FSkypeWin := FSkypeAPIX.FGetSkypeWin;
    end;
    asAttachNotAvailable:
    begin
      FSkypeAPIX.FSkypeWin := TWindow(0);
      Terminate;
    end;
  end;
  FSkypeAPIX.RDoAttachmentStatus(FNewAttachmentStatus);
end;

end.

