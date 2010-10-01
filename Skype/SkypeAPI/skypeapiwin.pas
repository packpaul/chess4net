unit SkypeAPIWin;

interface

uses
  Windows, Classes, Messages,
  {$IFDEF FPC}
    ClassesWin,
  {$ENDIF}
  SkypeAPI;

type
  { TSkypeAPIWin }

  TSkypeAPIWin = class(TSkypeAPI)
  private
    FHandle: HWND;
    FMsgAttach: LongWord;
    FMsgDiscover: LongWord;
    FSkypeWindowHandle: HWND;

    procedure FProcessMessages(var Msg : TMessage);

  protected
    constructor RCreate; override;
    procedure RSendCommand(const Command: UTF8String); override;

  public
    destructor Destroy; override;

    procedure Attach; override;
  end;

implementation

{ TSkypeAPIWin }

constructor TSkypeAPIWin.RCreate;
begin
  inherited RCreate;

  // Here we will get back message type IDs
  FMsgAttach := RegisterWindowMessage('SkypeControlAPIAttach');
  FMsgDiscover := RegisterWindowMessage('SkypeControlAPIDiscover');

  // FProcessMessage will handle incoming messages from Skype client
  FHandle := AllocateHWnd(FProcessMessages);
end;


destructor TSkypeAPIWin.Destroy;
begin
  if (FHandle <> 0) then
    DeallocateHWnd(FHandle);
  inherited;
end;

//------------------------------------------------------------------------------
// Here we handle finalising the handshake and later, incoming notification
// messages from Skype.

procedure TSkypeAPIWin.FProcessMessages(var Msg: TMessage);
begin
  Msg.Result := 1;

  // MsgAttach sort of message. This we will get as resoponse from Skype
  // to MsgDiscover broadcast. WParam will contain Skype API window handle
  // (which is not necessarily Skype's UI main window handle). That handle
  // we can use to send further messages directly to Skype, instead of
  // broadcasting. LParam will contain current handshake status code.
  if (Msg.Msg = FMsgAttach) Then
  begin
    FSkypeWindowHandle := Msg.WParam;

    case Msg.LParam  of
      0: RDoAttachmentStatus(asAttachSuccess);
      1: RDoAttachmentStatus(asAttachPendingAuthorization);
      2: RDoAttachmentStatus(asAttachRefused);
      3: RDoAttachmentStatus(asAttachNotAvailable);
    end;

    exit;
  end;

  // Here we have recieved a notification message from Skype.
  if ((Msg.Msg = WM_COPYDATA) and (HWND(Msg.WParam) = FSkypeWindowHandle)) then
  begin
    // The LParam contains a pointer to a TCopyDataStruct record.
    // lpData field of that record conatins pointer to a null-terminated string.
    // Through typecasting, we will pass that string to our RecvCommand procedure,
    // where further processing can take place.
    RDoCommandReceived(PChar(PCopyDataStruct(Msg.LParam).lpData));
    exit;
  end;

  Msg.Result := 0;
end;


procedure TSkypeAPIWin.Attach;
begin
  // Broadcasting all over the system that this application wants to
  // attach itself to Skype public API. Response from Skype will be
  // handled in ProcessMessages.
  PostMessage(HWND_BROADCAST, FMsgDiscover, FHandle, 0);
end;

//------------------------------------------------------------------------------
// Here we use Windows SendMessage to transmit API commands to Skype client.
// LParam contains pointer to CopyDataSruct record.
// cbData field of that record contains message length (+1 here because of
// the termianting null character). lpData field contains pointer to
// null-terminated string.

procedure TSkypeAPIWin.RSendCommand(const Command: UTF8String);
var
  CopyData: TCopyDataStruct;
begin
  if (Length(Command) = 0) then
    exit;
  if (FSkypeWindowHandle = 0) then
    exit;

  CopyData.dwData := 0;
  CopyData.cbData := Length(Command) + 1;
  CopyData.lpData := @(Command[1]);

  SendMessage(FSkypeWindowHandle, WM_COPYDATA, FHandle, LPARAM(@CopyData));
end;

end.

