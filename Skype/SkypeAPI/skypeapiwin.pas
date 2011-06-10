////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit SkypeAPIWin;

interface

uses
  Windows, Classes, Messages, SysUtils,
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
    FReceivedCommandsQueue: TStringList;
    FThread: TThread;

    procedure FProcessMessages(var Msg : TMessage);
    procedure FProcessPendingCommands;

  protected
    constructor RCreate; override;
    procedure RSendCommand(const Command: UTF8String); override;

  public
    destructor Destroy; override;

    procedure Attach; override;
  end;

implementation

type
  TSkypeAPIWinThread = class(TThread)
  private
    FSkypeAPIWin: TSkypeAPIWin;
  protected
    procedure Execute; override;
  public
    constructor Create(const ASkypeAPIWin: TSkypeAPIWin);
  end;

{ TSkypeAPIWin }

constructor TSkypeAPIWin.RCreate;
begin
  inherited RCreate;

  // Here we will get back message type IDs
  FMsgAttach := RegisterWindowMessage('SkypeControlAPIAttach');
  FMsgDiscover := RegisterWindowMessage('SkypeControlAPIDiscover');

  // FProcessMessage will handle incoming messages from Skype client
  FHandle := AllocateHWnd(FProcessMessages);

  FReceivedCommandsQueue := TStringList.Create;
  FThread := TSkypeAPIWinThread.Create(self);
end;


destructor TSkypeAPIWin.Destroy;
begin
  FThread.Terminate;
  FThread := nil;
  
  FreeAndNil(FReceivedCommandsQueue);
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
      1:
      begin
        Sleep(1); // In order to prevent Skype freezing on Windows
        RDoAttachmentStatus(asAttachPendingAuthorization);
      end;
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

    FReceivedCommandsQueue.Add(PChar(PCopyDataStruct(Msg.LParam).lpData));
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


procedure TSkypeAPIWin.FProcessPendingCommands;
var
  Command: UTF8String;
begin
  while (FReceivedCommandsQueue.Count > 0) do
  begin
    Command := FReceivedCommandsQueue[0];
    FReceivedCommandsQueue.Delete(0);
    RDoCommandReceived(Command); 
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TSkypeAPIWinThread

constructor TSkypeAPIWinThread.Create(const ASkypeAPIWin: TSkypeAPIWin);
begin
  FSkypeAPIWin := ASkypeAPIWin;
  inherited Create(TRUE);
  FreeOnTerminate := TRUE;
  Resume;
end;


procedure TSkypeAPIWinThread.Execute;
begin
  while (not Terminated) do
  begin
    while (FSkypeAPIWin.FReceivedCommandsQueue.Count > 0) do
    begin
      Synchronize(FSkypeAPIWin.FProcessPendingCommands);
    end;
    Sleep(1);
  end;
end;

end.

