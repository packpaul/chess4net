////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit SkypeAPI;

interface

uses
  SysUtils;

type
  ESkypeAPI = class(Exception);

  TAttachmentStatus = (
    asAttachUnknown = -1,
    asAttachSuccess = 0,
    asAttachPendingAuthorization = 1,
    asAttachRefused = 2,
    asAttachNotAvailable = 3,
    asAttachAvailable = $8001);

  TOnAttachmentStatus = procedure(ASender: TObject; Status: TAttachmentStatus) of object;

  TOnCommandReceived = procedure(ASender: TObject; const wstrCommand: WideString) of object;

  { TSkypeAPI }

  TSkypeAPI = class
  private
    FOnAttachmentStatus: TOnAttachmentStatus;
    FOnCommandReceived: TOnCommandReceived;
    FAttachmentStatus: TAttachmentStatus;
  protected
    constructor RCreate; virtual;

    procedure RDoAttachmentStatus(Status: TAttachmentStatus);
    procedure RDoCommandReceived(const Command: UTF8String);

    procedure RSendCommand(const Command: UTF8String); virtual; abstract;

  public
    class function Create(const AFriendlyName: string): TSkypeAPI;

    procedure Attach; virtual; abstract;
    procedure SendCommand(const wstrCommand: WideString);

    property AttachmentStatus: TAttachmentStatus read FAttachmentStatus;

    property OnAttachmentStatus: TOnAttachmentStatus read FOnAttachmentStatus write FOnAttachmentStatus;
    property OnCommandReceived: TOnCommandReceived read FOnCommandReceived write FOnCommandReceived;
  end;

implementation

{$IF DEFINED(WINDOWS) OR DEFINED(MSWINDOWS)}
  {$DEFINE WIN}
{$IFEND}
{$IF DEFINED(UNIX)}
  {$DEFINE X}
{$IFEND}

uses
{$IFDEF WIN}
  SkypeAPIWin
{$ENDIF}
{$IFDEF X}
  SkypeAPIX
{$ENDIF}
  ;

type
  TSkypeAPIClass = class of TSkypeAPI;

{ TSkypeAPI }

constructor TSkypeAPI.RCreate;
begin
  inherited Create;
  FAttachmentStatus := asAttachUnknown;
end;

class function TSkypeAPI.Create(const AFriendlyName: string): TSkypeAPI;
var
  SkypeAPIClass: TSkypeAPIClass;
begin
  Result := nil;

{$IFDEF WIN}
  SkypeAPIClass := TSkypeAPIWin;
  Result := SkypeAPIClass.RCreate;
{$ENDIF}
{$IFDEF X}
  SkypeAPIClass := TSkypeAPIX;
  Result := SkypeAPIClass.RCreate;
  TSkypeAPIX(Result).FriendlyName := AFriendlyName;
{$ENDIF}
end;


procedure TSkypeAPI.RDoAttachmentStatus(Status: TAttachmentStatus);
begin
  FAttachmentStatus := Status;
  if (Assigned(FOnAttachmentStatus)) then
    FOnAttachmentStatus(self, Status);
end;


procedure TSkypeAPI.RDoCommandReceived(const Command: UTF8String);
var
  wstrCommand: WideString;
begin
  if (Assigned(FOnCommandReceived)) then
  begin
    wstrCommand := UTF8Decode(Command);
    FOnCommandReceived(self, wstrCommand);
  end;
end;


procedure TSkypeAPI.SendCommand(const wstrCommand: WideString);
var
  UTF8Cmd: UTF8String;
begin
  if (AttachmentStatus <> asAttachSuccess) then
    exit;
  UTF8Cmd := UTF8Encode(wstrCommand);
  RSendCommand(UTF8Cmd);
end;

end.

