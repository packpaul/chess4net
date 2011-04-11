////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit ModalFormBase;

interface

uses
  Forms, Controls, LCLType;

type
  TModalFormBase = class;

  TModalFormID = (mfNone, mfMsgClose, mfMsgLeave, mfMsgAbort, mfMsgResign,
                  mfMsgDraw, mfMsgTakeBack, mfConnecting, mfGameOptions,
                  mfLookFeel, mfCanPause, mfContinue, mfIncompatible,
                  mfSelectSkypeContact, mfMsgAdjourn);

  TModalFormHandler = procedure(modSender: TModalFormBase; modID: TModalFormID) of object;

  TDialogsBase = class
  protected
    Owner: TForm;
    Handler: TModalFormHandler;
  public
    procedure SetShowing(msgDlgID: TModalFormID); virtual; abstract;
    procedure UnsetShowing(msgDlgID: TModalFormID; msgDlg: TModalFormBase = nil); virtual; abstract;
  end;

  TModalFormBase = class(TForm)
  private
    GenFormClose: TCloseEvent;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var aAction: TCloseAction);
  protected
    Handler: TModalFormHandler;
    dlgOwner: TDialogsBase;
    function GetHandle: hWnd; virtual; abstract;
    function GetEnabled_: boolean; virtual; abstract;
    procedure SetEnabled_(flag: boolean); virtual; abstract;
    function RGetModalResult: TModalResult; virtual; abstract;
    procedure RSetModalResult(Value: TModalResult); virtual; abstract;
    function GetLeft_: integer; virtual; abstract;
    procedure SetLeft_(x: integer); virtual; abstract;
    function GetTop_: integer; virtual; abstract;
    procedure SetTop_(y: integer); virtual; abstract;
    function RGetWidth: integer; virtual; abstract;
    procedure RSetWidth(iValue: integer); virtual; abstract;
    function RGetHeight: integer; virtual; abstract;
    procedure RSetHeight(iValue: integer); virtual; abstract;
    function RGetConstraints: TSizeConstraints; virtual; abstract;
    function RGetOnClose: TCloseEvent; virtual; abstract;
    procedure RSetOnClose(Value: TCloseEvent); virtual; abstract;
    function RGetFormState: TFormState; virtual; abstract;

  public
    constructor Create(aOwner: TForm; modHandler: TModalFormHandler = nil); virtual; overload; reintroduce;
    constructor Create(aDlgOwner: TDialogsBase); overload; reintroduce;

    procedure SetFocus; virtual; abstract;
    procedure Show; virtual; abstract;
    function GetModalID: TModalFormID; virtual; abstract;

    property Handle: hWnd read GetHandle;
    property Enabled: boolean read GetEnabled_ write SetEnabled_;
    property ModalResult: TModalResult read RGetModalResult write RSetModalResult;

    property Left: integer read GetLeft_ write SetLeft_;
    property Top: integer read GetTop_ write SetTop_;
    property Width: integer read RGetWidth write RSetWidth;
    property Height: integer read RGetHeight write RSetHeight;

    property Constraints: TSizeConstraints read RGetConstraints;

    property OnClose: TCloseEvent read RGetOnClose write RSetOnClose;
    property FormState: TFormState read RGetFormState;
  end;

implementation

////////////////////////////////////////////////////////////////////////////////
// TModalFormBase

constructor TModalFormBase.Create(aOwner: TForm; modHandler: TModalFormHandler = nil);
begin
  inherited Create(aOwner);
  FormCreate(self);

  GenFormClose := OnClose;
  OnClose := FormClose;

  Handler := modhandler;
end;


constructor TModalFormBase.Create(aDlgOwner: TDialogsBase);
begin
  dlgOwner := aDlgOwner;
  Create(dlgOwner.Owner, dlgOwner.Handler);
  dlgOwner.SetShowing(GetModalID);
end;


procedure TModalFormBase.FormCreate(Sender: TObject);
var
  frmOwner: TForm;
begin
{$IFDEF LCLgtk2}
  with Constraints do
  begin
    MinWidth := Width;
    MaxWidth := Width + 1;
    MinHeight := Height;
    MaxHeight := Height + 1;
  end;
{$ENDIF}
  frmOwner := (Owner as TForm);
  Left := frmOwner.Left + (frmOwner.Width - Width) div 2;
  Top := frmOwner.Top + (frmOwner.Height - Height) div 2;
end;


procedure TModalFormBase.FormClose(Sender: TObject; var aAction: TCloseAction);
begin
  if Assigned(GenFormClose) then
    GenFormClose(Sender, aAction);
  if Assigned(dlgOwner) then
    dlgOwner.UnsetShowing(GetModalID, self);
  if Assigned(Handler) then
    Handler(self, GetModalID);
  if (fsModal in FormState) then
  begin
    if (ModalResult = mrNone) then
      ModalResult := mrCancel;
  end;
  aAction := caFree;
end;

end.

