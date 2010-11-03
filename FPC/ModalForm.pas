unit ModalForm;

interface

uses
  Forms, Dialogs, Classes, LCLType;

type
  TModalForm = class;
  TModalFormClass = class of TModalForm;

  TModalFormID = (mfNone, mfMsgClose, mfMsgLeave, mfMsgAbort, mfMsgResign,
                  mfMsgDraw, mfMsgTakeBack, mfConnecting, mfGameOptions,
                  mfLookFeel, mfCanPause, mfContinue, mfIncompatible);

  TModalFormHandler = procedure(modSender: TModalForm; modID: TModalFormID) of object;

  TDialogs = class
  private
    IDCount: array[TModalFormID] of word;
    frmList: TList;
    function GetShowing: boolean;
  protected
    Handler: TModalFormHandler;
  public
    Owner: TForm;
// -5  procedure MessageDlg(const Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; msgDlgID: TModalFormID);
    function CreateDialog(modalFormClass: TModalFormClass): TModalForm;
    procedure SetShowing(msgDlgID: TModalFormID);
    procedure UnsetShowing(msgDlgID: TModalFormID; msgDlg: TModalForm = nil);
    function InFormList(frm: TForm): boolean;
    procedure BringToFront;
    procedure MoveForms(dx, dy: integer);
    constructor Create(aOwner: TForm; aHandler: TModalFormHandler);
    destructor Destroy; override;
    property Showing: boolean read GetShowing;
  end;

  TModalForm = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var aAction: TCloseAction);
    procedure ButtonClick(Sender: TObject);
  private
    GenFormClose: TCloseEvent;
  protected
    Handler: TModalFormhandler;
    dlgOwner: TDialogs;
    function GetHandle: hWnd; virtual;
    function GetEnabled_: boolean; virtual;
    procedure SetEnabled_(flag: boolean); virtual;
    function GetLeft_: integer; virtual;
    procedure SetLeft_(x: integer); virtual;
    function GetTop_: integer; virtual;
    procedure SetTop_(y: integer); virtual;
  public
    procedure Show; virtual;
    constructor Create(aOwner: TForm; modID: TModalFormID = mfNone; modHandler: TModalFormHandler = nil); reintroduce; overload;
    constructor Create(aDlgOwner: TDialogs; modID: TModalFormID; modHandler: TModalFormHandler); reintroduce; overload;
    class function GetModalID : TModalFormID; virtual;

    property Handle: hWnd read GetHandle;
    property Enabled: boolean read GetEnabled_ write SetEnabled_;
    property Left: integer read GetLeft_ write SetLeft_;
    property Top: integer read GetTop_ write SetTop_;
  end;

implementation

uses
  SysUtils, StdCtrls, Controls, Buttons,
  //
  GlobalsUnit;


procedure TModalForm.FormCreate(Sender: TObject);
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


procedure TModalForm.FormClose(Sender: TObject; var aAction: TCloseAction);
begin
  if Assigned(GenFormClose) then
    GenFormClose(Sender, aAction);
  if Assigned(dlgOwner) then
    dlgOwner.UnsetShowing(GetModalID, self);
  if fsModal in FormState then
    exit;
  if Assigned(Handler) then
    Handler(self, GetModalID);
  aAction := caFree;
end;


procedure TModalForm.ButtonClick(Sender: TObject);
begin
  if fsModal in FormState then
    exit;  
  Close;
end;


constructor TModalForm.Create(aOwner: TForm; modID: TModalFormID; modHandler: TModalFormHandler);
var
  i: integer;
begin
  FormStyle := aOwner.FormStyle;

  inherited Create(aOwner);
  FormCreate(self);

  GenFormClose := OnClose;
  OnClose := FormClose;

  Handler := modhandler;

  for i := 0 to (ComponentCount - 1) do
  begin
    if (Components[i] is TButton) then
      (Components[i] as TButton).OnClick := ButtonClick;
  end;
end;

constructor TModalForm.Create(aDlgOwner: TDialogs; modID: TModalFormID; modHandler: TModalFormHandler);
begin
  dlgOwner := aDlgOwner;
  Create(dlgOwner.Owner, modID, modHandler);
  dlgOwner.SetShowing(modID);
end;


class function TModalForm.GetModalID : TModalFormID;
begin
  Result := mfNone;
end;


function TModalForm.GetHandle: hWnd;
begin
  Result := inherited Handle;
end;


function TModalForm.GetEnabled_: boolean;
begin
  Result := inherited Enabled;
end;


procedure TModalForm.SetEnabled_(flag: boolean);
begin
  inherited Enabled := flag;
end;


procedure TModalForm.Show;
begin
  inherited Show;
end;


function TModalForm.GetLeft_: integer;
begin
  Result := inherited Left;
end;


procedure TModalForm.SetLeft_(x: integer);
begin
  inherited Left := x;
end;


function TModalForm.GetTop_: integer;
begin
  Result := inherited Top;
end;


procedure TModalForm.SetTop_(y: integer);
begin
  inherited Top := y;
end;

{-------------------------- TDialogs ------------------------------}

function TDialogs.GetShowing: boolean;
var
  i: TModalFormID;
begin
  Result := TRUE;
  for i := Low(TModalFormID) to High(TModalFormID) do
    begin
      if IDCount[i] > 0 then
        exit;
    end;
  Result := FALSE;
end;


procedure TDialogs.UnsetShowing(msgDlgID: TModalFormID; msgDlg: TModalForm = nil);
var
  i: integer;
begin
  dec(IDCount[msgDlgID]);

  if Assigned(msgDlg) then
    begin
      for i := 0 to frmList.Count - 1 do
        begin
          if TModalForm(frmList[i]).Handle = msgDlg.Handle then
            begin
              frmList.Delete(i);
              break;
            end;
        end; { for }
    end;
  if frmList.Count > 0 then
    begin
      TModalForm(frmList.Last).Enabled := TRUE;
      TModalForm(frmList.Last).SetFocus;
    end
  else
    begin
      TForm(Owner).Enabled := TRUE;
      TForm(Owner).SetFocus;
    end;
end;


function TDialogs.InFormList(frm: TForm): boolean;
var
  i: integer;
begin
  for i := 0 to frmList.Count - 1 do
    begin
      if TModalForm(frmList[i]).Handle = frm.Handle then
        begin
          Result := TRUE;
          exit;
        end;
    end;
  Result := FALSE;
end;

{ // -5
procedure TDialogs.MessageDlg(const Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; msgDlgID: TModalFormID);
var
  DialogForm: TDialogForm;
begin
  if (msgDlgID <> mfNone) and (IDCount[msgDlgID] > 0) then
    exit;
  DialogForm := TDialogForm.Create(self, Msg, DlgType, Buttons, msgDlgID, Handler);
  DialogForm.Caption := DIALOG_CAPTION;
  SetShowing(msgDlgID);
  DialogForm.Show;
  frmList.Add(DialogForm);
end;
}

function TDialogs.CreateDialog(modalFormClass: TModalFormClass): TModalForm;
begin
  Result := modalFormClass.Create(self, modalFormClass.GetModalID, Handler);
  frmList.Add(Result);
end;


constructor TDialogs.Create(aOwner: TForm; aHandler: TModalFormHandler);
var
  i: TModalFormID;
begin
  Owner := aOwner;
  Handler := aHandler;
  frmList := TList.Create;
  for i := Low(TModalFormID) to High(TModalFormID) do
    IDCount[i] := 0;
end;


destructor TDialogs.Destroy;
begin
  frmList.Free;
  inherited;
end;


procedure TDialogs.SetShowing(msgDlgID: TModalFormID);
begin
  inc(IDCount[msgDlgID]);
  if frmList.Count > 0 then
    TModalForm(frmList.Last).Enabled := FALSE;
end;


procedure TDialogs.BringToFront;
var
  i: integer;
begin
  if frmList.Count = 0 then
    exit;
  for i := 0 to frmList.Count - 1 do
    TModalForm(frmList[i]).Show;
  TModalForm(frmList.Last).SetFocus;
end;


procedure TDialogs.MoveForms(dx, dy: integer);
var
  i: integer;
begin
  for i := 0 to frmList.Count - 1 do
    with TModalForm(frmList[i]) do
      begin
        Left := Left + dx;
        Top := Top + dy;
      end;
end;

end.

