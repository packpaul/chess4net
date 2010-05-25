unit ModalForm;

interface

uses
  Forms, TntForms, Dialogs, Classes, Windows;

type
  TModalForm = class;
  TModalFormClass = class of TModalForm;

  TModalFormID = (mfNone, mfMsgClose, mfMsgLeave, mfMsgAbort, mfMsgResign,
                  mfMsgDraw, mfMsgTakeBack, mfMsgAdjourn, mfConnecting, mfGameOptions,
                  mfLookFeel, mfCanPause, mfContinue, mfIncompatible
{$IFDEF SKYPE}
                  , mfSelectSkypeContact
{$ENDIF}
{$IFDEF MIRANDA}
                  , mfTransmitting, mfTransmitGame
{$ENDIF}

                  );

  TModalFormHandler = procedure(modSender: TModalForm; modID: TModalFormID) of object;

  TDialogs = class
  private
    IDCount: array[TModalFormID] of word;
    frmList: TList;
    function GetShowing: boolean;
  protected
    RHandler: TModalFormHandler;
  public
    Owner: TForm;
    constructor Create(Owner: TForm; Handler: TModalFormHandler);
    destructor Destroy; override;
    procedure MessageDlg(const wstrMsg: WideString; DlgType: TMsgDlgType;
      Buttons: TMsgDlgButtons; msgDlgID: TModalFormID);
    function CreateDialog(modalFormClass: TModalFormClass): TModalForm;
    procedure SetShowing(msgDlg: TModalForm);
    procedure UnsetShowing(msgDlg: TModalForm);
    function InFormList(frm: TForm): boolean;
    procedure BringToFront;
    procedure MoveForms(dx, dy: integer);
    procedure CloseNoneDialogs;
    property Showing: boolean read GetShowing;
  end;

  TModalForm = class(TTntForm)
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonClick(Sender: TObject);
  private
    GenFormShow: TNotifyEvent;
    GenFormClose: TCloseEvent;
  protected
    RHandler: TModalFormHandler;
    dlgOwner: TDialogs;
    constructor Create(Owner: TForm; modHandler: TModalFormHandler = nil); reintroduce; overload; virtual;
    constructor Create(dlgOwner: TDialogs; modHandler: TModalFormHandler); reintroduce; overload; virtual;
    function GetHandle: hWnd; virtual;
    function GetEnabled_: boolean; virtual;
    procedure SetEnabled_(flag: boolean); virtual;
    function GetLeft_: integer; virtual;
    procedure SetLeft_(x: integer); virtual;
    function GetTop_: integer; virtual;
    procedure SetTop_(y: integer); virtual;

    function GetModalID: TModalFormID; virtual;

  public
    procedure Show; virtual;

    property Handle: hWnd read GetHandle;
    property Enabled: boolean read GetEnabled_ write SetEnabled_;
    property Left: integer read GetLeft_ write SetLeft_;
    property Top: integer read GetTop_ write SetTop_;
  end;

implementation

uses
  SysUtils, StdCtrls, Controls,
  DialogUnit, GlobalsUnit;

procedure TModalForm.FormShow(Sender: TObject);
var
  frmOwner: TForm;
  iWidth, iHeight: integer;
  iLeft, iTop: integer;
begin
  if (Assigned(Owner)) then
  begin
    frmOwner := (Owner as TForm);
    iLeft := frmOwner.Left;
    iTop := frmOwner.Top;
    iWidth := frmOwner.Width;
    iHeight := frmOwner.Height;
  end
  else
  begin
    iLeft := 0;
    iTop := 0;
    iWidth := Screen.Width;
    iHeight := Screen.Height;
  end;
  Left := iLeft + (iWidth - self.Width) div 2;
  Top := iTop + (iHeight - self.Height) div 2;

  if (Assigned(GenFormShow)) then
    GenFormShow(Sender);
end;


procedure TModalForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(GenFormClose) then
    GenFormClose(Sender, Action);
  if Assigned(dlgOwner) then
    dlgOwner.UnsetShowing(self);
  if fsModal in FormState then
    exit;
  if (Assigned(RHandler)) then
    RHandler(self, GetModalID);
  Action := caFree;  
end;


procedure TModalForm.ButtonClick(Sender: TObject);
begin
  if (fsModal in FormState) then
    exit;  
  Close;
end;


constructor TModalForm.Create(Owner: TForm; modHandler: TModalFormHandler);
var
  i: integer;
begin
  if (Assigned(Owner)) then
    FormStyle := Owner.FormStyle;
  inherited Create(Owner);
  RHandler := modHandler;

  GenFormShow := OnShow;
  GenFormClose := OnClose;
  OnShow := FormShow;
  OnClose := FormClose;

  for i := 0 to (ComponentCount - 1) do
    begin
      if (Components[i] is TButton) then
        (Components[i] as TButton).OnClick := ButtonClick;
    end;
end;

constructor TModalForm.Create(dlgOwner: TDialogs; modHandler: TModalFormHandler);
begin
  self.dlgOwner := dlgOwner;
  Create(dlgOwner.Owner, modHandler);
  dlgOwner.SetShowing(self);
end;


function TModalForm.GetModalID : TModalFormID;
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


procedure TDialogs.UnsetShowing(msgDlg: TModalForm);
var
  i: integer;
begin
  dec(IDCount[msgDlg.GetModalID]);

  if (Assigned(msgDlg)) then
  begin
    for i := 0 to frmList.Count - 1 do
    begin
      if (TModalForm(frmList[i]).Handle = msgDlg.Handle) then
      begin
        frmList.Delete(i);
        break;
      end;
    end; // for
  end;

  if (frmList.Count > 0) then
  begin
    TModalForm(frmList.Last).Enabled := TRUE;
    TModalForm(frmList.Last).SetFocus;
  end
  else
  begin
    if (Assigned(Owner)) then
    begin
      Owner.Enabled := TRUE;
      Owner.SetFocus;
    end;
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


procedure TDialogs.MessageDlg(const wstrMsg: WideString; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; msgDlgID: TModalFormID);
var
  DialogForm: TDialogForm;
begin
  if ((msgDlgID <> mfNone) and (IDCount[msgDlgID] > 0)) then
    exit;
  DialogForm := TDialogForm.Create(self, wstrMsg, DlgType, Buttons, msgDlgID, RHandler);
  DialogForm.Caption := DIALOG_CAPTION;
  SetShowing(DialogForm);
  DialogForm.Show;
  frmList.Add(DialogForm);
end;


function TDialogs.CreateDialog(modalFormClass: TModalFormClass): TModalForm;
begin
  Result := modalFormClass.Create(self, RHandler);
  frmList.Add(Result);
end;


constructor TDialogs.Create(Owner: TForm; Handler: TModalFormHandler);
var
  i: TModalFormID;
begin
  self.Owner := Owner;
  self.RHandler := Handler;
  frmList := TList.Create;
  for i := Low(TModalFormID) to High(TModalFormID) do
    IDCount[i] := 0;
end;


destructor TDialogs.Destroy;
var
  i: integer;
  ModalForm: TModalForm;
begin
  for i := 0 to frmList.Count - 1 do
  begin
    ModalForm := frmList[i];
    ModalForm.RHandler := nil;
    ModalForm.Release;
  end;

  inherited;
end;


procedure TDialogs.SetShowing(msgDlg: TModalForm);
begin
  inc(IDCount[msgDlg.GetModalID]);
  if (frmList.Count > 0) then
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
  begin
    with TModalForm(frmList[i]) do
    begin
      Left := Left + dx;
      Top := Top + dy;
    end;
  end;
end;


procedure TDialogs.CloseNoneDialogs;
var
  i: integer;
  Dlg: TModalForm;
begin
  i := frmList.Count - 1;
  while (i >= 0) do
  begin
    Dlg := frmList[i];
    if (Dlg.GetModalID = mfNone) then
      UnsetShowing(Dlg);
    dec(i);
  end;
end;

end.

