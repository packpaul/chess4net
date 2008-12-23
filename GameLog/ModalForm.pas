unit ModalForm;

interface

uses
  Forms, Dialogs, Classes, Windows;

type
  TModalForm = class;
  TModalFormClass = class of TModalForm;

  TModalFormID = (mfNone, mfMsgClose, mfMsgLeave, mfMsgAbort, mfMsgResign,
                  mfMsgDraw, mfMsgTakeBack, mfMsgAdjourn, mfConnecting, mfGameOptions,
                  mfLookFeel, mfCanPause, mfContinue, mfIncompatible);

  TModalFormHandler = procedure(modSender: TModalForm; modID: TModalFormID) of object;

  TDialogs = class
  private
    IDCount: array[TModalFormID] of integer;
    frmList: TList;
    function FGetShowing: boolean;
  protected
    Handler: TModalFormHandler;
  public
    Owner: TForm;
    procedure MessageDlg(const Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; msgDlgID: TModalFormID);
    function CreateDialog(modalFormClass: TModalFormClass; bFreeOnClose: boolean = TRUE;
      bModal: boolean = TRUE): TModalForm;
    procedure SetShowing(msgDlgID: TModalFormID);
    procedure UnsetShowing(msgDlgID: TModalFormID; msgDlg: TModalForm = nil);
    function InFormList(frm: TForm): boolean;
    procedure BringToFront;
    procedure MoveForms(dx, dy: integer);
    constructor Create(Owner: TForm; Handler: TModalFormHandler = nil);
    destructor Destroy; override;
    property Showing: boolean read FGetShowing;
  end;

  TModalForm = class(TForm)
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonClick(Sender: TObject);
  private
    m_bFreeOnClose: boolean;
    m_bModal: boolean;
    GenFormShow: TNotifyEvent;
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
    function RGetVisible: boolean; virtual;
    procedure RSetVisible(bValue: boolean); virtual;
  public
    procedure Show; virtual;
    procedure BringToFront; virtual;
    constructor Create(Owner: TForm; modID: TModalFormID = mfNone;
      modHandler: TModalFormHandler = nil; bFreeOnClose: boolean = TRUE;
      bModal: boolean = TRUE); reintroduce; overload;
    constructor Create(dlgOwner: TDialogs; modID: TModalFormID;
      modHandler: TModalFormHandler; bFreeOnClose: boolean = TRUE;
      bModal: boolean = TRUE); reintroduce; overload;
    class function GetModalID : TModalFormID; virtual;

    property Handle: hWnd read GetHandle;
    property Enabled: boolean read GetEnabled_ write SetEnabled_;
    property Left: integer read GetLeft_ write SetLeft_;
    property Top: integer read GetTop_ write SetTop_;
    property Visible: boolean read RGetVisible write RSetVisible;
  end;

implementation

uses
  SysUtils, StdCtrls, Controls,
  DialogUnit, GlobalsUnit;

procedure TModalForm.FormShow(Sender: TObject);
var
  frmOwner: TForm;
begin
  frmOwner := (Owner as TForm);
  Left:= frmOwner.Left + (frmOwner.Width - Width) div 2;
  Top:= frmOwner.Top + (frmOwner.Height - Height) div 2;
  if Assigned(GenFormShow) then
    GenFormShow(Sender);
end;


procedure TModalForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(GenFormClose) then
    GenFormClose(Sender, Action);
  if (Assigned(dlgOwner) and m_bFreeOnClose) then
    dlgOwner.UnsetShowing(GetModalID, self);
  if (fsModal in FormState) then
    exit;
  if Assigned(Handler) then
    Handler(self, GetModalID);
  if (m_bFreeOnClose) then
    Action := caFree;
end;


procedure TModalForm.ButtonClick(Sender: TObject);
begin
  if fsModal in FormState then
    exit;  
  Close;
end;


constructor TModalForm.Create(Owner: TForm; modID: TModalFormID = mfNone;
  modHandler: TModalFormHandler = nil; bFreeOnClose: boolean = TRUE;
  bModal: boolean = TRUE);
var
  i: integer;
begin
  FormStyle := Owner.FormStyle;
  inherited Create(Owner);
  Handler := modhandler;
  m_bFreeOnClose := bFreeOnClose;
  m_bModal := bModal;

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


constructor TModalForm.Create(dlgOwner: TDialogs; modID: TModalFormID;
  modHandler: TModalFormHandler; bFreeOnClose: boolean = TRUE;
  bModal: boolean = TRUE);
begin
  self.dlgOwner := dlgOwner;
  Create(dlgOwner.Owner, modID, modHandler, bFreeOnClose, bModal);
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


procedure TModalForm.BringToFront;
begin
  inherited BringToFront;
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


function TModalForm.RGetVisible: boolean;
begin
  Result := inherited Visible;
end;


procedure TModalForm.RSetVisible(bValue: boolean);
begin
  inherited Visible := bValue;
end;

{-------------------------- TDialogs ------------------------------}

function TDialogs.FGetShowing: boolean;
var
  i: TModalFormID;
begin
  Result := TRUE;
  for i := Low(TModalFormID) to High(TModalFormID) do
    begin
      if (i = mfNone) then
        continue;
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


function TDialogs.CreateDialog(modalFormClass: TModalFormClass; bFreeOnClose: boolean = TRUE;
  bModal: boolean = TRUE): TModalForm;
begin
  Result := modalFormClass.Create(self, modalFormClass.GetModalID, Handler, bFreeOnClose, bModal);
  frmList.Add(Result);
end;


constructor TDialogs.Create(Owner: TForm; Handler: TModalFormHandler = nil);
var
  i: TModalFormID;
begin
  self.Owner := Owner;
  self.Handler := Handler;
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
    with TModalForm(frmList[i]) do
      if (Visible) then
        TModalForm(frmList[i]).BringToFront;
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

