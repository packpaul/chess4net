unit DialogUnit;

interface

uses
  Forms, Dialogs, Controls, Classes, Windows,
  ModalForm;

type
  TDialogForm = class(TModalForm)
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonClick(Sender: TObject);
  private
    modID: TModalFormID;
    msgDlg: TForm;
    function FGetCaption: TCaption;
    procedure FSetCaption(capt: TCaption);
  protected
    function GetHandle: hWnd; override;
    function GetEnabled_: boolean; override;
    procedure SetEnabled_(flag: boolean); override;
    function GetLeft_: integer; override;
    procedure SetLeft_(x: integer); override;
    function GetTop_: integer; override;
    procedure SetTop_(y: integer); override;
    function RGetVisible: boolean; override;
    procedure RSetVisible(bValue: boolean); override;
  public
    procedure Show; override;
    procedure BringToFront; override;
    function ShowModal: integer; override;
    procedure SetFocus; override;
    constructor Create(frmOwner: TForm; const Msg: string;
           DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; modID: TModalFormID = mfNone;
           msgDlgHandler: TModalFormHandler = nil); overload;
    constructor Create(dlgOwner: TDialogs; const Msg: string;
           DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; modID: TModalFormID;
            msgDlgHandler: TModalFormHandler); overload;
    destructor Destroy; reintroduce;
    property Caption: TCaption read FGetCaption write FSetCaption;
    property Visible: boolean read RGetVisible write RSetVisible;
  end;

implementation

uses
  StdCtrls, SysUtils, MessageDialogUnit;

////////////////////////////////////////////////////////////////////////////////
// TDialogForm

constructor TDialogForm.Create(frmOwner: TForm; const Msg: string;
                   DlgType: TMsgDlgType; Buttons: TMsgDlgButtons;
                   modID: TModalFormID; msgDlgHandler: TModalFormHandler);
var
  i: integer;
begin
  inherited CreateNew(frmOwner);

  self.modID := modID;
  Handler := msgDlgHandler;

  msgDlg := MessageDialogUnit.CreateMessageDialog(frmOwner, Msg, DlgType, Buttons);
  // msgDlg.FormStyle := frmOwner.FormStyle;
  msgDlg.OnShow := FormShow;
  msgDlg.OnClose := FormClose;

  for i := 0 to (msgDlg.ComponentCount - 1) do
    begin
      if (msgDlg.Components[i] is TButton) then
         (msgDlg.Components[i] as TButton).OnClick := ButtonClick;
    end;
end;

constructor TDialogForm.Create(dlgOwner: TDialogs; const Msg: string;
                DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; modID: TModalFormID;
                msgDlgHandler: TModalFormHandler);
begin
  Create((dlgOwner.Owner as TForm), Msg, DlgType, Buttons, modID, msgDlgHandler);
  self.dlgOwner := dlgOwner;
end;



procedure TDialogForm.FormShow(Sender: TObject);
var
  frmOwner: TForm;
begin
  frmOwner := (Owner as TForm);
  msgDlg.Left := frmOwner.Left + (frmOwner.Width - msgDlg.Width) div 2;
  msgDlg.Top := frmOwner.Top + (frmOwner.Height - msgDlg.Height) div 2;
end;


procedure TDialogForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(dlgOwner) then
    dlgOwner.UnsetShowing(modID, self);
  if fsModal in msgDlg.FormState then
    exit;
  if Assigned(Handler) then
    Handler(TModalForm(msgDlg), modID);
  Action := caFree;
end;

procedure TDialogForm.Show;
begin
  msgDlg.Show;
end;


procedure TDialogForm.BringToFront;
begin
  msgDlg.BringToFront;
end;


function TDialogForm.ShowModal: integer;
begin
  Result := msgDlg.ShowModal;
end;

procedure TDialogForm.ButtonClick(Sender: TObject);
begin
  if not (fsModal in msgDlg.FormState) then
    msgDlg.Close;
end;

destructor TDialogForm.Destroy;
begin
  msgDlg.Free;
end;

function TDialogForm.FGetCaption: TCaption;
begin
  Result := msgDlg.Caption;
end;

procedure TDialogForm.FSetCaption(capt: TCaption);
begin
  msgDlg.Caption := capt;
end;

function TDialogForm.GetHandle: hWnd;
begin
  Result := msgDlg.Handle;
end;


function TDialogForm.GetEnabled_: boolean;
begin
  Result := msgDlg.Enabled;
end;


procedure TDialogForm.SetEnabled_(flag: boolean);
begin
  msgDlg.Enabled := flag;
end;


procedure TDialogForm.SetFocus;
begin
  msgDlg.SetFocus;
  msgDlg.Show;
end;


function TDialogForm.GetLeft_: integer;
begin
  Result := msgDlg.Left;
end;


procedure TDialogForm.SetLeft_(x: integer);
begin
  msgDlg.Left := x;
end;


function TDialogForm.GetTop_: integer;
begin
  Result := msgDlg.Top;
end;


procedure TDialogForm.SetTop_(y: integer);
begin
  msgDlg.Top := y;
end;


function TDialogForm.RGetVisible: boolean;
begin
  Result := msgDlg.Visible;
end;



procedure TDialogForm.RSetVisible(bValue: boolean);
begin
  msgDlg.Visible := bValue;
end;


end.
