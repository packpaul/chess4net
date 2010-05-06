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
    function GetCaption: TCaption;
    procedure SetCaption(capt: TCaption);
  protected
    function GetHandle: hWnd; override;
    function GetEnabled_: boolean; override;
    procedure SetEnabled_(flag: boolean); override;
    function GetLeft_: integer; override;
    procedure SetLeft_(x: integer); override;
    function GetTop_: integer; override;
    procedure SetTop_(y: integer); override;
  public
    procedure Show; override;
    function ShowModal: integer; reintroduce;
    procedure SetFocus; override;
    constructor Create(frmOwner: TForm; const wstrMsg: WideString;
           DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; modID: TModalFormID = mfNone;
           msgDlgHandler: TModalFormHandler = nil); overload;
    constructor Create(dlgOwner: TDialogs; const wstrMsg: WideString;
           DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; modID: TModalFormID;
            msgDlgHandler: TModalFormHandler); overload;
    destructor Destroy; override;
    property Caption: TCaption read GetCaption write SetCaption;
  end;

implementation

uses
  StdCtrls, SysUtils, MessageDialogUnit;

constructor TDialogForm.Create(frmOwner: TForm; const wstrMsg: WideString;
                   DlgType: TMsgDlgType; Buttons: TMsgDlgButtons;
                   modID: TModalFormID; msgDlgHandler: TModalFormHandler);
var
  i: integer;
begin
  inherited CreateNew(frmOwner);

  self.modID := modID;
  RHandler := msgDlgHandler;

  msgDlg := MessageDialogUnit.CreateMessageDialog(frmOwner, wstrMsg, DlgType, Buttons);
  // msgDlg.FormStyle := frmOwner.FormStyle;
  msgDlg.OnShow := FormShow;
  msgDlg.OnClose := FormClose;

  for i := 0 to (msgDlg.ComponentCount - 1) do
    begin
      if (msgDlg.Components[i] is TButton) then
         (msgDlg.Components[i] as TButton).OnClick := ButtonClick;
    end;
end;

constructor TDialogForm.Create(dlgOwner: TDialogs; const wstrMsg: WideString;
                DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; modID: TModalFormID;
                msgDlgHandler: TModalFormHandler);
begin
  Create((dlgOwner.Owner as TForm), wstrMsg, DlgType, Buttons, modID, msgDlgHandler);
  self.dlgOwner := dlgOwner;
end;



procedure TDialogForm.FormShow(Sender: TObject);
var
  frmOwner: TForm;
begin
  frmOwner := (Owner as TForm);
  if (Assigned(frmOwner)) then
  begin
    msgDlg.Left := frmOwner.Left + (frmOwner.Width - msgDlg.Width) div 2;
    msgDlg.Top := frmOwner.Top + (frmOwner.Height - msgDlg.Height) div 2;
  end;
end;


procedure TDialogForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(dlgOwner) then
    dlgOwner.UnsetShowing(modID, self);
  if fsModal in msgDlg.FormState then
    exit;
  if (Assigned(RHandler)) then
    RHandler(TModalForm(msgDlg), modID);
  Action := caFree;
end;

procedure TDialogForm.Show;
begin
  msgDlg.Show;
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

function TDialogForm.GetCaption: TCaption;
begin
  Result := msgDlg.Caption;
end;

procedure TDialogForm.SetCaption(capt: TCaption);
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

end.
