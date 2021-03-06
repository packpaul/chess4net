////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit ModalForm;

interface

uses
  Forms, Dialogs, Classes, Controls, LCLType,
  //
  ModalFormBase;

type
  TModalForm = class;
  TModalFormClass = class of TModalForm;

  TDialogs = class(TDialogsBase)
  private
    IDCount: array[TModalFormID] of word;
    frmList: TList;
    function GetShowing: boolean;
  public
    constructor Create(aOwner: TForm; aHandler: TModalFormHandler);
    destructor Destroy; override;
    procedure MessageDlg(const Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; msgDlgID: TModalFormID);
    function CreateDialog(modalFormClass: TModalFormClass): TModalForm;
    procedure SetShowing(msgDlgID: TModalFormID); override;
    procedure UnsetShowing(msgDlgID: TModalFormID; msgDlg: TModalFormBase = nil); override;
    function InFormList(frm: TForm): boolean;
    procedure BringToFront;
    procedure CloseNoneDialogs;
    procedure MoveForms(dx, dy: integer);
    property Showing: boolean read GetShowing;
  end;

  TModalForm = class(TModalFormBase)
  private
    procedure ButtonClick(Sender: TObject);
  protected
    function GetHandle: hWnd; override;
    function GetEnabled_: boolean; override;
    procedure SetEnabled_(flag: boolean); override;
    function RGetModalResult: TModalResult; override;
    procedure RSetModalResult(Value: TModalResult); override;
    function GetLeft_: integer; override;
    procedure SetLeft_(x: integer); override;
    function GetTop_: integer; override;
    procedure SetTop_(y: integer); override;
    function RGetWidth: integer; override;
    procedure RSetWidth(iValue: integer); override;
    function RGetHeight: integer; override;
    procedure RSetHeight(iValue: integer); override;
    function RGetConstraints: TSizeConstraints; override;
    function RGetOnClose: TCloseEvent; override;
    procedure RSetOnClose(Value: TCloseEvent); override;
    function RGetFormState: TFormState; override;
  public
    constructor Create(aOwner: TForm; modHandler: TModalFormHandler = nil); override;

    procedure Show; override;
    procedure SetFocus; override;

    function GetModalID: TModalFormID; override;
  end;

implementation

uses
  SysUtils, StdCtrls,
  //
  GlobalsUnit;

type
  TModalFormDecorator = class(TModalFormBase)
  private
    m_Form: TForm;
    m_ModalID: TModalFormID;
    FGenFormClose: TCloseEvent;
    procedure FFormClose(Sender: TObject; var aAction: TCloseAction);
  protected
    function GetHandle: hWnd; override;
    function GetEnabled_: boolean; override;
    procedure SetEnabled_(flag: boolean); override;
    function RGetModalResult: TModalResult; override;
    procedure RSetModalResult(Value: TModalResult); override;
    function GetLeft_: integer; override;
    procedure SetLeft_(x: integer); override;
    function GetTop_: integer; override;
    procedure SetTop_(y: integer); override;
    function RGetWidth: integer; override;
    procedure RSetWidth(iValue: integer); override;
    function RGetHeight: integer; override;
    procedure RSetHeight(iValue: integer); override;
    function RGetConstraints: TSizeConstraints; override;
    function RGetOnClose: TCloseEvent; override;
    procedure RSetOnClose(Value: TCloseEvent); override;
    function RGetFormState: TFormState; override;

  public
    constructor Create(const AForm: TForm; AModalID: TModalFormID;
      aDlgOwner: TDialogs); reintroduce;
    procedure SetFocus; override;
    procedure Show; override;
    function GetModalID: TModalFormID; override;
    property Handle: hWnd read GetHandle;
    property Enabled: boolean read GetEnabled_ write SetEnabled_;
  end;

////////////////////////////////////////////////////////////////////////////////
// TModalForm


procedure TModalForm.ButtonClick(Sender: TObject);
begin
  if fsModal in FormState then
    exit;  
  Close;
end;


constructor TModalForm.Create(aOwner: TForm; modHandler: TModalFormHandler);
var
  i: integer;
begin
  FormStyle := aOwner.FormStyle;

  inherited Create(aOwner, modHandler);

  for i := 0 to (ComponentCount - 1) do
  begin
    if (Components[i] is TButton) then
      (Components[i] as TButton).OnClick := ButtonClick;
  end;
end;


function TModalForm.GetModalID : TModalFormID;
begin
  Result := mfNone;
end;


function TModalForm.GetHandle: hWnd;
begin
  Result := (self as TForm).Handle;
end;


function TModalForm.GetEnabled_: boolean;
begin
  Result := (self as TForm).Enabled;
end;


procedure TModalForm.SetEnabled_(flag: boolean);
begin
  (self as TForm).Enabled := flag;
end;


function TModalForm.RGetModalResult: TModalResult;
begin
  Result := (self as TForm).ModalResult;
end;


procedure TModalForm.RSetModalResult(Value: TModalResult);
begin
  (self as TForm).ModalResult := Value;
end;

procedure TModalForm.Show;
begin
  (self as TForm).Show;
end;


function TModalForm.GetLeft_: integer;
begin
  Result := (self as TForm).Left;
end;


procedure TModalForm.SetLeft_(x: integer);
begin
  (self as TForm).Left := x;
end;


function TModalForm.GetTop_: integer;
begin
  Result := (self as TForm).Top;
end;


procedure TModalForm.SetTop_(y: integer);
begin
  (self as TForm).Top := y;
end;


function TModalForm.RGetWidth: integer;
begin
  Result := (self as TForm).Width;
end;


procedure TModalForm.RSetWidth(iValue: integer);
begin
  (self as TForm).Width := iValue;
end;


function TModalForm.RGetHeight: integer;
begin
  Result := (self as TForm).Height;
end;


procedure TModalForm.RSetHeight(iValue: integer);
begin
  (self as TForm).Height := iValue;
end;


procedure TModalForm.SetFocus;
begin
  (self as TForm).SetFocus;
end;


function TModalForm.RGetConstraints: TSizeConstraints;
begin
  Result := (self as TForm).Constraints;
end;


function TModalForm.RGetOnClose: TCloseEvent;
begin
  Result := (self as TForm).OnClose;
end;


procedure TModalForm.RSetOnClose(Value: TCloseEvent);
begin
  (self as TForm).OnClose := Value;
end;


function TModalForm.RGetFormState: TFormState;
begin
  Result := (self as TForm).FormState;
end;

////////////////////////////////////////////////////////////////////////////////
// TDialogs

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


procedure TDialogs.UnsetShowing(msgDlgID: TModalFormID; msgDlg: TModalFormBase = nil);
var
  i: integer;
begin
  dec(IDCount[msgDlgID]);

  if Assigned(msgDlg) then
    begin
      for i := 0 to frmList.Count - 1 do
        begin
          if TModalFormBase(frmList[i]).Handle = msgDlg.Handle then
            begin
              frmList.Delete(i);
              break;
            end;
        end; { for }
    end;
  if frmList.Count > 0 then
    begin
      TModalFormBase(frmList.Last).Enabled := TRUE;
      TModalFormBase(frmList.Last).SetFocus;
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
      if (TModalFormBase(frmList[i]).Handle = frm.Handle) then
        begin
          Result := TRUE;
          exit;
        end;
    end;
  Result := FALSE;
end;


procedure TDialogs.MessageDlg(const Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; msgDlgID: TModalFormID);
var
  DialogForm: TForm;
  ModalFormDecorator: TModalFormDecorator;
begin
  if (msgDlgID <> mfNone) and (IDCount[msgDlgID] > 0) then
    exit;

  ModalFormDecorator := nil;

  DialogForm := CreateMessageDialog(Msg, DlgType, Buttons);
  with DialogForm do
  try
    Position := poDesigned;
    Caption := DIALOG_CAPTION;
    FormStyle := self.Owner.FormStyle;

    ModalFormDecorator := TModalFormDecorator.Create(DialogForm, msgDlgID, self);
    frmList.Add(ModalFormDecorator);

    DialogForm.ShowModal;

  finally
    ModalFormDecorator.Free;
  end;
end;


function TDialogs.CreateDialog(modalFormClass: TModalFormClass): TModalForm;
begin
  Result := modalFormClass.Create(self);
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
    TModalFormBase(frmList.Last).Enabled := FALSE;
end;


procedure TDialogs.BringToFront;
var
  i: integer;
begin
  if frmList.Count = 0 then
    exit;
  for i := 0 to frmList.Count - 1 do
    TModalFormBase(frmList[i]).Show;
  TModalFormBase(frmList.Last).SetFocus;
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
      Dlg.Close;
    dec(i);
  end;
end;

procedure TDialogs.MoveForms(dx, dy: integer);
var
  i: integer;
begin
  for i := 0 to frmList.Count - 1 do
    with TModalFormBase(frmList[i]) do
    begin
      Left := Left + dx;
      Top := Top + dy;
    end;
end;

////////////////////////////////////////////////////////////////////////////////
// TModalFormDecorator

constructor TModalFormDecorator.Create(const AForm: TForm; AModalID: TModalFormID;
  aDlgOwner: TDialogs);
begin
  m_Form := AForm;
  m_ModalID := AModalID;
  FGenFormClose := m_Form.OnClose;

  inherited Create(aDlgOwner);
end;


function TModalFormDecorator.GetHandle: hWnd;
begin
  Result := m_Form.Handle;
end;


function TModalFormDecorator.GetEnabled_: boolean;
begin
  Result := m_Form.Enabled;
end;


procedure TModalFormDecorator.SetEnabled_(flag: boolean);
begin
  m_Form.Enabled := flag;
end;


function TModalFormDecorator.RGetModalResult: TModalResult;
begin
  Result := m_Form.ModalResult;
end;


procedure TModalFormDecorator.RSetModalResult(Value: TModalResult);
begin
  m_Form.ModalResult := Value;
end;

procedure TModalFormDecorator.SetFocus;
begin
  m_Form.SetFocus;
end;


procedure TModalFormDecorator.Show;
begin
  m_Form.Show;
end;


function TModalFormDecorator.GetModalID: TModalFormID;
begin
  Result := m_ModalID;
end;

function TModalFormDecorator.GetLeft_: integer;
begin
  Result := m_Form.Left;
end;


procedure TModalFormDecorator.SetLeft_(x: integer);
begin
  m_Form.Left := x;
end;


function TModalFormDecorator.GetTop_: integer;
begin
  Result := m_Form.Top;
end;


procedure TModalFormDecorator.SetTop_(y: integer);
begin
  m_Form.Top := y;
end;


function TModalFormDecorator.RGetWidth: integer;
begin
  Result := m_Form.Width;
end;


procedure TModalFormDecorator.RSetWidth(iValue: integer);
begin
  m_Form.Width := iValue;
end;


function TModalFormDecorator.RGetHeight: integer;
begin
  Result := m_Form.Height;
end;


procedure TModalFormDecorator.RSetHeight(iValue: integer);
begin
  m_Form.Height := iValue;
end;


function TModalFormDecorator.RGetConstraints: TSizeConstraints;
begin
  Result := m_Form.Constraints;
end;


function TModalFormDecorator.RGetOnClose: TCloseEvent;
begin
  Result := FGenFormClose;
end;


procedure TModalFormDecorator.RSetOnClose(Value: TCloseEvent);
begin
  FGenFormClose := Value;
  if (Assigned(FGenFormClose)) then
    m_Form.OnClose := FFormClose
  else
    m_Form.OnClose := nil;
end;


procedure TModalFormDecorator.FFormClose(Sender: TObject; var aAction: TCloseAction);
begin
  if (Assigned(FGenFormClose)) then
    FGenFormClose(self, aAction);
//  if (aAction = caFree) then // this instance is always freed explicitly
//    Release;
end;


function TModalFormDecorator.RGetFormState: TFormState;
begin
  Result := m_Form.FormState;
end;

end.

