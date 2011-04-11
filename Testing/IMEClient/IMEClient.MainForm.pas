////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit IMEClient.MainForm;

interface

uses
  Forms, Controls, StdCtrls, Mask, Classes,
  //
  IMEClient.Headers, Buttons;

type
  TMainForm = class(TForm, IView)
    memIn: TMemo;
    memOut: TMemo;
    btnSend: TButton;
    HandleNameEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    HandleIdEdit: TMaskEdit;
    DisConnectButton: TButton;
    ContactsListBox: TListBox;
    StartPluginButton: TSpeedButton;
    PluginLogMemo: TMemo;
    Label3: TLabel;
    ClearPluginLogButton: TButton;
    procedure btnSendClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

    function FGetHandleID: integer;
    function FGetHandleName: string;
    function FGetSendText: string;

    procedure DisConnectButtonClick(Sender: TObject);
    procedure ContactsListBoxClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HandleIdEditChange(Sender: TObject);
    procedure HandleNameEditChange(Sender: TObject);
    procedure memOutChange(Sender: TObject);
    procedure StartPluginButtonClick(Sender: TObject);
    procedure ClearPluginLogButtonClick(Sender: TObject);

  private
    m_Events: IViewEvents;
    m_bConnected: boolean;
    m_bUpdating: boolean;

    function FGetContactHandleID: integer;
    procedure FDoChangeContactHandleID(iNewContactHandleID: integer);

    procedure FUpdateControls;
    procedure FDoClose;
    procedure FDoSend;
    procedure FDoDisconnect;
    procedure FDoConnect;

    function FIsContactSelected: boolean;
    function FFormExtHandleName(iHandleID: integer; const strHandleName: string): string;

  protected
    procedure IView.SetEvents = RSetEvents;
    procedure RSetEvents(Value: IViewEvents);
    procedure IView.SetConnected = RSetConnected;
    procedure RSetConnected(bConnected: boolean);
    procedure IView.AddContact = RAddContact;
    procedure RAddContact(iHandleID: integer; const strHandleName: string);
    procedure IView.DeleteContact = RDeleteContact;
    procedure RDeleteContact(iHandleID: integer; const strHandleName: string);
    procedure IView.SetInMessage = RSetInMessage;
    procedure RSetInMessage(iFromHandleID: integer; const strFromHandleName, strMsg: string);
    procedure IView.SetServerMessage = RSetServerMessage;
    procedure RSetServerMessage(const strData: string);
    procedure IView.OutputError = ROutputError;
    procedure ROutputError(const strError: string);
    procedure IView.SetHandleID = RSetHandleID;
    procedure RSetHandleID(iHandleID: integer);
    procedure IView.SetHandleName = RSetHandleName;
    procedure RSetHandleName(const strHandleName: string);
    procedure IView.SetSendText = RSetSendText;
    procedure RSetSendText(const strValue: string);
    procedure IView.AddPluginData = RAddPluginData;
    procedure RAddPluginData(iHandleID: integer; const strHandleName, strData: string; bReceived: boolean);
    procedure IView.ClearPluginData = RClearPluginData;
    procedure RClearPluginData;
  end;

implementation

{$R *.dfm}

uses
  Dialogs, SysUtils, StrUtils, Math;

////////////////////////////////////////////////////////////////////////////////
// TMainForm

const
  CONNECT_BTN_STR = 'Connect';
  DISCONNECT_BTN_STR = 'Disconnect';

function TMainForm.FGetContactHandleID: integer;
begin
 with ContactsListBox do
 begin
   if (ItemIndex >= 0) then
     Result := Integer(ContactsListBox.Items.Objects[ItemIndex])
   else
     Result := 0;
 end;
end;


procedure TMainForm.FDoChangeContactHandleID(iNewContactHandleID: integer);
begin
  if (Assigned(m_Events)) then
    m_Events.OnChangeContactHandleID(iNewContactHandleID)
end;


procedure TMainForm.btnSendClick(Sender: TObject);
begin
  FDoSend;
end;


procedure TMainForm.RSetConnected(bConnected: boolean);
begin
  m_bConnected := bConnected;
  if (not m_bConnected) then
  begin
    ContactsListBox.Clear;
    memOut.Clear;
  end
  else
  begin
    memIn.Clear;
  end;
  FUpdateControls;
end;


function TMainForm.FIsContactSelected: boolean;
begin
  Result := (ContactsListBox.ItemIndex >= 0);
end;


procedure TMainForm.FUpdateControls;
begin
  btnSend.Enabled := (m_bConnected and FIsContactSelected);
  StartPluginButton.Enabled := (m_bConnected and FIsContactSelected);

  memOut.ReadOnly := (not m_bConnected);
  memOut.Enabled := m_bConnected;
  ContactsListBox.Enabled := m_bConnected;

  if (m_bConnected) then
    DisConnectButton.Caption := DISCONNECT_BTN_STR
  else
    DisConnectButton.Caption := CONNECT_BTN_STR;

  HandleIdEdit.Enabled := (not m_bConnected);
  HandleNameEdit.Enabled := (not m_bConnected);

end;


procedure TMainForm.FDoClose;
begin
  if (Assigned(m_Events)) then
    m_Events.OnClose;
end;


procedure TMainForm.FDoSend;
begin
  if (Assigned(m_Events)) then
    m_Events.OnSend;
end;


procedure TMainForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  FDoClose;
end;


function TMainForm.FGetHandleID: integer;
begin
  Result := StrToIntDef(Trim(HandleIdEdit.Text), 0);
end;


procedure TMainForm.RSetHandleID(iHandleID: integer);
begin
  m_bUpdating := TRUE;
  try
    HandleIdEdit.Text := IntToStr(iHandleID);
  finally
    m_bUpdating := FALSE;
  end;
end;


function TMainForm.FGetHandleName: string;
begin
  Result := HandleNameEdit.Text;
end;


function TMainForm.FGetSendText: string;
begin
  Result := memOut.Text;
end;


procedure TMainForm.RSetHandleName(const strHandleName: string);
begin
  m_bUpdating := TRUE;
  try
    HandleNameEdit.Text := strHandleName;
  finally
    m_bUpdating := FALSE;
  end;
end;


procedure TMainForm.RAddContact(iHandleID: integer; const strHandleName: string);
begin
  ContactsListBox.AddItem(FFormExtHandleName(iHandleID, strHandleName), Pointer(iHandleID));
  FUpdateControls;
end;


function TMainForm.FFormExtHandleName(iHandleID: integer; const strHandleName: string): string;
begin
  Result := Format('%s (%d)', [strHandleName, iHandleID]);
end;



procedure TMainForm.RDeleteContact(iHandleID: integer; const strHandleName: string);
var
  iIndex: integer;
begin
  iIndex := ContactsListBox.Items.IndexOfObject(Pointer(iHandleID));
  if (iIndex >= 0) then
  begin
    ContactsListBox.Items.Delete(iIndex);
    FDoChangeContactHandleID(FGetContactHandleID);
  end;

  FUpdateControls;
end;


procedure TMainForm.RSetInMessage(iFromHandleID: integer; const strFromHandleName, strMsg: string);
var
  str: string;
begin
  if (strFromHandleName <> '') then
    str := Format('%s (%d)', [strFromHandleName, iFromHandleID]);

  memIn.Lines.Add('[' + str + ']');
  memIn.Lines.Add(strMsg);
end;


procedure TMainForm.ContactsListBoxClick(Sender: TObject);
begin
  FUpdateControls;
  FDoChangeContactHandleID(FGetContactHandleID);
end;


procedure TMainForm.RSetEvents(Value: IViewEvents);
begin
  m_Events := Value;
end;


procedure TMainForm.DisConnectButtonClick(Sender: TObject);
begin
  if (m_bConnected) then
    FDoDisconnect
  else
    FDoConnect;
end;


procedure TMainForm.FDoDisconnect;
begin
  if (Assigned(m_Events)) then
    m_Events.OnDisconnect;
end;


procedure TMainForm.FDoConnect;
begin
  if (Assigned(m_Events)) then
    m_Events.OnConnect;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  FUpdateControls;
end;


procedure TMainForm.RSetServerMessage(const strData: string);
begin
  memIn.Lines.Add(strData);
end;


procedure TMainForm.ROutputError(const strError: string);
begin
  MessageDlg(strError, mtError, [mbOk], 0);
end;


procedure TMainForm.HandleIdEditChange(Sender: TObject);
begin
  if (m_bUpdating) then
    exit;
  if (Assigned(m_Events)) then
    m_Events.OnChangeHandleID(FGetHandleID); 
end;

procedure TMainForm.HandleNameEditChange(Sender: TObject);
begin
  if (m_bUpdating) then
    exit;
  if (Assigned(m_Events)) then
    m_Events.OnChangeHandleName(FGetHandleName);
end;


procedure TMainForm.RSetSendText(const strValue: string);
begin
  m_bUpdating := TRUE;
  try
    memOut.Text := strValue;
  finally
    m_bUpdating := FALSE;
  end;
end;


procedure TMainForm.RAddPluginData(iHandleID: integer; const strHandleName, strData: string; bReceived: boolean);
var
  str: string;
begin
  str := IfThen(bReceived, '<< ', '>> ') + FFormExtHandleName(iHandleID, strHandleName);
  PluginLogMemo.Lines.Add(str);
  PluginLogMemo.Lines.Add(strData);
end;


procedure TMainForm.memOutChange(Sender: TObject);
begin
  if (m_bUpdating) then
    exit;
  if (Assigned(m_Events)) then
    m_Events.OnChangeSendText(FGetSendText);
end;


procedure TMainForm.StartPluginButtonClick(Sender: TObject);
begin
  if (Assigned(m_Events)) then
    m_Events.OnStartPlugin;
  Width := Max(679, Width);
end;


procedure TMainForm.ClearPluginLogButtonClick(Sender: TObject);
begin
  if (Assigned(m_Events)) then
    m_Events.OnClearPluginData;
end;


procedure TMainForm.RClearPluginData;
begin
  PluginLogMemo.Clear;
end;

end.
