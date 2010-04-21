unit IMEClient.MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdException,
  Mask;

type
  TMainForm = class(TForm)
    memIn: TMemo;
    memOut: TMemo;
    btnSend: TButton;
    TCPClient: TIdTCPClient;
    HandleNameEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    HandleIdEdit: TMaskEdit;
    DisConnectButton: TButton;
    ContactsListBox: TListBox;
    procedure btnSendClick(Sender: TObject);
    procedure TCPClientConnected(Sender: TObject);
    procedure TCPClientDisconnected(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

    function FGetHandleID: integer;
    procedure FSetHandleID(Value: integer);
    function FGetHandleName: string;
    procedure FSetHandleName(const Value: string);    

    property HandleID: integer read FGetHandleID write FSetHandleID;
    property HandleName: string read FGetHandleName write FSetHandleName;
    procedure DisConnectButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ContactsListBoxClick(Sender: TObject);

  private
    m_strTCPClientData: string;
    m_strlContacts: TStringList;
    procedure FOnTCPClientDataReceived;
    procedure FSendConnect;
    procedure FSendMessage(const strMsg: string; iHandleID: integer);
    procedure FUpdateControls;
    function FParseCommands(strData: string): boolean;
    procedure FAddContact(iHandleID: integer; const strHandleName: string);
    procedure FDeleteContact(iHandleID: integer; const strHandleName: string);
    procedure FOnDisconnectedFromServer;
    procedure FDisConnect;
    procedure FLoadSettings;
    procedure FSaveSettings;
    function FGetIniFileName: string;
    property IniFileName: string read FGetIniFileName;
  end;

implementation

{$R *.dfm}

uses
  HTTPApp, StrUtils, IniFiles;

type
  TCPClientListenerThread = class(TThread)
  private
    m_MsgClientForm: TMainForm;
    procedure FOnTCPClientDataReceived(const strData: string);
  protected
    procedure Execute; override;
  public
    constructor Create(AMsgClientForm: TMainForm);
  end;

function GetNextToken(var strData: string; chDelim: char = ' '): string;
var
  iPos: integer;
begin
  strData := TrimLeft(strData);
  iPos := Pos(string(chDelim), strData);
  if (iPos > 0) then
  begin
    Result := LeftStr(strData, Pred(iPos));
    strData := Copy(strData, Succ(iPos), MaxInt);
  end
  else
  begin
    Result := strData;
    strData := '';
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TMainForm

const
  CONNECT_BTN_STR = 'Connect';
  DISCONNECT_BTN_STR = 'Disconnect';

  INI_SECTION_SETTINGS = 'Settings';
  INI_HANDLE_ID_IDENT = 'HandleID';
  INI_HANDLE_NAME_IDENT = 'HandleName';

procedure TMainForm.btnSendClick(Sender: TObject);
var
  iHandleID: integer;
begin
  with ContactsListBox do
  begin
    Assert(ItemIndex >= 0);
    iHandleID := Integer(m_strlContacts.Objects[ItemIndex]);
  end;

  if (memOut.Text <> '') then
  begin
    FSendMessage(memOut.Text, iHandleID);
    memOut.Text := '';
  end;
end;


procedure TMainForm.FSendMessage(const strMsg: string; iHandleID: integer);
begin
  TCPClient.WriteLn(Format('>message %d %s', [iHandleID, HTTPEncode(strMsg)]));
end;


procedure TMainForm.TCPClientConnected(Sender: TObject);
begin
  FUpdateControls;
  TCPClientListenerThread.Create(self);
  FSendConnect;
end;


procedure TMainForm.FUpdateControls;
var
  bConnected: boolean;
begin
  bConnected := TCPClient.Connected;

  btnSend.Enabled := (bConnected and (ContactsListBox.ItemIndex >= 0));
  memOut.ReadOnly := (not bConnected);
  memOut.Enabled := bConnected;
  ContactsListBox.Enabled := bConnected;

  if (bConnected) then
    DisConnectButton.Caption := DISCONNECT_BTN_STR
  else
    DisConnectButton.Caption := CONNECT_BTN_STR;

  HandleIdEdit.Enabled := (not bConnected);
  HandleNameEdit.Enabled := (not bConnected);
end;


procedure TMainForm.FOnDisconnectedFromServer;
begin
  ContactsListBox.Clear;
  memIn.Clear;
  memOut.Clear;
  FUpdateControls;
end;


procedure TMainForm.FSendConnect;
var
  strHTTPHN, strCmd: string;
begin
  strHTTPHN := HttpEncode(HandleName);
  strCmd := Format('>connect %s %d', [strHTTPHN, HandleID]);
  TCPClient.WriteLn(strCmd);
end;


procedure TMainForm.TCPClientDisconnected(Sender: TObject);
begin
  FOnDisconnectedFromServer;
end;


procedure TMainForm.FOnTCPClientDataReceived;
var
  strData: string;
begin
  if (m_strTCPClientData <> '') then
  begin
    strData := m_strTCPClientData;
    m_strTCPClientData := '';
    if (not FParseCommands(strData)) then
      memIn.Lines.Add(strData);
  end;
end;


procedure TMainForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if (TCPClient.Connected) then
    TCPClient.Disconnect;
//  if (Assigned(m_TCPClientListener)) then
//    m_TCPClientListener.WaitFor;
end;


function TMainForm.FGetHandleID: integer;
begin
  Result := StrToInt(HandleIdEdit.Text);
end;


procedure TMainForm.FSetHandleID(Value: integer);
begin
  HandleIdEdit.Text := IntToStr(Value);
end;


function TMainForm.FGetHandleName: string;
begin
  Result := HandleNameEdit.Text;
  if (Result = '') then
    Result := IntToStr(HandleID);
end;


procedure TMainForm.FSetHandleName(const Value: string);
begin
  HandleNameEdit.Text := Value;
end;


procedure TMainForm.FDisConnect;
begin
  if (TCPClient.Connected) then
    TCPClient.Disconnect
  else
  begin
    try
      TCPClient.Connect;
    except
      on EIdSocketError do
        MessageDlg('Cannot connect to server!', mtError, [mbOk], 0);
    end;
  end;
end;


procedure TMainForm.FormCreate(Sender: TObject);
begin
  m_strlContacts := TStringList.Create;
  FLoadSettings;  
  FUpdateControls;
end;


procedure TMainForm.FAddContact(iHandleID: integer; const strHandleName: string);
begin
  ContactsListBox.AddItem(Format('%s (%d)', [strHandleName, iHandleID]), Pointer(iHandleID));
  m_strlContacts.AddObject(strHandleName, Pointer(iHandleID));
  FUpdateControls;
end;


procedure TMainForm.FDeleteContact(iHandleID: integer; const strHandleName: string);
var
  iIndex: integer;
begin
  iIndex := ContactsListBox.Items.IndexOfObject(Pointer(iHandleID));
  if (iIndex >= 0) then
    ContactsListBox.Items.Delete(iIndex);

  iIndex := m_strlContacts.IndexOfObject(Pointer(iHandleID));
  if (iIndex >= 0) then
    m_strlContacts.Delete(iIndex);

  FUpdateControls;    
end;


function TMainForm.FParseCommands(strData: string): boolean;

 function NParseCommandEnters(strData: string): boolean;
  var
    strHandleName: string;
    iHandleID: integer;
  begin
    strHandleName := HTTPDecode(GetNextToken(strData));
    iHandleID := StrToIntDef(GetNextToken(strData), 0);
    Result := ((strHandleName <> '') and (iHandleID > 0));
    if (Result) then
      FAddContact(iHandleID, strHandleName);
  end;

  function NParseCommandLeaves(strData: string): boolean;
  var
    strHandleName: string;
    iHandleID: integer;
  begin
    strHandleName := GetNextToken(strData);
    iHandleID := StrToIntDef(GetNextToken(strData), 0);
    Result := ((strHandleName <> '') and (iHandleID > 0));
    if (Result) then
      FDeleteContact(iHandleID, strHandleName);
  end;


  function NParseCommandMessage(strData: string): boolean;
  var
    strMsg: string;
    iFromHandleID: integer;
    strFromHandleName: string;
    iIndex: integer;
  begin
    Result := FALSE;
    iFromHandleID := StrToIntDef(GetNextToken(strData), 0);
    strMsg := HTTPDecode(GetNextToken(strData));
    if (strMsg <> '') then
    begin
      iIndex := m_strlContacts.IndexOfObject(Pointer(iFromHandleID));
      if (iIndex >= 0) then
        strFromHandleName := Format('%s (%d)', [m_strlContacts[iIndex], Integer(m_strlContacts.Objects[iIndex])])
      else
        strFromHandleName := '';

      memIn.Lines.Add('[' + strFromHandleName + ']');
      memIn.Lines.Add(strMsg);
      Result := TRUE;
    end;
  end;

var
  strCmd: string;
begin // TMainForm.FParseCommands
  strCmd := GetNextToken(strData);
  if (strCmd = '>message') then
    Result := NParseCommandMessage(strData)
  else if (strCmd = '>enters') then
    Result := NParseCommandEnters(strData)
  else if (strCmd = '>leaves') then
    Result := NParseCommandLeaves(strData)
  else
    Result := FALSE;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FSaveSettings;
  m_strlContacts.Free;
end;


procedure TMainForm.ContactsListBoxClick(Sender: TObject);
begin
  FUpdateControls;
end;


procedure TMainForm.FLoadSettings;
begin
  with TIniFile.Create(IniFileName) do
  try
    HandleID := ReadInteger(INI_SECTION_SETTINGS, INI_HANDLE_ID_IDENT, 1234);
    HandleName := ReadString(INI_SECTION_SETTINGS, INI_HANDLE_NAME_IDENT, 'HandleName' + IntToStr(HandleID));
  finally
    Free;
  end;
end;


procedure TMainForm.FSaveSettings;
begin
  with TIniFile.Create(IniFileName) do
  try
    WriteInteger(INI_SECTION_SETTINGS, INI_HANDLE_ID_IDENT, HandleID);
    WriteString(INI_SECTION_SETTINGS, INI_HANDLE_NAME_IDENT, HandleName);
  finally
    Free;
  end;
end;


function TMainForm.FGetIniFileName: string;
begin
  Result := ChangeFileExt(Application.ExeName, '.ini');
end;


////////////////////////////////////////////////////////////////////////////////
// TCPClientListenerThread

constructor TCPClientListenerThread.Create(AMsgClientForm: TMainForm);
begin
  inherited Create(TRUE);
  m_MsgClientForm := AMsgClientForm;
  FreeOnTerminate := TRUE;
  Resume;
end;


procedure TCPClientListenerThread.Execute;
var
  strData: string;
begin
  while (m_MsgClientForm.TCPClient.Connected) do
  begin
    try
      strData := m_MsgClientForm.TCPClient.ReadLn;
    except
      on EIdNotConnected do
        ;
      on EIdConnClosedGracefully do
        ;
      on Exception do
        raise;
    end;
    if (strData <> '') then
    begin
      FOnTCPClientDataReceived(strData);
      strData := '';
    end;
    Sleep(1);
  end;
end;


procedure TCPClientListenerThread.FOnTCPClientDataReceived(const strData: string);
begin
  m_MsgClientForm.m_strTCPClientData := strData;
  Synchronize(m_MsgClientForm.FOnTCPClientDataReceived);
end;


procedure TMainForm.DisConnectButtonClick(Sender: TObject);
begin
  FDisConnect;
end;

end.
