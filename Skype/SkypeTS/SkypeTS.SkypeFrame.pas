////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit SkypeTS.SkypeFrame;

interface

uses
  Forms, StdCtrls, TntStdCtrls, ExtCtrls, Controls, Classes,
  //
  SkypeTS.Skype;

type
  TIMSendEvent = procedure(Sender: TObject; const wstrMessage: WideString) of object;

  TSkypeFrame = class(TFrame)
    SkypeGroupBox: TGroupBox;
    HandleLabel: TLabel;
    FullNameLabel: TLabel;
    DisplayNameLabel: TLabel;
    Bevel: TBevel;
    IMLabel: TLabel;
    HandleEdit: TTntEdit;
    FullNameEdit: TTntEdit;
    DisplayNameEdit: TTntEdit;
    AttachButton: TButton;
    SendIMEdit: TTntEdit;
    SendIMButton: TButton;
    IMMemo: TTntMemo;
    ContactLabel: TLabel;
    ContactComboBox: TComboBox;
    procedure SendIMButtonClick(Sender: TObject);
    procedure AttachButtonClick(Sender: TObject);
  private
    FOnAttach: TNotifyEvent;
    FOnIMSend: TIMSendEvent;
    function FGetHandle: WideString;
    function FGetFullName: WideString;
    function FGetDisplayName: WideString;
    procedure FDoIMSend;
    procedure FDoAttach;
    function FGetContactID: integer;
  public
    procedure UpdateGUI(Skype: TSkype; bEnableSendIMGroup: boolean);
    procedure IMLogAdd(const wstrMessage: WideString);
    procedure SetContacts(const Contacts: TStrings);
    property OnAttach: TNotifyEvent read FOnAttach write FOnAttach;
    property OnIMSend: TIMSendEvent read FOnIMSend write FOnIMSend;
    property Handle: WideString read FGetHandle;
    property FullName: WideString read FGetFullName;
    property DisplayName: WideString read FGetDisplayName;
    property ContactID: integer read FGetContactID;
  end;

implementation

{$R *.dfm}

////////////////////////////////////////////////////////////////////////////////
// TSkypeFrame

procedure TSkypeFrame.UpdateGUI(Skype: TSkype; bEnableSendIMGroup: boolean);
var
  bAvailable: boolean;
  i: integer;
  bEnabled: boolean;
begin
  bAvailable := Assigned(Skype);
  SkypeGroupBox.Enabled := bAvailable;
  for i := 0 to SkypeGroupBox.ControlCount - 1 do
    SkypeGroupBox.Controls[i].Enabled := bAvailable;
  if (bAvailable) then
  begin
    AttachButton.Enabled := (not (ssAttached in Skype.States));
    HandleEdit.ReadOnly := (ssAttached in Skype.States);
    FullNameEdit.ReadOnly := (ssAttached in Skype.States);
    DisplayNameEdit.ReadOnly := (ssAttached in Skype.States);
  end;

  bEnabled := (bEnableSendIMGroup and bAvailable and (ssAttached in Skype.States));
  SendIMButton.Enabled := bEnabled;
  SendIMEdit.Enabled := bEnabled;
  ContactComboBox.Enabled := bEnabled;
  ContactLabel.Enabled := bEnabled;
end;


function TSkypeFrame.FGetHandle: WideString;
begin
  Result := HandleEdit.Text;
end;


function TSkypeFrame.FGetFullName: WideString;
begin
  Result := FullNameEdit.Text;
end;


function TSkypeFrame.FGetDisplayName: WideString;
begin
  Result := DisplayNameEdit.Text;
end;


procedure TSkypeFrame.SendIMButtonClick(Sender: TObject);
begin
   FDoIMSend;
end;


procedure TSkypeFrame.FDoIMSend;
begin
  if (SendIMEdit.Text = '') then
    exit;

  if (Assigned(FOnIMSend)) then
  begin
     FOnIMSend(self, SendIMEdit.Text);
     SendIMEdit.Text := '';
  end;
end;


procedure TSkypeFrame.AttachButtonClick(Sender: TObject);
begin
  FDoAttach;
end;


procedure TSkypeFrame.FDoAttach;
begin
  if (Assigned(FOnAttach)) then
    FOnAttach(self);
end;


procedure TSkypeFrame.IMLogAdd(const wstrMessage: WideString);
begin
  IMMemo.Lines.Append(wstrMessage);
end;


procedure TSkypeFrame.SetContacts(const Contacts: TStrings);
var
  strlContacts: TStringList;
  iIndex: integer;
begin
  strlContacts := TStringList.Create;
  try
    strlContacts.Assign(Contacts);
    iIndex := strlContacts.IndexOfObject(TObject(Tag));
    Assert(iIndex >= 0);
    strlContacts.Delete(iIndex);
    ContactComboBox.Items.Assign(strlContacts);
  finally
    strlContacts.Free;
  end;
  ContactComboBox.ItemIndex := 0;
end;


function TSkypeFrame.FGetContactID: integer;
begin
  if (ContactComboBox.ItemIndex >= 0) then
    Result := Integer(ContactComboBox.Items.Objects[ContactComboBox.ItemIndex])
  else
    Result := -1;
end;

end.
