////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit SelectSkypeContactUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, TntClasses, Graphics, Controls,
  Dialogs, StdCtrls, TntStdCtrls, ExtCtrls, ComCtrls,
  ModalForm;

type
  TSelectSkypeContactForm = class(TModalForm)
    OkButton: TTntButton;
    CancelButton: TTntButton;
    ContactsListBox: TTntListBox;
    procedure FormCreate(Sender: TObject);
    procedure ContactsListBoxDblClick(Sender: TObject);
  private
    procedure FLocalize;
    procedure FUpdateGUI;
    function FSelectedContactIndex: integer;
  protected
    function GetModalID: TModalFormID; override;
  public
    procedure Init(const AContacts: TTntStrings);
    property SelectedContactIndex: integer read FSelectedContactIndex;
  end;

implementation

{$R *.dfm}

uses
  LocalizerUnit;

function TSelectSkypeContactForm.GetModalID: TModalFormID;
begin
  Result := mfSelectSkypeContact;
end;


procedure TSelectSkypeContactForm.FormCreate(Sender: TObject);
begin
  FLocalize;
end;


procedure TSelectSkypeContactForm.FLocalize;
begin
  with TLocalizer.Instance do
  begin
    Caption := GetLabel(68); // Select Skype contact
    OkButton.Caption := GetLabel(11);
    CancelButton.Caption := GetLabel(12);
  end;
end;


procedure TSelectSkypeContactForm.Init(const AContacts: TTntStrings);
begin
  ContactsListBox.Items.Assign(AContacts);
  if (ContactsListBox.Count > 0) then
    ContactsListBox.ItemIndex := 0;
  FUpdateGUI;
end;


procedure TSelectSkypeContactForm.FUpdateGUI;
begin
  OkButton.Enabled := (ContactsListBox.Count > 0);
end;


function TSelectSkypeContactForm.FSelectedContactIndex: integer;
begin
  Result := ContactsListBox.ItemIndex;
end;


procedure TSelectSkypeContactForm.ContactsListBoxDblClick(Sender: TObject);
begin
  OkButton.Click;
end;

end.
