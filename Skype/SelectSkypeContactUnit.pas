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
{
  with TLocalizer.Instance do
  begin
  end;
}
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
  with ContactsListBox do
  begin
    if (ItemIndex >= 0) then
      Result := Integer(Items.Objects[ItemIndex])
    else
      Result := -1;
  end; // with

end;


procedure TSelectSkypeContactForm.ContactsListBoxDblClick(Sender: TObject);
begin
  OkButton.Click;
end;

end.
