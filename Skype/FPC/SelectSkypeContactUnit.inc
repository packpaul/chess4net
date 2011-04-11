////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

{$MODE Delphi}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls,
  //
  ModalFormBase, ModalForm;

type

  { TSelectSkypeContactForm }

  TSelectSkypeContactForm = class(TModalForm)
    OkButton: TButton;
    CancelButton: TButton;
    ContactsListBox: TListBox;
    procedure ContactsListBoxDblClick(Sender: TObject);
  private
    function FGetSelectedContactIndex: integer;
    procedure FUpdateGUI;
  protected
    function GetModalID: TModalFormID; override;
  public
    procedure Init(const AContacts: TStrings);
    property SelectedContactIndex: integer read FGetSelectedContactIndex;
  end;

implementation

function TSelectSkypeContactForm.GetModalID: TModalFormID;
begin
  Result := mfSelectSkypeContact;
end;


procedure TSelectSkypeContactForm.Init(const AContacts: TStrings);
begin
  ContactsListBox.Items.Assign(AContacts);
  if (ContactsListBox.Count > 0) then
    ContactsListBox.ItemIndex := 0;
  FUpdateGUI;
end;


procedure TSelectSkypeContactForm.ContactsListBoxDblClick(Sender: TObject);
begin
  OkButton.Click;
end;


function TSelectSkypeContactForm.FGetSelectedContactIndex: integer;
begin
  with ContactsListBox do
  begin
    if (ItemIndex >= 0) then
      Result := Integer(Items.Objects[ItemIndex])
    else
      Result := -1;
  end; // with
end;


procedure TSelectSkypeContactForm.FUpdateGUI;
begin
  OkButton.Enabled := (ContactsListBox.Count > 0);
end;

initialization
  {$I SelectSkypeContactUnit.lrs}

end.

