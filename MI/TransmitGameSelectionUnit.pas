unit TransmitGameSelectionUnit;

interface

uses
  Controls, StdCtrls, TntStdCtrls, Classes,
  //
  ModalForm;

type
  TTransmitGameSelectionForm = class(TModalForm)
    OkButton: TTntButton;
    CancelButton: TTntButton;
    TransmitGameListBox: TTntListBox;
    procedure FormCreate(Sender: TObject);
  private
    procedure FLocalize;

  public
    class function GetModalID: TModalFormID; override;
  end;

implementation

{$R *.dfm}

uses
  LocalizerUnit;

procedure TTransmitGameSelectionForm.FormCreate(Sender: TObject);
begin
  FLocalize;
end;


procedure TTransmitGameSelectionForm.FLocalize;
begin
  with TLocalizer.Instance do
  begin
    // TODO:

    // Caption := GetLabel(24);
    OkButton.Caption := GetLabel(11);
    CancelButton.Caption := GetLabel(12);
  end;
end;


class function TTransmitGameSelectionForm.GetModalID: TModalFormID;
begin
  Result := mfTransmitGame;
end;

end.
