unit TransmitGameSelectionUnit;

interface

uses
  Forms, Controls, StdCtrls, TntStdCtrls, Classes,
  //
  ModalForm;

type
  TTransmitGameSelectionForm = class(TModalForm)
    OkButton: TTntButton;
    CancelButton: TTntButton;
    TransmitGameListBox: TTntListBox;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    procedure FLocalize;

  public
    class function GetModalID: TModalFormID; override;
    procedure SetGames(Games: TStrings);
    function GetSelected: TObject;
  end;

implementation

{$R *.dfm}

uses
  LocalizerUnit;

////////////////////////////////////////////////////////////////////////////////
// TTransmitGameSelectionForm

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


procedure TTransmitGameSelectionForm.SetGames(Games: TStrings);
begin
  TransmitGameListBox.Items.Assign(Games);
  if (TransmitGameListBox.Count > 0) then
    TransmitGameListBox.ItemIndex := 0;
end;


function TTransmitGameSelectionForm.GetSelected: TObject;
var
  iIndex: integer;
begin
  iIndex := TransmitGameListBox.ItemIndex;
  if (iIndex >= 0) then
    Result := TransmitGameListBox.Items.Objects[iIndex]
  else
    Result := nil;
end;

procedure TTransmitGameSelectionForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  ModalResult := CancelButton.ModalResult;
end;

end.
