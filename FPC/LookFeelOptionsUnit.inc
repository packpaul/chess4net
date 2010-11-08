
{$MODE Delphi}

interface

uses
  LCLIntf, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons, LResources,
  //
  ModalFormBase, ModalForm, LocalizerUnit;

type

  { TLookFeelOptionsForm }

  TLookFeelOptionsForm = class(TModalForm, ILocalizable)
    GUILangLabel: TLabel;
    GUILangComboBox: TComboBox;
    OkButton: TButton;
    CancelButton: TButton;
    AnimationComboBox: TComboBox;
    AnimateLabel: TLabel;
    BoxPanel: TPanel;
    HilightLastMoveBox: TCheckBox;
    CoordinatesBox: TCheckBox;
    StayOnTopBox: TCheckBox;
    ExtraExitBox: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GUILangComboBoxChange(Sender: TObject);
  private
    procedure ILocalizable.Localize = FLocalize;
    procedure FLocalize;
  public
    function GetModalID : TModalFormID; override;
  end;

implementation

function TLookFeelOptionsForm.GetModalID: TModalFormID;
begin
  Result := mfLookFeel;
end;


procedure TLookFeelOptionsForm.FLocalize;
var
  iSavedAnimation: integer;
begin
  with TLocalizer.Instance do
  begin
    Caption := GetLabel(0);
    AnimateLabel.Caption := GetLabel(1);
    with AnimationComboBox do
    begin
      iSavedAnimation := ItemIndex;
      Items[0] := GetLabel(2);
      Items[1] := GetLabel(3);
      Items[2] := GetLabel(4);
      ItemIndex := iSavedAnimation;
    end;
    HilightLastMoveBox.Caption := GetLabel(5);
//    FlashIncomingMoveBox.Caption := GetLabel(6);
    CoordinatesBox.Caption := GetLabel(7);
    StayOnTopBox.Caption := GetLabel(8);
    ExtraExitBox.Caption := GetLabel(9);
    GUILangLabel.Caption := GetLabel(10);

    OkButton.Caption := GetLabel(11);
    CancelButton.Caption := GetLabel(12);
  end;
end;



procedure TLookFeelOptionsForm.FormCreate(Sender: TObject);
var
  i: integer;
begin
{$IFDEF LCLgtk2}
  StayOnTopBox.Enabled := FALSE;
{$ENDIF}

  // Fill GUI Languages combo box
  GUILangComboBox.Clear;
  with TLocalizer.Instance do
  begin
    for i := 0 to LanguagesCount - 1 do
      GUILangComboBox.Items.Add(LanguageName[i]);
    GUILangComboBox.ItemIndex := ActiveLanguage;
  end;

  TLocalizer.Instance.AddSubscriber(self);
  FLocalize;
end;


procedure TLookFeelOptionsForm.GUILangComboBoxChange(Sender: TObject);
begin
  TLocalizer.Instance.ActiveLanguage := GUILangComboBox.ItemIndex;
end;


procedure TLookFeelOptionsForm.FormDestroy(Sender: TObject);
begin
  TLocalizer.Instance.DeleteSubscriber(self);
end;

initialization
  {$i LookFeelOptionsUnit.lrs}

end.
