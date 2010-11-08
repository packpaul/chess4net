{$MODE Delphi}

interface

uses
  LCLIntf, SysUtils, Classes, Graphics, Controls,
  Dialogs, StdCtrls, ExtCtrls, Buttons, ComCtrls, LResources,
  //
  ModalFormBase, ModalForm;

type

  { TGameOptionsForm }

  TGameOptionsForm = class(TModalForm)
    OkButton: TButton;
    CancelButton: TButton;
    TimeControlGroupBox: TGroupBox;
    EqualTimeCheckBox: TCheckBox;
    YouGroupBox: TGroupBox;
    YouMinLabel: TLabel;
    YouIncLabel: TLabel;
    YouMinEdit: TEdit;
    YouIncEdit: TEdit;
    YouUnlimitedCheckBox: TCheckBox;
    OpponentGroupBox: TGroupBox;
    OpponentMinLabel: TLabel;
    OpponentIncLabel: TLabel;
    OpponentIncEdit: TEdit;
    OpponentMinEdit: TEdit;
    OpponentUnlimitedCheckBox: TCheckBox;
    Panel1: TPanel;
    AutoFlagCheckBox: TCheckBox;
    TakeBackCheckBox: TCheckBox;
    TrainingModeGroupBox: TGroupBox;
    TrainingEnabledCheckBox: TCheckBox;
    ExtBaseComboBox: TComboBox;
    UsrBaseCheckBox: TCheckBox;
    ExtBaseLabel: TLabel;
    GamePauseCheckBox: TCheckBox;
    GameAdjournCheckBox: TCheckBox;
    YouMinUpDown: TUpDown;
    YouIncUpDown: TUpDown;
    OpponentMinUpDown: TUpDown;
    OpponentIncUpDown: TUpDown;
    procedure EqualTimeCheckBoxChange(Sender: TObject);
    procedure TrainingEnabledCheckBoxChange(Sender: TObject);
    procedure YouEditChange(Sender: TObject);
    procedure OpponentEditChange(Sender: TObject);
    procedure ExtBaseComboBoxChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure UnlimitedCheckBoxChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);

  private
    procedure FLocalize;

  public
    function GetModalID: TModalFormID; override;
  end;

implementation

uses
  LocalizerUnit;

procedure TGameOptionsForm.YouEditChange(Sender: TObject);
begin
  YouMinEdit.Text := IntToStr(YouMinUpDown.Position);
  YouIncEdit.Text := IntToStr(YouIncUpDown.Position);
  if EqualTimeCheckBox.Checked then
    begin
      OpponentMinEdit.Text := YouMinEdit.Text;
      OpponentIncEdit.Text := YouIncEdit.Text;
    end;
end;

procedure TGameOptionsForm.EqualTimeCheckBoxChange(Sender: TObject);
begin
  if EqualTimeCheckBox.Checked then
    begin
      OpponentMinEdit.Text := YouMinEdit.Text;
      OpponentIncEdit.Text := YouIncEdit.Text;
      OpponentUnlimitedCheckBox.Checked := YouUnlimitedCheckBox.Checked;
    end;
end;

procedure TGameOptionsForm.TrainingEnabledCheckBoxChange(Sender: TObject);
begin
  ExtBaseComboBox.Enabled := TrainingEnabledCheckBox.Checked;
  UsrBaseCheckBox.Enabled := TrainingEnabledCheckBox.Checked and (ExtBaseComboBox.ItemIndex <> 0);
  TakeBackCheckBox.Enabled := not TrainingEnabledCheckBox.Checked;
end;

procedure TGameOptionsForm.OpponentEditChange(Sender: TObject);
begin
  OpponentMinEdit.Text := IntToStr(OpponentMinUpDown.Position);
  OpponentIncEdit.Text := IntToStr(OpponentIncUpDown.Position);
  if EqualTimeCheckBox.Checked then
    begin
      YouMinEdit.Text := OpponentMinEdit.Text;
      YouIncEdit.Text := OpponentIncEdit.Text;
    end;
end;

procedure TGameOptionsForm.ExtBaseComboBoxChange(Sender: TObject);
begin
  UsrBaseCheckBox.Enabled := (ExtBaseComboBox.ItemIndex <> 0);
  if ExtBaseComboBox.ItemIndex = 0 then
    UsrBaseCheckBox.Checked := TRUE;
end;

procedure TGameOptionsForm.FormShow(Sender: TObject);
begin
  ExtBaseComboBoxChange(Sender);
end;

procedure TGameOptionsForm.UnlimitedCheckBoxChange(Sender: TObject);
begin
  YouMinUpDown.Enabled := not YouUnlimitedCheckBox.Checked;
  YouMinEdit.Enabled := not YouUnlimitedCheckBox.Checked;
  YouIncUpDown.Enabled := not YouUnlimitedCheckBox.Checked;
  YouIncEdit.Enabled := not YouUnlimitedCheckBox.Checked;

  OpponentMinUpDown.Enabled := not OpponentUnlimitedCheckBox.Checked;
  OpponentMinEdit.Enabled := not OpponentUnlimitedCheckBox.Checked;
  OpponentIncUpDown.Enabled := not OpponentUnlimitedCheckBox.Checked;
  OpponentIncEdit.Enabled := not OpponentUnlimitedCheckBox.Checked;

  if EqualTimeCheckBox.Checked then
    begin
      YouUnlimitedCheckBox.Checked := TCheckBox(Sender).Checked;
      OpponentUnlimitedCheckBox.Checked := TCheckBox(Sender).Checked;
    end;
end;

function TGameOptionsForm.GetModalID : TModalFormID;
begin
  Result := mfGameOptions;
end;


procedure TGameOptionsForm.FormCreate(Sender: TObject);
begin
  FLocalize;
end;


procedure TGameOptionsForm.FLocalize;
begin
  with TLocalizer.Instance do
  begin
    Caption := GetLabel(24);

    TimeControlGroupBox.Caption := GetLabel(25);
    EqualTimeCheckBox.Caption := GetLabel(26);
    YouGroupBox.Caption := GetLabel(27);
    OpponentGroupBox.Caption := GetLabel(28);
    YouUnlimitedCheckBox.Caption := GetLabel(29);
    OpponentUnlimitedCheckBox.Caption := GetLabel(29);
    YouMinLabel.Caption := GetLabel(30);
    OpponentMinLabel.Caption := GetLabel(30);
    YouIncLabel.Caption := GetLabel(31);
    OpponentIncLabel.Caption := GetLabel(31);

    TrainingModeGroupBox.Caption := GetLabel(32);
    TrainingEnabledCheckBox.Caption := GetLabel(33);
    ExtBaseLabel.Caption := GetLabel(34);
    UsrBaseCheckBox.Caption := GetLabel(35);

    GamePauseCheckBox.Caption := GetLabel(36);
    GameAdjournCheckBox.Caption := GetLabel(37);
    TakeBackCheckBox.Caption := GetLabel(38);
    AutoFlagCheckBox.Caption := GetLabel(39);

    OkButton.Caption := GetLabel(11);
    CancelButton.Caption := GetLabel(12);
  end;
end;


initialization
  {$i GameOptionsUnit.lrs}

end.
