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
    GroupBox1: TGroupBox;
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
    GroupBox2: TGroupBox;
    TrainingEnabledCheckBox: TCheckBox;
    ExtBaseComboBox: TComboBox;
    UsrBaseCheckBox: TCheckBox;
    ExtBaseLabel: TLabel;
    GamePauseCheckBox: TCheckBox;
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

  public
    function GetModalID: TModalFormID; override;
  end;

implementation

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

initialization
  {$i GameOptionsUnit.lrs}

end.
