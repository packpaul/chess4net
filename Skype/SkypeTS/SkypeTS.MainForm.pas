unit SkypeTS.MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TntStdCtrls, ExtCtrls,
  SkypeTS.Skype;

type
  TMainForm = class(TForm)
    Skype1GroupBox: TGroupBox;
    Handle1Edit: TTntEdit;
    Handle1Label: TLabel;
    FullName1Edit: TTntEdit;
    FullName1Label: TLabel;
    DisplayName1Label: TLabel;
    DisplayName1Edit: TTntEdit;
    Attach1Button: TButton;
    SendIM1Edit: TTntEdit;
    Bevel1: TBevel;
    SendIM1Button: TButton;
    IM1Memo: TTntMemo;
    IM1Label: TLabel;
    Skype2GroupBox: TGroupBox;
    Handle2Edit: TTntEdit;
    Handle2Label: TLabel;
    FullName2Edit: TTntEdit;
    FullName2Label: TLabel;
    DisplayName2Label: TLabel;
    DisplayName2Edit: TTntEdit;
    Attach2Button: TButton;
    SendIM2Edit: TTntEdit;
    Bevel2: TBevel;
    SendIM2Button: TButton;
    IM2Memo: TTntMemo;
    IM2Label: TLabel;
    procedure FormShow(Sender: TObject);
    procedure Attach1ButtonClick(Sender: TObject);
    procedure Attach2ButtonClick(Sender: TObject);
    procedure SendIM1ButtonClick(Sender: TObject);
    procedure SendIM2ButtonClick(Sender: TObject);

  private
    function FGetSkype(wID: word): TSkype;
    procedure FOnInstantMessageReceived(Sender: TObject; const wstrMessage: WideString);
    property Skypes[wID: word]: TSkype read FGetSkype;
  public
    procedure UpdateGUI;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

////////////////////////////////////////////////////////////////////////////////
// TMainForm

procedure TMainForm.UpdateGUI;
var
  bAvailable1, bAvailable2, bEnabled: boolean;
  i: integer;
  Skype1, Skype2: TSkype;
begin // TMainForm.UpdateGUI
  // Skype1
  Skype1 := Skypes[0];
  bAvailable1 := Assigned(Skype1);
  Skype1GroupBox.Enabled := bAvailable1;
  for i := 0 to Skype1GroupBox.ControlCount - 1 do
    Skype1GroupBox.Controls[i].Enabled := bAvailable1;
  if (bAvailable1) then
  begin
    Attach1Button.Enabled := (not (sAttached in Skype1.States));
    Handle1Edit.ReadOnly := (sAttached in Skype1.States);
    FullName1Edit.ReadOnly := (sAttached in Skype1.States);
    DisplayName1Edit.ReadOnly := (sAttached in Skype1.States);
    Skype1.OnInstantMessageReceived := FOnInstantMessageReceived;
  end;

  // Skype2
  Skype2 := Skypes[1];
  bAvailable2 := Assigned(Skype2);
  Skype2GroupBox.Enabled := bAvailable2;
  for i := 0 to Skype2GroupBox.ControlCount - 1 do
    Skype2GroupBox.Controls[i].Enabled := bAvailable2;
  if (bAvailable2) then
  begin
    Attach2Button.Enabled := (not (sAttached in Skype2.States));
    Handle2Edit.ReadOnly := (sAttached in Skype2.States);
    FullName2Edit.ReadOnly := (sAttached in Skype2.States);
    DisplayName2Edit.ReadOnly := (sAttached in Skype2.States);
    Skype2.OnInstantMessageReceived := FOnInstantMessageReceived;
  end;

  bEnabled := (bAvailable1 and bAvailable2 and
    (sAttached in Skype1.States) and (sAttached in Skype2.States));
  SendIM1Button.Enabled := bEnabled;
  SendIM1Edit.Enabled := bEnabled;
  SendIM2Button.Enabled := bEnabled;
  SendIM2Edit.Enabled := bEnabled;
end;


procedure TMainForm.FormShow(Sender: TObject);
begin
  UpdateGUI;
end;


function TMainForm.FGetSkype(wID: word): TSkype;
begin
  if (Length(g_arrSkypes) > wID) then
    Result := g_arrSkypes[wID]
  else
    Result := nil;
end;


procedure TMainForm.Attach1ButtonClick(Sender: TObject);
begin
  if (Assigned(Skypes[0])) then
    Skypes[0].DoAttach(Handle1Edit.Text, FullName1Edit.Text, DisplayName1Edit.Text);
end;


procedure TMainForm.Attach2ButtonClick(Sender: TObject);
begin
  if (Assigned(Skypes[1])) then
    Skypes[1].DoAttach(Handle2Edit.Text, FullName2Edit.Text, DisplayName2Edit.Text);
end;


procedure TMainForm.SendIM1ButtonClick(Sender: TObject);
begin
  if (SendIM1Edit.Text <> '') then
  begin
    Skypes[0].SendInstantMessge(Skypes[1].CurrentUserHandle, SendIM1Edit.Text);
    IM1Memo.Lines.Add('> ' + SendIM1Edit.Text);
    SendIM1Edit.Text := '';
  end;
end;


procedure TMainForm.SendIM2ButtonClick(Sender: TObject);
begin
  if (SendIM2Edit.Text <> '') then
  begin
    Skypes[1].SendInstantMessge(Skypes[0].CurrentUserHandle, SendIM2Edit.Text);
    IM2Memo.Lines.Add('> ' + SendIM2Edit.Text);
    SendIM2Edit.Text := '';
  end;
end;


procedure TMainForm.FOnInstantMessageReceived(Sender: TObject; const wstrMessage: WideString);
var
  Skype: TSkype;
begin
  Skype := Sender as TSkype;

  if (Skype.ID = Skypes[0].ID) then
    IM1Memo.Lines.Add('< ' + wstrMessage)
  else if (Skype.ID = Skypes[1].ID) then
    IM2Memo.Lines.Add('< ' + wstrMessage);
end;

end.
