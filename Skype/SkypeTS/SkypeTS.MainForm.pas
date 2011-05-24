////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit SkypeTS.MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TntStdCtrls, ExtCtrls,
  //
  SkypeTS.Skype, SkypeTS.SkypeFrame;

type
  TMainForm = class(TForm)
    SkypeFrame1: TSkypeFrame;
    SkypeFrame2: TSkypeFrame;
    SkypeFrame3: TSkypeFrame;
    SkypeFrame4: TSkypeFrame;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);

  private
    function FGetSkypeByID(wID: word): TSkype;
    procedure FOnInstantMessageReceived(Sender: TObject; const wstrMessage: WideString);

    procedure FOnSkypeAttach(Sender: TObject);
    procedure FOnIMSend(Sender: TObject; const wstrMessage: WideString);

    function FGetSkypeFrame(Skype: TSkype): TSkypeFrame;

    property Skypes[wID: word]: TSkype read FGetSkypeByID;

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
  i: integer;
  SkypeFrame: TSkypeFrame;
  Skype: TSkype;
  iEnabledCount: integer;
begin // TMainForm.UpdateGUI
  iEnabledCount := 0;

  for i := 0 to ControlCount - 1 do
  begin
    if (not (Controls[i] is TSkypeFrame)) then
      continue;
    SkypeFrame := TSkypeFrame(Controls[i]);
    Skype := Skypes[SkypeFrame.Tag];
    if (Assigned(Skype)) then
    begin
      Skype.OnInstantMessageReceived := FOnInstantMessageReceived;
      if (ssAttached in Skype.States) then
        inc(iEnabledCount);
    end;
  end;

  for i := 0 to ControlCount - 1 do
  begin
    if (not (Controls[i] is TSkypeFrame)) then
      continue;
    SkypeFrame := TSkypeFrame(Controls[i]);
    Skype := Skypes[SkypeFrame.Tag];
    SkypeFrame.UpdateGUI(Skype, (iEnabledCount >= 2));
  end;
end;


procedure TMainForm.FormShow(Sender: TObject);
begin
  UpdateGUI;
end;


function TMainForm.FGetSkypeByID(wID: word): TSkype;
begin
  Result := TSkype.GetSkypeByID(wID);
end;


procedure TMainForm.FOnInstantMessageReceived(Sender: TObject; const wstrMessage: WideString);
var
  SkypeFrame: TSkypeFrame;
begin
  SkypeFrame := FGetSkypeFrame(Sender as TSkype);
  Assert(Assigned(SkypeFrame));

  SkypeFrame.IMLogAdd('< ' + wstrMessage);
end;


function TMainForm.FGetSkypeFrame(Skype: TSkype): TSkypeFrame;
var
  i: integer;
begin
  for i := 0 to ControlCount - 1 do
  begin
    if (not (Controls[i] is TSkypeFrame)) then
      continue;
    Result := TSkypeFrame(Controls[i]);
    if (Skypes[Result.Tag].ID = Skype.ID) then
      exit;
  end;
  Result := nil;
end;


procedure TMainForm.FormCreate(Sender: TObject);
var
  i: integer;
  SkypeFrame: TSkypeFrame;
  strlContacts: TStringList;
begin
  strlContacts := TStringList.Create;
  try
    for i := 0 to ControlCount - 1 do
    begin
      if (not (Controls[i] is TSkypeFrame)) then
        continue;
      SkypeFrame := TSkypeFrame(Controls[i]);
      SkypeFrame.OnAttach := FOnSkypeAttach;
      SkypeFrame.OnIMSend := FOnIMSend;
      strlContacts.AddObject(Format('Skype%d', [Succ(SkypeFrame.Tag)]),
        TObject(SkypeFrame.Tag));
    end;

    strlContacts.Sort;

    for i := 0 to ControlCount - 1 do
    begin
      if (not (Controls[i] is TSkypeFrame)) then
        continue;
      SkypeFrame := TSkypeFrame(Controls[i]);
      SkypeFrame.SetContacts(strlContacts);
    end;

  finally
    strlContacts.Free;
  end;
end;


procedure TMainForm.FOnSkypeAttach(Sender: TObject);
var
  Skype: TSkype;
begin
  with Sender as TSkypeFrame do
  begin
    Skype := Skypes[Tag];

    if (Assigned(Skype)) then
      Skype.DoAttach(Handle, FullName, DisplayName);
  end;
end;


procedure TMainForm.FOnIMSend(Sender: TObject; const wstrMessage: WideString);
var
  Skype, SkypeTo: TSkype;
begin
  with Sender as TSkypeFrame do
  begin
    Skype := Skypes[Tag];
    SkypeTo := Skypes[ContactID];
    Skype.SendInstantMessge(SkypeTo.CurrentUserHandle, wstrMessage);
    IMLogAdd('> ' + wstrMessage);
  end;
end;

end.
