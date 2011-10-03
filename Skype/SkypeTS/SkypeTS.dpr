////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

program SkypeTS;

uses
  Forms,
  SkypeTS.MainForm in 'SkypeTS.MainForm.pas' {MainForm},
  SkypeTS_TLB in 'SkypeTS_TLB.pas' {Skype: CoClass},
  SkypeTS.Skype in 'SkypeTS.Skype.pas',
  SkypeTS.SkypeFrame in 'SkypeTS.SkypeFrame.pas' {SkypeFrame: TFrame};

{$R *.TLB}

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
