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
