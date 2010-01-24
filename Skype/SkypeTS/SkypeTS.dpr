program SkypeTS;

uses
  Forms,
  SkypeTS.MainForm in 'SkypeTS.MainForm.pas' {Form1},
  SkypeTS_TLB in 'SkypeTS_TLB.pas' {Skype: CoClass},
  SkypeTS.Skype in 'SkypeTS.Skype.pas';

{$R *.TLB}

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
