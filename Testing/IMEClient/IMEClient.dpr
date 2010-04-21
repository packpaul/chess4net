program IMEClient;

uses
  Forms,
  IMEClient.MainForm in 'IMEClient.MainForm.pas' {MainForm};

{$R *.res}

var
  MainForm: TMainForm;

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
