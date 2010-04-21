program IMEServer;

uses
  Forms,
  IMEServer.MainForm in 'IMEServer.MainForm.pas' {MainForm};

{$R *.res}

var
  MainForm: TMainForm;

begin
  Application.Initialize;
  Application.Title := 'IMEServer';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
