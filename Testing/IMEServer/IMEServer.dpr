program IMEServer;

uses
  Forms,
  IMEServer.MainForm in 'IMEServer.MainForm.pas' {MainForm},
  IMEServer.Headers in 'IMEServer.Headers.pas',
  IMEServer.ModelModule in 'IMEServer.ModelModule.pas' {ModelModule: TDataModule};

{$R *.res}

var
  MainForm: TMainForm;
  ModelModule: TModelModule;

begin
  Application.Initialize;
  Application.Title := 'IMEServer';

  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TModelModule, ModelModule);

  ModelModule.SetView(MainForm);

  Application.Run;
end.
