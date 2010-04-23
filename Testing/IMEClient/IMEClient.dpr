program IMEClient;

uses
  Forms,
  IMEClient.MainForm in 'IMEClient.MainForm.pas' {MainForm},
  IMEClient.ModelModule in 'IMEClient.ModelModule.pas' {ModelModule: TDataModule},
  IMEClient.Headers in 'IMEClient.Headers.pas';

{$R *.res}

var
  MainForm: TMainForm;
  ModelModule: TModelModule;

begin
  Application.Initialize;
  Application.Title := 'IMEClient';

  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TModelModule, ModelModule);

  ModelModule.SetView(MainForm); 

  Application.Run;
end.
