program FormViewer;

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainFormUnit, InfoUnit, ModalForm, LocalizerUnit, CreditsFormUnit,
  LookFeelOptionsUnit, GameOptionsUnit;

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

