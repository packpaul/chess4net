program Chess4Net_Skype;

uses
{$IFDEF UNIX}
  cthreads,
{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Dialogs, LResources,
  { add your units here }
  UniqueInstanceRaw,
  ManagerUnit, ManagerSkypeUnit, PromotionUnit, PosBaseUnit,
  PosBaseChessBoardUnit, GlobalsSkypeUnit, ConnectorSkypeUnit, GlobalsUnit,
  ModalFormBase, ModalForm, LocalizerUnit, ChessBoardUnit,
  GameOptionsUnit, LookFeelOptionsUnit, ContinueUnit, ConnectingUnit,
  SelectSkypeContactUnit, CreditsFormUnit;

var
  Manager: TManager;

{$IFDEF WINDOWS}{$R Chess4Net_Skype.rc}{$ENDIF}

begin
  {$I Chess4Net_Skype.lrs}
  Application.Title := 'Chess4Net_Skype';
  Application.Initialize;

  if (InstanceRunning(SKYPE_APP_NAME)) then
  begin
    ShowMessage('You have already an instance of Chess4Net [Skype] running!');
    exit;
  end;

  Application.CreateForm(TManagerSkype, Manager);
  Application.ShowMainForm := False;
  Application.Run;
end.
