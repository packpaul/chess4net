program Chess4Net_Skype;

uses
{$IFDEF UNIX}
  cthreads,
{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  { add your units here }
  ManagerUnit, ManagerSkypeUnit, PromotionUnit,
  PosBaseUnit, PosBaseChessBoardUnit, GlobalsSkypeUnit, ConnectorSkypeUnit,
  GlobalsUnit, ModalFormBase, ModalForm, LocalizerUnit, ChessBoardUnit,
  GameOptionsUnit, LookFeelOptionsUnit, ContinueUnit, ConnectingUnit,
  SelectSkypeContactUnit;

var
  Manager: TManager;

{$IFDEF WINDOWS}{$R Chess4Net_Skype.rc}{$ENDIF}

begin
  {$I Chess4Net_Skype.lrs}
  Application.Title := 'Chess4Net_Skype';
  Application.Initialize;
  Application.CreateForm(TManagerSkype, Manager);
  Application.ShowMainForm := False;
  Application.Run;
end.
