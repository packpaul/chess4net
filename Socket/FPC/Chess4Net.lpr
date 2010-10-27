program Chess4Net;

uses
{$IFDEF UNIX}
  cthreads,
{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, LResources, lnetvisual,
  { add your units here } ManagerSocketUnit, PromotionUnit,
  ConnectorSocketUnit, PosBaseUnit, PosBaseChessBoardUnit,
  GlobalsSocketUnit, GlobalsUnit, ModalForm, ChessBoardUnit, ConnectionUnit,
  GameOptionsUnit, LookFeelOptionsUnit, ContinueUnit, ConnectingUnit;

var
  Manager: TManager;

{$IFDEF WINDOWS}{$R Chess4Net.rc}{$ENDIF}

begin
  {$I Chess4Net.lrs}
  Application.Title := 'Chess4Net';
  Application.Initialize;
  Application.CreateForm(TManager, Manager);
  Application.ShowMainForm := False;
  Application.Run;
end.
