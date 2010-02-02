program Chess4Net_Skype;

(*
{$IFDEF FASTMM4}
  FastMM4,
{$ENDIF}
{$IFDEF TESTING}
  , SkypeTS_TLB in 'SkypeTS_TLB.pas'
{$ELSE}
  , SKYPE4COMLib_TLB in '.\Skype4COM\SKYPE4COMLib_TLB.pas'
{$ENDIF}
*)

uses
{$IFDEF FASTMM4}
  FastMM4,
{$ENDIF}
  Forms,
  ConnectingUnit in '..\ConnectingUnit.pas' {ConnectingForm},
  GameOptionsUnit in '..\GameOptionsUnit.pas' {GameOptionsForm},
  ManagerUnit in '..\ManagerUnit.pas' {Manager},
  ChessBoardHeaderUnit in '..\ChessBoardHeaderUnit.pas',
  ChessBoardUnit in '..\ChessBoardUnit.pas' {ChessBoard},
  PromotionUnit in '..\PromotionUnit.pas' {PromotionForm},
  LookFeelOptionsUnit in '..\LookFeelOptionsUnit.pas' {OptionsForm},
  PosBaseUnit in '..\PosBaseUnit.pas',
  PosBaseChessBoardUnit in '..\PosBaseChessBoardUnit.pas',
  ModalForm in '..\ModalForm.pas',
  GlobalsLocalUnit in 'GlobalsLocalUnit.pas',
  GlobalsUnit in '..\GlobalsUnit.pas',
  ContinueUnit in '..\ContinueUnit.pas' {ContinueForm},
  DialogUnit in '..\DialogUnit.pas',
  MessageDialogUnit in '..\MessageDialogUnit.pas',
  BitmapResUnit in '..\BitmapResUnit.pas',
  ChessRulesEngine in '..\ChessRulesEngine.pas',
  LocalizerUnit in '..\LocalizerUnit.pas',
  ConnectorUnit in 'ConnectorUnit.pas',
  InfoUnit in '..\InfoUnit.pas', {InfoForm}
  SelectSkypeContactUnit in 'SelectSkypeContactUnit.pas' {SelectSkypeContactForm}
{$IFDEF TESTING}
  , SkypeTS_TLB in 'SkypeTS_TLB.pas'
{$ELSE}
  , SKYPE4COMLib_TLB in '.\Skype4COM\SKYPE4COMLib_TLB.pas'
{$ENDIF}
  ;

{$R ..\Chess4Net.res}

var
  Manager: TManager;

begin
  Forms.Application.Initialize;
  Forms.Application.Title := 'Chess4Net';
  Forms.Application.CreateForm(TManager, Manager);
  Forms.Application.ShowMainForm := False;
  Forms.Application.Run;
end.

