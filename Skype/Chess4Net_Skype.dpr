program Chess4Net_Skype;

(*
{$IFDEF FASTMM4}
  FastMM4,
{$ENDIF}
{$IFDEF TESTING}
  , SkypeTS_TLB in 'SkypeTS_TLB.pas'
{$ELSE}
  {$IFDEF SKYPE_API}
  , SkypeAPI_Skype in '.\SkypeAPI\skypeapi_skype.pas'
  {$ELSE SKYPE4COM}
  , SKYPE4COMLib_TLB in '.\Skype4COM\SKYPE4COMLib_TLB.pas'
  {$ENDIF}
{$ENDIF}
*)

uses
{$IFDEF FASTMM4}
  FastMM4,
{$ENDIF}
  Forms,
  ConnectingUnit in 'ConnectingUnit.pas' {ConnectingForm},
  GameOptionsUnit in '..\GameOptionsUnit.pas' {GameOptionsForm},
  ManagerUnit in '..\ManagerUnit.pas' {Manager},
  ManagerUnit.Skype in 'ManagerUnit.Skype.pas',  
  ChessBoardHeaderUnit in '..\ChessBoardHeaderUnit.pas',
  ChessBoardUnit in '..\ChessBoardUnit.pas' {ChessBoard},
  ChessClockUnit in '..\ChessClockUnit.pas',
  PosBaseChessBoardUnit in '..\PosBaseChessBoardUnit.pas',
  GameChessBoardUnit in '..\GameChessBoardUnit.pas' {GameChessBoard},
  PromotionUnit in '..\PromotionUnit.pas' {PromotionForm},
  LookFeelOptionsUnit in '..\LookFeelOptionsUnit.pas' {OptionsForm},
  PosBaseUnit in '..\PosBaseUnit.pas',
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
  InfoUnit in '..\InfoUnit.pas' {InfoForm},
  SelectSkypeContactUnit in 'SelectSkypeContactUnit.pas' {SelectSkypeContactForm},
  CreditsFormUnit in 'CreditsFormUnit.pas' {CreditsForm},
  InterProc in 'InterProc.pas'
{$IFDEF TESTING}
  , SkypeTS_TLB in 'SkypeTS_TLB.pas'
{$ELSE}
  {$IFDEF SKYPE_API}
  , SkypeAPI_Skype in '.\SkypeAPI\skypeapi_skype.pas'
  {$ELSE SKYPE4COM}
  , SKYPE4COMLib_TLB in '.\Skype4COM\SKYPE4COMLib_TLB.pas'
  {$ENDIF}
{$ENDIF}
  ;

{$R ..\Chess4Net.res}

var
  Manager: TManagerSkype;

begin
  Forms.Application.Initialize;
  Forms.Application.Title := 'Chess4Net [Skype]';
{$IFNDEF TESTING}
  if (ActivateApplicationIfRunning) then
    exit;
{$ENDIF}
  Forms.Application.CreateForm(TManagerSkype, Manager);
  Forms.Application.ShowMainForm := False;
  Forms.Application.Run;
end.

