library Chess4Net_MI;
{*******************************
  plugin library for Miranda
********************************}

(*
{$IFDEF FASTMM4}
  FastMM4,
{$ENDIF}
*)

uses
{$IFDEF FASTMM4}
  FastMM4,
{$ENDIF}
  Forms,
  SysUtils,
  Windows,
  BitmapResUnit in '..\BitmapResUnit.pas',
  ChessBoardHeaderUnit in '..\ChessBoardHeaderUnit.pas',
  ChessBoardUnit in '..\ChessBoardUnit.pas' {ChessBoard},
  ConnectingUnit in '..\ConnectingUnit.pas' {ConnectingForm},
  ConnectorUnit in 'ConnectorUnit.pas',
  ContinueUnit in '..\ContinueUnit.pas' {ContinueForm},
  ControlUnit in 'ControlUnit.pas',
  DialogUnit in '..\DialogUnit.pas',
  GameOptionsUnit in '..\GameOptionsUnit.pas' {GameOptionsForm},
  GameLogUnit in '..\GameLogUnit.pas' {GameLogForm},
  GlobalsLocalUnit in 'GlobalsLocalUnit.pas',
  GlobalsUnit in '..\GlobalsUnit.pas',
  InfoUnit in '..\InfoUnit.pas' {InfoForm},
  LookFeelOptionsUnit in '..\LookFeelOptionsUnit.pas' {OptionsForm},
  ManagerUnit in '..\ManagerUnit.pas' {Manager},
  MessageDialogUnit in '..\MessageDialogUnit.pas',
  ModalForm in '..\ModalForm.pas',
  PluginCommonUnit in 'PluginCommonUnit.pas',
  PosBaseChessBoardUnit in '..\PosBaseChessBoardUnit.pas',
  PosBaseUnit in '..\PosBaseUnit.pas',
  PromotionUnit in '..\PromotionUnit.pas' {PromotionForm};

{$R ..\Chess4Net.res}

begin
  ControlUnit.PLUGIN_NAME := 'Chess4Net';
  PLUGIN_MENU_NAME := '&Chess4Net';

  with _PluginInfo^ do
    begin
      shortName := 'Chess4Net';
      version := MakeMirandaPluginVersion(200,9,1,0); // 2009.1
      description := PLUGIN_PLAYING_VIA;
      author := 'Pavel Perminov';
      authorEmail := 'packpaul@mail.ru';
      copyright := '(c) 2007-2009 No Copyrights';
      homepage := 'http://www.chess4net.ru';
    end;

  guidPlugin := StringToGUID('{BF17C6E3-C52C-4CB8-88ED-E0FC5F5D566A}');
  miidPlugin := StringToGUID('{09A68681-841C-4728-9F33-07A9F1E976A6}'); // interface

  MirandaPluginMenuPosition := $7FFFFFFF; // or < $7FFFFFFF

  gCreatePluginInstance    := CreatePluginInstance;
  gInitializeControls      := InitializeControls;
  gDeinitializeControls    := DeinitializeControls;
  gErrorDuringPluginStart  := ErrorDuringPluginStart;
end.
