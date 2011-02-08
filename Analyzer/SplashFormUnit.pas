unit SplashFormUnit;

interface

uses
  Forms, Classes, Controls, StdCtrls, ExtCtrls;

type
  TSplashForm = class(TForm)
    ShowingTimer: TTimer;
    StaticText2: TStaticText;
    StaticText3: TStaticText;
    VerLabel: TLabel;
    StaticText1: TStaticText;
    procedure ShowingTimerTimer(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    procedure Splash;
    procedure WaitSplashShowing;
  end;

implementation

uses
  SysUtils,
  //
  GlobalsLocalUnit;

{$R *.dfm}

procedure TSplashForm.Splash;
begin
{$IFDEF RELEASE}
  Show;
  ShowingTimer.Enabled := TRUE;
  Update;
{$ENDIF}
end;


procedure TSplashForm.WaitSplashShowing;
begin
  while ShowingTimer.Enabled do
  begin
    Sleep(100);
    Application.ProcessMessages;
  end;
end;


procedure TSplashForm.ShowingTimerTimer(Sender: TObject);
begin
  Hide;
end;

procedure TSplashForm.FormHide(Sender: TObject);
begin
  ShowingTimer.Enabled := FALSE;
end;

procedure TSplashForm.FormCreate(Sender: TObject);
begin
  VerLabel.Caption := Format('ver. %s', [CHESS4NET_VERSION_TXT]);
end;

end.
