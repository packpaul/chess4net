////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit SplashFormUnit;

interface

uses
  Forms, Classes, Controls, StdCtrls, ExtCtrls, Graphics, jpeg;

type
  TSplashForm = class(TForm)
    ShowingTimer: TTimer;
    VersionLabel: TLabel;
    SplashImage: TImage;
    CopyrightLabel: TLabel;
    URLLabel: TLabel;
    EMailLabel: TLabel;
    ByAuthorLabel: TLabel;
    procedure ShowingTimerTimer(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure URLLabelClick(Sender: TObject);
    procedure EMailLabelClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure DoClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    function FIsSplashing: boolean;
  public
    procedure Splash;
    procedure WaitSplashShowing;
  end;

implementation

uses
  SysUtils, ShellAPI, Windows,
  //
  GlobalsLocalUnit;

{$R *.dfm}

const
  LBL_VERSION = 'Version %s';
  LBL_BY_AUTHOR = 'by Pavel Perminov';

////////////////////////////////////////////////////////////////////////////////
// TSplashForm

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
  VersionLabel.Caption := Format(LBL_VERSION, [CHESS4NET_VERSION_TXT]);
  ByAuthorLabel.Caption := LBL_BY_AUTHOR;
  URLLabel.Caption := CHESS4NET_URL;
  EMailLabel.Caption := EMAIL_ADRESS;
  CopyrightLabel.Caption := COPYRIGHT_TXT;
end;


procedure TSplashForm.URLLabelClick(Sender: TObject);
begin
  ShellExecute(Handle, nil, PChar(URLLabel.Caption), nil, nil, SW_SHOWNORMAL);
  Close;
end;


procedure TSplashForm.EMailLabelClick(Sender: TObject);
var
  shellStr: string;
begin
  shellStr := 'mailto:' + EMailLabel.Caption;
  ShellExecute(Handle, nil, PChar(shellStr), nil, nil, SW_SHOWNORMAL);
  Close;
end;


procedure TSplashForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := (not FIsSplashing);
end;


function TSplashForm.FIsSplashing: boolean;
begin
  Result := ShowingTimer.Enabled; 
end;


procedure TSplashForm.DoClick(Sender: TObject);
begin
  Close;
end;


procedure TSplashForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  Key := 0;
  Close;
end;

end.
