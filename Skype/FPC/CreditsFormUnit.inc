////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

{$MODE Delphi}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, LResources;

type
  TCreditsForm = class(TForm)
    CloseButton: TButton;
    CreditsLabel: TLabel;
    Chess4NetImage: TImage;
    Label1: TLabel;
    URLLabel: TLabel;
    cbDontShowAgain: TCheckBox;
    procedure CloseButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure URLLabelClick(Sender: TObject);
  private
    procedure FLocalize;
    function FGetDontShowAgain: boolean;
  public
    property DontShowAgain: boolean read FGetDontShowAgain;
  end;

implementation

uses
{$IFDEF WINDOWS}
  Windows, ShellAPI,
{$ENDIF}
  GlobalsUnit, LocalizerUnit;

////////////////////////////////////////////////////////////////////////////////
// TCreditsForm

procedure TCreditsForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;


procedure TCreditsForm.FormCreate(Sender: TObject);
begin
  Caption := DIALOG_CAPTION;
  URLLabel.Caption := PLUGIN_URL;
  FLocalize;
end;


procedure TCreditsForm.FLocalize;
begin
  with TLocalizer.Instance do
  begin
    CreditsLabel.Caption := GetLabel(64);
    CloseButton.Caption := GetLabel(65);
    cbDontShowAgain.Caption := GetLabel(66); 
  end;
end;


procedure TCreditsForm.URLLabelClick(Sender: TObject);
begin
{$IFDEF WINDOWS}
  ShellExecute(Handle, nil, PChar(URLLabel.Caption), nil, nil, SW_SHOWNORMAL);
  CloseButton.Click;
{$ENDIF}
end;


function TCreditsForm.FGetDontShowAgain: boolean;
begin
  Result := cbDontShowAgain.Checked;
end;

initialization
  {$i CreditsFormUnit.lrs}

end.
