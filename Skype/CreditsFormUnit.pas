unit CreditsFormUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, TntForms,
  Dialogs, StdCtrls, TntStdCtrls, ShellAPI, ExtCtrls;

type
  TCreditsForm = class(TTntForm)
    CloseButton: TTntButton;
    CreditsLabel: TTntLabel;
    Chess4NetImage: TImage;
    Label1: TLabel;
    URLLabel: TLabel;
    cbDontShowAgain: TTntCheckBox;
    URL2Label: TLabel;
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

{$R *.dfm}

uses
  GlobalsUnit, GlobalsLocalUnit, LocalizerUnit;

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
  URL2Label.Caption := CVETI_URL;
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
  ShellExecute(Handle, nil, PChar((Sender as TLabel).Caption), nil, nil, SW_SHOWNORMAL);
  CloseButton.Click;
end;


function TCreditsForm.FGetDontShowAgain: boolean;
begin
  Result := cbDontShowAgain.Checked;
end;

end.

