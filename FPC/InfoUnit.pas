////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit InfoUnit;

interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, LResources,
  //
  ModalForm;

type
  TInfoForm = class(TModalForm)
    OkButton: TButton;
    PluginNameLabel: TLabel;
    PlayingViaLabel: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    URLLabel: TLabel;
    EMailLabel: TLabel;
    procedure OkButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EMailLabelClick(Sender: TObject);
    procedure URLLabelClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  end;

implementation

uses
{$IFDEF WINDOWS}
  Windows, ShellAPI,
{$ENDIF}
  GlobalsUnit, GlobalsSkypeUnit;

procedure TInfoForm.OkButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TInfoForm.FormCreate(Sender: TObject);
begin
  Caption := DIALOG_CAPTION;

  PlayingViaLabel.Caption := PLUGIN_PLAYING_OVER;
  PluginNameLabel.Caption := PLUGIN_INFO_NAME;
  URLLabel.Caption := PLUGIN_URL;
  EMailLabel.Caption := PLUGIN_EMAIL;
end;

procedure TInfoForm.URLLabelClick(Sender: TObject);
begin
{$IFDEF WINDOWS}
  ShellExecute(Handle, nil, PChar(URLLabel.Caption), nil, nil, SW_SHOWNORMAL);
{$ENDIF}
end;

procedure TInfoForm.EMailLabelClick(Sender: TObject);
var
  shellStr: string;
begin
  shellStr := 'mailto:' + EMailLabel.Caption;
{$IFDEF WINDOWS}
  ShellExecute(Handle, nil, PChar(shellStr), nil, nil, SW_SHOWNORMAL);
{$ENDIF}
end;

procedure TInfoForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

initialization
  {$i InfoUnit.lrs}

end.
