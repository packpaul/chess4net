unit InfoUnit;

interface

uses
   Forms, Controls, StdCtrls, Classes;

type
  TInfoForm = class(TForm)
    OkButton: TButton;
    ApplicationNameLabel: TLabel;
    DescriptionLabel: TLabel;
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
  end;

procedure ShowInfo;

implementation

{$R *.dfm}

uses
  ShellAPI, Windows,
  //
  GlobalsLocalUnit;

const
  APPLICATION_NAME_VERSION = 'Chess4Net Analyzer ' + CHESS4NET_VERSION_TXT;

procedure ShowInfo;
begin
  with TInfoForm.Create(Application) do
  try
    ShowModal;
  finally
    Free;
  end;
end;


procedure TInfoForm.OkButtonClick(Sender: TObject);
begin
  Close;
end;


procedure TInfoForm.FormCreate(Sender: TObject);
begin
  ApplicationNameLabel.Caption := APPLICATION_NAME_VERSION;
  URLLabel.Caption := CHESS4NET_URL;
  EMailLabel.Caption := EMAIL_ADRESS;
end;


procedure TInfoForm.URLLabelClick(Sender: TObject);
begin
  ShellExecute(Handle, nil, PChar(URLLabel.Caption), nil, nil, SW_SHOWNORMAL);
end;


procedure TInfoForm.EMailLabelClick(Sender: TObject);
var
  shellStr: string;
begin
  shellStr := 'mailto:' + EMailLabel.Caption;
  ShellExecute(Handle, nil, PChar(shellStr), nil, nil, SW_SHOWNORMAL);
end;

end.
