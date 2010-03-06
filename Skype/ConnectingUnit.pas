unit ConnectingUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, TntStdCtrls,
  Forms, {Dialogs, }ExtCtrls, StdCtrls,
  DialogUnit, ModalForm;

type
  TConnectingHandler = procedure of object;

  TConnectingForm = class(TModalForm)
    AbortButton: TTntButton;
    ConnectingLabel: TTntLabel;
    ConnectingImage: TImage;
    Bevel1: TBevel;
    SkypeAcceptImage: TImage;
    procedure AbortButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
//  dlgOwner: TDialogs;
    ConnectingHandler: TConnectingHandler;
    shuted: boolean;
    m_bShowSkypeAcceptLogo: boolean;
    procedure FLocalize;
    procedure FSetShowSkypeAcceptLogo(bValue: boolean);

  public
    procedure Shut;
    class function GetModalID : TModalFormID; override;
    constructor Create(Owner: TForm; h: TConnectingHandler = nil); reintroduce; overload;
//    constructor Create(dlgOwner: TDialogs; h: TConnectingHandler); reintroduce; overload;
    property ShowSkypeAcceptLogo: boolean read m_bShowSkypeAcceptLogo write FSetShowSkypeAcceptLogo;
  end;

var
  ConnectingForm: TConnectingForm;

implementation

{$R *.dfm}

uses
  LocalizerUnit;

////////////////////////////////////////////////////////////////////////////////
// TConnectiongForm

procedure TConnectingForm.AbortButtonClick(Sender: TObject);
begin
  Close;
end;


procedure TConnectingForm.FormShow(Sender: TObject);
var
  frmOwner: TForm;
begin
  frmOwner := (Owner as TForm);
  Left:= frmOwner.Left + (frmOwner.Width - Width) div 2;
  Top:= frmOwner.Top + (frmOwner.Height - Height) div 2;
end;

constructor TConnectingForm.Create(Owner: TForm; h: TConnectingHandler = nil);
begin
  self.FormStyle := Owner.FormStyle;
  inherited Create(Owner);
  shuted := FALSE;
  ConnectingHandler := h;
end;


procedure TConnectingForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if not shuted then
    begin
      ModalResult := AbortButton.ModalResult;
      if Assigned(ConnectingHandler) then
        ConnectingHandler;
    end
  else
    ModalResult := mrNone;
end;


procedure TConnectingForm.Shut;
begin
  shuted:= TRUE;
  Close;
end;

class function TConnectingForm.GetModalID: TModalFormID;
begin
  Result := mfConnecting;
end;


procedure TConnectingForm.FLocalize;
begin
  with TLocalizer.Instance do
  begin
    Caption := GetLabel(19);
    ConnectingLabel.Caption := GetLabel(20);
    AbortButton.Caption := GetLabel(21);
  end;
end;


procedure TConnectingForm.FormCreate(Sender: TObject);
begin
  m_bShowSkypeAcceptLogo := FALSE;
  ShowSkypeAcceptLogo := TRUE;
  FLocalize;
end;


procedure TConnectingForm.FSetShowSkypeAcceptLogo(bValue: boolean);
begin
  if (m_bShowSkypeAcceptLogo = bValue) then
    exit;

  m_bShowSkypeAcceptLogo := bValue;
  if (m_bShowSkypeAcceptLogo) then
  begin
    Height := 202;
    Bevel1.Visible := TRUE;
  end
  else
  begin
    Height := 119;
    Bevel1.Visible := FALSE;
  end;
end;

end.
