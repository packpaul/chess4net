////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

{$MODE Delphi}

interface

uses
  LCLIntf, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls{, Mask}, LResources, Buttons, MaskEdit;

type

  { TConnectionForm }

  TConnectionForm = class(TForm)
    ClientRadioButton: TRadioButton;
    GroupBox1: TGroupBox;
    NickLabel: TLabel;
    NickEdit: TEdit;
    OKButton: TButton;
    CancelButton: TButton;
    IPEdit: TEdit;
    IPLabel: TLabel;
    PortLabel: TLabel;
    PortEdit: TMaskEdit;
    ServerRadioButton: TRadioButton;
    procedure ClientRadioButtonChange(Sender: TObject);
    procedure ServerRadioButtonChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure NickEditExit(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure IPEditChange(Sender: TObject);
    procedure PortEditExit(Sender: TObject);
  public
    function GetPort: word;
    constructor Create(Owner: TComponent); reintroduce;
  end;

implementation

uses
  GlobalsSocketUnit;

procedure TConnectionForm.FormCreate(Sender: TObject);
var
  frmOwner: TForm;
begin
  frmOwner := (Owner as TForm);
  Left:= frmOwner.Left + (frmOwner.Width - Width) div 2;
  Top:= frmOwner.Top + (frmOwner.Height - Height) div 2;
end;

procedure TConnectionForm.ClientRadioButtonChange(Sender: TObject);
begin
  IPEdit.Enabled:= TRUE;
  if IPEdit.Text <> '' then
    OKButton.Enabled:= TRUE
  else
    OKButton.Enabled:= FALSE;
end;

procedure TConnectionForm.ServerRadioButtonChange(Sender: TObject);
begin
  IPEdit.Enabled := FALSE;
  OKButton.Enabled := TRUE;
end;

procedure TConnectionForm.PortEditExit(Sender: TObject);
begin
  PortEdit.Text := IntToStr(GetPort);
end;

procedure TConnectionForm.NickEditExit(Sender: TObject);
begin
  if NickEdit.Text = '' then NickEdit.Text:= 'NN';
end;

procedure TConnectionForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then ModalResult:= mrOk;
end;

procedure TConnectionForm.IPEditChange(Sender: TObject);
begin
  if IPEdit.Text <> '' then
    OKButton.Enabled:= TRUE
  else
    OKButton.Enabled:= FALSE;
end;

constructor TConnectionForm.Create(Owner: TComponent);
begin
  FormStyle := (Owner as TForm).FormStyle;
  inherited;
{$IFDEF LCLgtk2}
  with Constraints do
    begin
      MinWidth := Width;
      MaxWidth := Width + 1;
      MinHeight := Height;
      MaxHeight := Height + 1;
    end;
{$ENDIF}
  PortEdit.Text := IntToStr(DEFAULT_PORT);
end;

function TConnectionForm.GetPort: word;
var
  port: integer;
begin
  try
    port := StrToInt(Trim(PortEdit.Text));
    if (port > 0) and (port <= $FFFF) then
      Result := port
    else
      Result := DEFAULT_PORT
  except
    on EConvertError do
      Result := DEFAULT_PORT;
  end;
end;

initialization
  {$i ConnectionUnit.lrs}

end.
