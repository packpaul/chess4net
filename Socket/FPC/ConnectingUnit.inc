
{$MODE Delphi}

interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  ExtCtrls, StdCtrls, Buttons,
  { Dialogs, // -5 DialogUnit, } ModalForm, LResources;

type
  TConnectingHandler = procedure of object;

  TConnectingForm = class(TModalForm)
    AbortButton: TButton;
    ConnectingLabel: TLabel;
    ConnectingImage: TImage;
    procedure AbortButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
//  dlgOwner: TDialogs;
    ConnectingHandler: TConnectingHandler;
    shuted: boolean;
  public
    procedure Shut;
    class function GetModalID : TModalFormID; override;
    constructor Create(Owner: TForm; h: TConnectingHandler = nil); reintroduce; overload;
//    constructor Create(dlgOwner: TDialogs; h: TConnectingHandler); reintroduce; overload;
  end;

var
  ConnectingForm: TConnectingForm;

implementation


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
  FormStyle := Owner.FormStyle;
  inherited Create(Owner);
{$IFDEF LCLgtk2}
  with Constraints do
    begin
      MinWidth := Width;
      MaxWidth := Width;
      MinHeight := Height;
      MaxHeight := Height;
    end;
{$ENDIF}
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

initialization
  {$i ConnectingUnit.lrs}

end.
