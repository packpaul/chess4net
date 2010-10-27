
{$MODE Delphi}

interface

uses
  LCLIntf, SysUtils, Classes, Graphics, Controls, Forms,
  ExtCtrls, StdCtrls, Buttons,
  {Dialogs, // -5 DialogUnit,} ModalForm, LResources;

type
  TContinueHandler = procedure of object;

  TContinueForm = class(TModalForm)
    ContinueButton: TButton;
    ContinueLabel: TLabel;
    procedure ContinueButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
//  dlgOwner: TDialogs;
    ContinueHandler: TContinueHandler;
    shuted: boolean;
  public
    procedure Shut;
    class function GetModalID : TModalFormID; override;
    constructor Create(Owner: TForm; h: TContinueHandler = nil); reintroduce; overload;
//    constructor Create(dlgOwner: TDialogs; h: TContinueHandler); reintroduce; overload;
  end;


implementation

uses
  GlobalsUnit;


procedure TContinueForm.ContinueButtonClick(Sender: TObject);
begin
  Close;
end;


procedure TContinueForm.FormShow(Sender: TObject);
var
  frmOwner: TForm;
begin
  frmOwner := (Owner as TForm);
  Left:= frmOwner.Left + (frmOwner.Width - Width) div 2;
  Top:= frmOwner.Top + (frmOwner.Height - Height) div 2;
end;

constructor TContinueForm.Create(Owner: TForm; h: TContinueHandler = nil);
begin
  self.FormStyle := Owner.FormStyle;
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
  ContinueHandler := h;
end;


procedure TContinueForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if not shuted then
    begin
      ModalResult := ContinueButton.ModalResult;
      if Assigned(ContinueHandler) then
        ContinueHandler;
    end
  else
    ModalResult := mrNone;
end;


procedure TContinueForm.Shut;
begin
  shuted:= TRUE;
  Close;
end;

class function TContinueForm.GetModalID: TModalFormID;
begin
  Result := mfContinue;
end;

procedure TContinueForm.FormCreate(Sender: TObject);
begin
  Caption := DIALOG_CAPTION;
end;

initialization
  {$i ContinueUnit.lrs}

end.
