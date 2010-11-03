
{$MODE Delphi}

interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  ExtCtrls, StdCtrls, Buttons,
  //
  ModalForm, LResources;

type
  TConnectingHandler = procedure of object;

  TConnectingForm = class(TModalForm)
    AbortButton: TButton;
    ConnectingLabel: TLabel;
    ConnectingImage: TImage;
    procedure AbortButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    ConnectingHandler: TConnectingHandler;
    shuted: boolean;
    function FGetOnShow: TNotifyEvent;
    procedure FSetOnShow(Value: TNotifyEvent);
  public
    procedure Shut;
    class function GetModalID : TModalFormID; override;
    constructor Create(Owner: TForm; h: TConnectingHandler = nil); reintroduce; overload;
    property OnShow: TNotifyEvent read FGetOnShow write FSetOnShow;
  end;

var
  ConnectingForm: TConnectingForm;

implementation


procedure TConnectingForm.AbortButtonClick(Sender: TObject);
begin
  Close;
end;


constructor TConnectingForm.Create(Owner: TForm; h: TConnectingHandler = nil);
begin
  FormStyle := Owner.FormStyle;
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


function TConnectingForm.FGetOnShow: TNotifyEvent;
begin
  Result := inherited OnShow;
end;


procedure TConnectingForm.FSetOnShow(Value: TNotifyEvent);
begin
  if (not Assigned(Value)) then
    exit;
  Assert(not Assigned(inherited OnShow));
  inherited OnShow := Value;
end;

class function TConnectingForm.GetModalID: TModalFormID;
begin
  Result := mfConnecting;
end;

initialization
  {$i ConnectingUnit.lrs}

end.
