
{$MODE Delphi}

interface

uses
  LCLIntf, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons,
  ModalForm, LResources;

type

  { TLookFeelOptionsForm }

  TLookFeelOptionsForm = class(TModalForm)
    OkButton: TButton;
    CancelButton: TButton;
    AnimationComboBox: TComboBox;
    AnimateLabel: TLabel;
    BoxPanel: TPanel;
    HilightLastMoveBox: TCheckBox;
    CoordinatesBox: TCheckBox;
    StayOnTopBox: TCheckBox;
    ExtraExitBox: TCheckBox;
  public
    class function GetModalID : TModalFormID; override;
  end;

implementation

class function TLookFeelOptionsForm.GetModalID: TModalFormID;
begin
  Result := mfLookFeel;
end;


initialization
  {$i LookFeelOptionsUnit.lrs}

end.
