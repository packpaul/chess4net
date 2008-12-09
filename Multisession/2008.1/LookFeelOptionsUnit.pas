unit LookFeelOptionsUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,
  ModalForm;

type
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

{$R *.dfm}

class function TLookFeelOptionsForm. GetModalID: TModalFormID;
begin
  Result := mfLookFeel;
end;

end.
