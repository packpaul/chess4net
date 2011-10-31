////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit AnalysisModeSelectionFormUnit;

interface

uses
  Forms, Classes, Controls, StdCtrls, ExtCtrls;

type
  TReplyMoveSelection = (rmsFirstMoveLine, rmsRandomMoveTreeWeight, rmsRandomMove);

  TAnalysisModeSelectionForm = class(TForm)
    CancelButton: TButton;
    OKButton: TButton;
    ReplyMoveRadioGroup: TRadioGroup;
  private
    function FGetReplyMoveSelection: TReplyMoveSelection;
    procedure FSetReplyMoveSelection(Value: TReplyMoveSelection);
  public
    constructor Create(AReplyMoveSelection: TReplyMoveSelection); reintroduce;
    property ReplyMoveSelection: TReplyMoveSelection read FGetReplyMoveSelection;
  end;

implementation

{$R *.dfm}

////////////////////////////////////////////////////////////////////////////////
// TAnalysisModeSelectionForm

constructor TAnalysisModeSelectionForm.Create(AReplyMoveSelection: TReplyMoveSelection);
begin
  inherited Create(Application);
  FSetReplyMoveSelection(AReplyMoveSelection);
end;


function TAnalysisModeSelectionForm.FGetReplyMoveSelection: TReplyMoveSelection;
begin
  Result := TReplyMoveSelection(ReplyMoveRadioGroup.ItemIndex);
end;


procedure TAnalysisModeSelectionForm.FSetReplyMoveSelection(Value: TReplyMoveSelection);
begin
  ReplyMoveRadioGroup.ItemIndex := Ord(Value);
end;

end.
