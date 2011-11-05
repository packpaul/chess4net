unit TipsOfTheDayFormUnit;

interface

uses
  StdCtrls, TntStdCtrls, Classes, Controls,
  //
  FloatingFormsUnit;
type
  TTipsOfTheDayForm = class(TChildFloatingForm)
    PrevTipButton: TButton;
    NextTipButton: TButton;
    CloseButton: TButton;
    TipMemo: TTntMemo;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

////////////////////////////////////////////////////////////////////////////////
// TTipsOfTheDayForm

end.
