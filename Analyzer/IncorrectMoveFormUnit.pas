unit IncorrectMoveFormUnit;

interface

uses
  Forms, Classes, Controls, StdCtrls;

type
  TIncorrectMoveModalResult = (immrTryAgain, immrShowHint);

  TIncorrectMoveForm = class(TForm)
    MsgLabel: TLabel;
    TryAgainButton: TButton;
    ShowHintButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ShowHintButtonClick(Sender: TObject);
    procedure TryAgainButtonClick(Sender: TObject);
  private
    m_ModalResult: TIncorrectMoveModalResult;
  public
    class function Show: TIncorrectMoveModalResult;
    property ModalResult: TIncorrectMoveModalResult read m_ModalResult;
  end;

implementation

{$R *.dfm}

const
  MSG_INCORRECT_MOVE = 'Incorrect move!';

  LBL_SHOW_HINT = 'Show Hint';
  LBL_TRY_AGAIN = 'Try Again';

////////////////////////////////////////////////////////////////////////////////
// TIncorrectMoveForm

procedure TIncorrectMoveForm.FormCreate(Sender: TObject);
begin
  Caption := Application.Title;

  MsgLabel.Caption := MSG_INCORRECT_MOVE;

  TryAgainButton.Caption := LBL_TRY_AGAIN;
  ShowHintButton.Caption := LBL_SHOW_HINT;
end;


class function TIncorrectMoveForm.Show: TIncorrectMoveModalResult;
begin
  with TIncorrectMoveForm.Create(nil) do
  try
    ShowModal;
    Result := ModalResult;
  finally
    Free;
  end;
end;


procedure TIncorrectMoveForm.ShowHintButtonClick(Sender: TObject);
begin
  m_ModalResult := immrShowHint;
  Close;
end;


procedure TIncorrectMoveForm.TryAgainButtonClick(Sender: TObject);
begin
  m_ModalResult := immrTryAgain;
  Close;
end;

end.
