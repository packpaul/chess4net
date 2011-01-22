unit ChessEngineInfoUnit;

interface

uses
  Forms, Classes, Controls, StdCtrls;

type
  TChessEngineInfoForm = class(TForm)
    InfoMemo: TMemo;
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
  private
    FOnShow: TNotifyEvent;
    FOnHide: TNotifyEvent;
    procedure FDoShow;
    procedure FDoHide;
  public
    procedure SetInfo(const rEvaluation: real; const strMovesLine: string);
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property OnHide: TNotifyEvent read FOnHide write FOnHide;
  end;

implementation

uses
  SysUtils, Math;

{$R *.dfm}

////////////////////////////////////////////////////////////////////////////////
// TChessEngineInfoForm

procedure TChessEngineInfoForm.SetInfo(const rEvaluation: real; const strMovesLine: string);

  function NEvalToStr: string;
  begin
    Result := 'Eval.: ';
    if (IsNan(rEvaluation)) then
      Result := Result + '#'
    else
      Result := Result + FormatFloat('0.00', rEvaluation)
  end;

begin // .SetInfo
  InfoMemo.Lines.BeginUpdate;
  try
    InfoMemo.Clear;
    InfoMemo.Lines.Add(NEvalToStr);
    InfoMemo.Lines.Add('Line: ' + strMovesLine);
  finally
    InfoMemo.Lines.EndUpdate;
  end;
end;


procedure TChessEngineInfoForm.FormShow(Sender: TObject);
begin
  FDoShow;
end;


procedure TChessEngineInfoForm.FDoShow;
begin
  if (Assigned(FOnShow)) then
    FOnShow(self);
end;


procedure TChessEngineInfoForm.FormHide(Sender: TObject);
begin
  FDoHide;
end;


procedure TChessEngineInfoForm.FDoHide;
begin
  if (Assigned(FOnHide)) then
    FOnHide(self);
end;

end.
