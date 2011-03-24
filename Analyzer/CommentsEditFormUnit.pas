unit CommentsEditFormUnit;

interface

uses
  Forms, StdCtrls, TntStdCtrls, Classes, Controls, Messages;

type
  TCommentsEditForm = class;

  TTntMemo = class(TntStdCtrls.TTntMemo)
  private
    function FGetCommentsEditForm: TCommentsEditForm;
    procedure CNKeyDown(var Message: TWMKeyDown); message CN_KEYDOWN;
    property CommentsEditForm: TCommentsEditForm read FGetCommentsEditForm;
  end;

  TCommentsEditForm = class(TForm)
    OkButton: TButton;
    CommentsMemo: TTntMemo;
    CancelButton: TButton;
    procedure CommentsMemoChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    function FGetFormattedComments: WideString;
    procedure FSetComments(const wstrValue: WideString);
    property Comments: WideString read FGetFormattedComments write FSetComments;
  public
    class function Edit(var wstrComments: WideString): boolean;
  end;

implementation

uses
  Windows, SysUtils, StrUtils,
  //
  WinControlHlpUnit;

{$R *.dfm}

////////////////////////////////////////////////////////////////////////////////
// TCommentsEditForm

class function TCommentsEditForm.Edit(var wstrComments: WideString): boolean;
begin
  Result := FALSE;

  with TCommentsEditForm.Create(nil) do
  try
    Comments := wstrComments;
    if (ShowModal <> mrOk) then
      exit;
    wstrComments := Comments;
  finally
    Release;
  end;

  Result := TRUE;
end;


function TCommentsEditForm.FGetFormattedComments: WideString;
var
  i: integer;
  wstrSource: WideString;
  iLen: integer;
begin
  iLen := 0;

  Result := '';
  for i := 0 to CommentsMemo.Lines.Count - 1 do
  begin
    wstrSource := TrimRight(CommentsMemo.Lines[i]);

    if (Result = '') then
      Result := wstrSource
    else
      Result := Result + sLineBreak + wstrSource;

    if (wstrSource <> '') then
      iLen := Length(Result);
  end;

  Result := LeftStr(Result, iLen);
end;


procedure TCommentsEditForm.FSetComments(const wstrValue: WideString);
begin
  CommentsMemo.Text := wstrValue;
end;


procedure TCommentsEditForm.CommentsMemoChange(Sender: TObject);
begin
  OkButton.Enabled := TRUE;
end;


procedure TCommentsEditForm.FormShow(Sender: TObject);
begin
  OkButton.Enabled := FALSE;
end;

////////////////////////////////////////////////////////////////////////////////
// TTntMemo

procedure TTntMemo.CNKeyDown(var Message: TWMKeyDown);
begin
  case Message.CharCode of
    VK_RETURN:
    begin
      if ((GetKeyState(VK_CONTROL) and (not $7FFF)) <> 0) then
      begin
        Message.Result := 1;
        CommentsEditForm.OkButton.Click;
      end;
    end;

    VK_ESCAPE:
    begin
      Message.Result := 1;
      CommentsEditForm.CancelButton.Click;
      exit;
    end;

  end;

  TWinControlHlp.CNKeyDown(self, Message);  
end;


function TTntMemo.FGetCommentsEditForm: TCommentsEditForm;
begin
  Result := Owner as TCommentsEditForm;
end;

end.
