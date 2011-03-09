unit CommentsFormUnit;

interface

uses
  StdCtrls, TntStdCtrls, Classes, Controls,
  //
  FloatingFormsUnit, PlysProviderIntfUnit;

type
  TCommentsForm = class(TChildFloatingForm)
    EditButton: TButton;
    CommentsMemo: TTntMemo;
    procedure FormCreate(Sender: TObject);
    procedure EditButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    m_PlysProvider: IPlysProvider;
    procedure FSetPlysProvider(APlysProvider: IPlysProvider);
    function FGetComments: WideString;
    procedure FSetComments(const wstrComments: WideString);
  public
    procedure Refresh;
    property PlysProvider: IPlysProvider read m_PlysProvider write FSetPlysProvider;
  end;

implementation

{$R *.dfm}

uses
  CommentsEditFormUnit;

////////////////////////////////////////////////////////////////////////////////
// TCommentsForm

procedure TCommentsForm.FSetPlysProvider(APlysProvider: IPlysProvider);
begin
  m_PlysProvider := APlysProvider;
  if (Showing) then
    Refresh;
end;


procedure TCommentsForm.Refresh;
begin
  CommentsMemo.Text := FGetComments;
end;


procedure TCommentsForm.FormCreate(Sender: TObject);
begin
  m_PlysProvider := nil;
end;


procedure TCommentsForm.EditButtonClick(Sender: TObject);
var
  wstrComments: WideString;
begin
  wstrComments := FGetComments;

  if (TCommentsEditForm.Edit(wstrComments)) then
  begin
    FSetComments(wstrComments);
    Refresh;
  end;
end;


function TCommentsForm.FGetComments: WideString;
begin
  if (Assigned(m_PlysProvider)) then
    Result := m_PlysProvider.Comments[m_PlysProvider.CurrentPlyIndex]
  else
    Result := '';
end;


procedure TCommentsForm.FSetComments(const wstrComments: WideString);
begin
  if (Assigned(m_PlysProvider)) then
    m_PlysProvider.Comments[m_PlysProvider.CurrentPlyIndex] := wstrComments;
end;


procedure TCommentsForm.FormShow(Sender: TObject);
begin
  Refresh;
end;

end.
