////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit SelectLineFormUnit;

interface

uses
  Forms, Classes, Controls, StdCtrls,
  //
  PlysProviderIntfUnit;

type
  TSelectLineForm = class(TForm)
    MovesListBox: TListBox;
    CancelButton: TButton;
    OKButton: TButton;
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure MovesListBoxDblClick(Sender: TObject);
  private
    m_PlysProvider: IPlysProvider;
    procedure FSetPlysProvider(APlysProvider: IPlysProvider);
    procedure FRefresh;
    procedure FApplySelection;
  public
    property PlysProvider: IPlysProvider read m_PlysProvider write FSetPlysProvider;
  end;

implementation

{$R *.dfm}

////////////////////////////////////////////////////////////////////////////////
// TSelectLineForm

procedure TSelectLineForm.FSetPlysProvider(APlysProvider: IPlysProvider);
begin
  m_PlysProvider := APlysProvider;
  if (Showing) then
    FRefresh;
end;


procedure TSelectLineForm.FormDestroy(Sender: TObject);
begin
  m_PlysProvider := nil;
end;


procedure TSelectLineForm.FRefresh;
var
  LinePlys: TStrings;
  iNextPly: integer;
  iIndex: integer;
begin
  MovesListBox.Clear;

  if (not Assigned(m_PlysProvider)) then
    exit;

  iNextPly := m_PlysProvider.CurrentPlyIndex + 1;

  LinePlys := MovesListBox.Items;
  m_PlysProvider.GetPlysForPlyIndex(iNextPly, LinePlys);

  iIndex := MovesListBox.Items.IndexOf(m_PlysProvider.Plys[iNextPly]);
  if (iIndex >= 0) then
    MovesListBox.ItemIndex := iIndex;
end;


procedure TSelectLineForm.FormShow(Sender: TObject);
begin
  ActiveControl := MovesListBox;
  FRefresh;
end;


procedure TSelectLineForm.CancelButtonClick(Sender: TObject);
begin
  Close;
end;


procedure TSelectLineForm.OKButtonClick(Sender: TObject);
begin
  FApplySelection;
  Close;
end;


procedure TSelectLineForm.FApplySelection;
var
  iIndex: integer;
  strPly: string;
begin
  if (not Assigned(m_PlysProvider)) then
    exit;

  iIndex := MovesListBox.ItemIndex;
  if (iIndex >= 0) then
    strPly := MovesListBox.Items[iIndex]
  else
    exit;

  m_PlysProvider.SetPlyForPlyIndex(m_PlysProvider.CurrentPlyIndex + 1, strPly);
end;


procedure TSelectLineForm.MovesListBoxDblClick(Sender: TObject);
begin
  OKButton.Click;
end;

end.
