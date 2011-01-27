unit OpeningsDBManagerFormUnit;

interface

uses
  Forms, StdCtrls, Classes, Controls;

type
  IOpeningsDBManagerProvider = interface
    function GetDBEnabled: boolean;
    procedure SetDBEnabled(bValue: boolean);
    property DBEnabled: boolean read GetDBEnabled write SetDBEnabled;
  end;

  TOpeningsDBManagerForm = class(TForm)
    OpeningsDBListBox: TListBox;
    AddDBButton: TButton;
    RemoveDBButton: TButton;
    DBEnabledCheckBox: TCheckBox;
    procedure DBEnabledCheckBoxClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    m_OpeningsDBManagerProvider: IOpeningsDBManagerProvider;
    procedure FRefresh;
    procedure FSetOpeningsDBManagerProvider(Value: IOpeningsDBManagerProvider);
  public
    property OpeningsDBManagerProvider: IOpeningsDBManagerProvider
      read m_OpeningsDBManagerProvider write FSetOpeningsDBManagerProvider;
  end;

implementation

{$R *.dfm}

procedure TOpeningsDBManagerForm.DBEnabledCheckBoxClick(Sender: TObject);
begin
  if (Assigned(m_OpeningsDBManagerProvider)) then
    m_OpeningsDBManagerProvider.DBEnabled := DBEnabledCheckBox.Checked;
end;


procedure TOpeningsDBManagerForm.FRefresh;
begin
  if (not Assigned(m_OpeningsDBManagerProvider)) then
    exit;

  DBEnabledCheckBox.Checked := m_OpeningsDBManagerProvider.DBEnabled;

  // TODO:
end;


procedure TOpeningsDBManagerForm.FSetOpeningsDBManagerProvider(Value: IOpeningsDBManagerProvider);
begin
  if ((not Assigned(self)) or (m_OpeningsDBManagerProvider = Value)) then
    exit;

  m_OpeningsDBManagerProvider := Value;

  FRefresh;
end;


procedure TOpeningsDBManagerForm.FormShow(Sender: TObject);
begin
  FRefresh;
end;

end.
