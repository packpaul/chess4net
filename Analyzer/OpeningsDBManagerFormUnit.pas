unit OpeningsDBManagerFormUnit;

interface

uses
   Forms, Dialogs, StdCtrls, Classes, Controls, SysUtils, ActnList,
  AppEvnts;

type
  IOpeningsDBManagerProvider = interface
    function GetEnabled: boolean;
    procedure SetEnabled(bValue: boolean);
    function GetDBIndex: integer;
    procedure SetDBIndex(iIndex: integer);
    function GetDBCount: integer;
    function GetDBs(iIndex: integer): TFileName;
    function GetDBNames(iIndex: integer): WideString;
    function AddDB(const DBFileName: TFileName): boolean;
    function RemoveDB(iIndex: integer): boolean;
    property Enabled: boolean read GetEnabled write SetEnabled;
    property DBIndex: integer read GetDBIndex write SetDBIndex;
    property DBCount: integer read GetDBCount;
    property DBs[iIndex: integer]: TFileName read GetDBs;
    property DBNames[iIndex: integer]: WideString read GetDBNames;    
  end;

  TOpeningsDBManagerForm = class(TForm)
    OpeningsDBListBox: TListBox;
    AddDBButton: TButton;
    RemoveDBButton: TButton;
    DBEnabledCheckBox: TCheckBox;
    DBOpenDialog: TOpenDialog;
    ActionList: TActionList;
    RemoveDBAction: TAction;
    ApplicationEvents: TApplicationEvents;
    procedure DBEnabledCheckBoxClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure AddDBButtonClick(Sender: TObject);
    procedure OpeningsDBListBoxClick(Sender: TObject);
    procedure RemoveDBActionExecute(Sender: TObject);
    procedure RemoveDBActionUpdate(Sender: TObject);
    procedure ApplicationEventsShowHint(var HintStr: String;
      var CanShow: Boolean; var HintInfo: THintInfo);
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

const
  MSG_DB_REMOVE = 'Do you really want to remove Opening DB "%s"?';

////////////////////////////////////////////////////////////////////////////////
// TOpeningsDBManagerForm

procedure TOpeningsDBManagerForm.DBEnabledCheckBoxClick(Sender: TObject);
begin
  if (Assigned(m_OpeningsDBManagerProvider)) then
    m_OpeningsDBManagerProvider.Enabled := DBEnabledCheckBox.Checked;
end;


procedure TOpeningsDBManagerForm.FRefresh;
var
  i: integer;
begin
  if (not Assigned(m_OpeningsDBManagerProvider)) then
    exit;

  OpeningsDBListBox.Clear;
  for i := 0 to m_OpeningsDBManagerProvider.DBCount - 1 do
    OpeningsDBListBox.Items.Append(m_OpeningsDBManagerProvider.DBNames[i]);
  if (OpeningsDBListBox.Count > 0) then
    OpeningsDBListBox.ItemIndex := m_OpeningsDBManagerProvider.DBIndex;

  DBEnabledCheckBox.Checked := m_OpeningsDBManagerProvider.Enabled;
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


procedure TOpeningsDBManagerForm.AddDBButtonClick(Sender: TObject);
var
  DBFileName: TFileName;
begin
  if (not DBOpenDialog.Execute) then
    exit;

  if (Assigned(m_OpeningsDBManagerProvider)) then
  begin
    DBFileName := ChangeFileExt(DBOpenDialog.FileName, '');
    if (m_OpeningsDBManagerProvider.AddDB(DBFileName)) then
      FRefresh;
  end;

end;


procedure TOpeningsDBManagerForm.OpeningsDBListBoxClick(Sender: TObject);
begin
  if (not Assigned(m_OpeningsDBManagerProvider)) then
    exit;

  m_OpeningsDBManagerProvider.DBIndex := OpeningsDBListBox.ItemIndex;
end;


procedure TOpeningsDBManagerForm.RemoveDBActionExecute(Sender: TObject);
var
  strMsg: string;
begin
  if (not Assigned(m_OpeningsDBManagerProvider)) then
    exit;

  if (OpeningsDBListBox.ItemIndex >= 0) then
  begin
    strMsg := Format(MSG_DB_REMOVE, [m_OpeningsDBManagerProvider.DBNames[OpeningsDBListBox.ItemIndex]]); // TODO: -> WideString
    if (MessageDlg(strMsg, mtConfirmation, [mbYes, mbNo], 0) <> mrYes) then
      exit;

    if (m_OpeningsDBManagerProvider.RemoveDB(OpeningsDBListBox.ItemIndex)) then
      FRefresh;
  end;
end;


procedure TOpeningsDBManagerForm.RemoveDBActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (OpeningsDBListBox.ItemIndex >= 0);
end;


procedure TOpeningsDBManagerForm.ApplicationEventsShowHint(
  var HintStr: String; var CanShow: Boolean; var HintInfo: THintInfo);
var
  iIndex: integer;
begin
  if (HintInfo.HintControl <> OpeningsDBListBox) then
    exit;

  if (not Assigned(m_OpeningsDBManagerProvider)) then
  begin
    CanShow := FALSE;
    exit;
  end;

  iIndex := OpeningsDBListBox.ItemAtPos(OpeningsDBListBox.ScreenToClient(Mouse.CursorPos),
    TRUE);

  HintStr := m_OpeningsDBManagerProvider.DBs[iIndex];
  CanShow := (HintStr <> '');
end;

end.
