////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit GamesListFormUnit;

interface

uses
  Forms, AppEvnts, Classes, Controls, StdCtrls,
  //
  FloatingFormsUnit;

type
  TGameData = object
  private
    wstrName: WideString;
    bDataError: boolean;
  public
    property Name: WideString read wstrName write wstrName;
    property DataError: boolean read bDataError write bDataError;
  end;

  IGamesListProvider = interface
    function GetGamesCount: integer;
    function GetCurrentGameIndex: integer;
    procedure SetCurrentGameIndex(iValue: integer);
    procedure GetGameData(iIndex: integer; out AGameData: TGameData);
    property GamesCount: integer read GetGamesCount;
    property CurrentGameIndex: integer read GetCurrentGameIndex
                                       write SetCurrentGameIndex;
  end;

  TGamesListForm = class(TChildFloatingForm)
    GamesListBox: TListBox;
    ApplicationEvents: TApplicationEvents;    
    procedure ApplicationEventsShowHint(var HintStr: String;
      var CanShow: Boolean; var HintInfo: THintInfo);
    procedure FormShow(Sender: TObject);
    procedure GamesListBoxClick(Sender: TObject);
  private
    m_GamesListProvider: IGamesListProvider;
    procedure FSetGamesListProvider(Value: IGamesListProvider);
    function FGetGameName(iGameIndex: integer): WideString;
  public
    procedure Refresh;
    property GamesListProvider: IGamesListProvider
      read m_GamesListProvider write FSetGamesListProvider;
  end;

implementation

uses
  SysUtils, TntSysUtils;

{$R *.dfm}

const
  DATA_ERROR = '<DATA ERROR>';

////////////////////////////////////////////////////////////////////////////////
// TGamesListForm

procedure TGamesListForm.Refresh;
var
  i: integer;
begin
  if (not Assigned(m_GamesListProvider)) then
    exit;

  GamesListBox.Clear;
  for i := 0 to m_GamesListProvider.GamesCount - 1 do
  begin
    GamesListBox.Items.Append(FGetGameName(i));
  end;
  if ((GamesListBox.Count > 0) and (m_GamesListProvider.CurrentGameIndex >= 0)) then
    GamesListBox.ItemIndex := m_GamesListProvider.CurrentGameIndex;
end;


function TGamesListForm.FGetGameName(iGameIndex: integer): WideString;
var
  AGameData: TGameData;
begin
  m_GamesListProvider.GetGameData(iGameIndex, AGameData);

  if (AGameData.DataError) then
    Result := Tnt_WideFormat('%d. %s', [iGameIndex + 1, DATA_ERROR])
  else
    Result := Tnt_WideFormat('%d. %s', [iGameIndex + 1, AGameData.Name]);
end;


procedure TGamesListForm.FSetGamesListProvider(Value: IGamesListProvider);
begin
  if ((not Assigned(self)) or (m_GamesListProvider = Value)) then
    exit;

  m_GamesListProvider := Value;

  Refresh;
end;


procedure TGamesListForm.FormShow(Sender: TObject);
begin
  Refresh;
end;


procedure TGamesListForm.ApplicationEventsShowHint(
  var HintStr: String; var CanShow: Boolean; var HintInfo: THintInfo);
var
  iIndex: integer;
begin
  if (HintInfo.HintControl <> GamesListBox) then
    exit;

  iIndex := GamesListBox.ItemAtPos(GamesListBox.ScreenToClient(Mouse.CursorPos),
    TRUE);

  if ((iIndex >= 0) and (iIndex < GamesListBox.Items.Count)) then
    HintStr := GamesListBox.Items[iIndex];
    
  CanShow := (HintStr <> '');
end;


procedure TGamesListForm.GamesListBoxClick(Sender: TObject);
begin
  if (not Assigned(m_GamesListProvider)) then
    exit;

  m_GamesListProvider.CurrentGameIndex := GamesListBox.ItemIndex;
  GamesListBox.ItemIndex := m_GamesListProvider.CurrentGameIndex;

  GamesListBox.Items[GamesListBox.ItemIndex] :=
    FGetGameName(m_GamesListProvider.CurrentGameIndex);
end;

end.
