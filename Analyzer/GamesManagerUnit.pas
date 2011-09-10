unit GamesManagerUnit;

interface

uses
  SysUtils, TntSysUtils, Classes, TntClasses, Contnrs,
  //
  PGNParserUnit, PGNWriterUnit, GamesListFormUnit, NonRefInterfacedObjectUnit;

type
  TGameItem = class
  private
    m_wstrPGNData: WideString;
    m_wstrName: WideString;
    m_bDataError: boolean;
    m_FileName: TFileName;
    m_Data: TObject;
    constructor FCreate(const wstrPGNData, wstrGameName: WideString);
    procedure FSetDataError(bValue: boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetData(var AData: TObject);
    property PGNData: WideString read m_wstrPGNData;
    property Name: WideString read m_wstrName;
    property FileName: TFileName read m_FileName;
    property Data: TObject read m_Data;
    property DataError: boolean read m_bDataError;
  end;


  TGamesManager = class(TNonRefInterfacedObject, IGamesListProvider)
  private
    m_wstrlData: TTntStringList;
    m_iDataLine: integer;

    m_Games: TObjectList;
    m_iCurrentGameIndex: integer;

    FOnChanged: TNotifyEvent;

    function FGetLine: WideString;
    function FGetNextLine: WideString;
    function FIsEndOfData: boolean;

    procedure FParseFile;
    class function FCreateGameName(const ATagParser: TPGNTagParser): WideString;

    procedure FAddGame(const APGNGame: TTntStrings; const wstrGameName: WideString);
    function FGetGame(iIndex: integer): TGameItem;

    procedure FDoChanged;

    procedure IGamesListProvider.GetGameData = FGetGameData;
    procedure FGetGameData(iIndex: integer; out AGameData: TGameData);

    function IGamesListProvider.GetCurrentGameIndex = FGetCurrentGameIndex;
    function FGetCurrentGameIndex: integer;
    procedure IGamesListProvider.SetCurrentGameIndex = FSetCurrentGameIndex;
    procedure FSetCurrentGameIndex(iValue: integer);

  public
    constructor Create;
    destructor Destroy; override;

    function LoadFromFile(const AFileName: TFileName): boolean;
    function GetGamesCount: integer;

    procedure Clear;
    procedure AddGame(const APGNWriter: TPGNWriter);
    function ParseGame(var PGNParser: TPGNParser; iGameIndex: integer): boolean;

    property GamesCount: integer read GetGamesCount;
    property Games[iIndex: integer]: TGameItem read FGetGame;
    property CurrentGameIndex: integer read m_iCurrentGameIndex;

    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

implementation

type
  TPGNDataCursorAdaptor = class(TNonRefInterfacedObject, IPGNDataCursor)
  private
    m_GamesManager: TGamesManager;
  public
    constructor Create(const AGamesManager: TGamesManager);
    function GetLine: WideString;
    function GetNextLine: WideString;
    function IsEndOfData: boolean;
  end;

const
  NO_NAME = 'NN';
  NO_NAME_GAME = 'NN - NN';

////////////////////////////////////////////////////////////////////////////////
// TGamesManager

constructor TGamesManager.Create;
begin
  inherited Create;

  m_wstrlData := TTntStringList.Create;
  m_Games := TObjectList.Create;
  Clear;
end;


destructor TGamesManager.Destroy;
begin
  m_Games.Free;
  m_wstrlData.Free;
  inherited;
end;


function TGamesManager.LoadFromFile(const AFileName: TFileName): boolean;
begin
  m_Games.Clear;
  m_iCurrentGameIndex := -1;

  m_wstrlData.LoadFromFile(AFileName);
  try
    FParseFile;
  finally
    m_iDataLine := 0;
    m_wstrlData.Clear;
  end;

  if (GamesCount = 1) then
    Games[0].m_FileName := AFileName;

  Result := (GamesCount > 0);
  if (Result) then
    m_iCurrentGameIndex := 0;

  FDoChanged;
end;


procedure TGamesManager.FParseFile;

  function NParseGame(var wstrlPGNGame: TTntStringList): boolean;
  var
    wstr: WideString;
  begin
    Result := FALSE;

    if (FIsEndOfData) then
      exit;

    wstr := FGetLine;
    repeat
      if (TPGNTagParser.IsTag(wstr)) then
        exit;

      if (TrimRight(wstr) <> '') then
        Result := TRUE;

      wstrlPGNGame.Add(wstr);

      wstr := FGetNextLine;

    until (FIsEndOfData);

    // TODO: delete empty lines after the game
  end;

var
  TagParser: TPGNTagParser;
  DataCursor: TPGNDataCursorAdaptor;

  function NParseTag(var wstrlPGNGame: TTntStringList): boolean;
  var
    iStartTagDataLine: integer;
    i: integer;
  begin
    Result := FALSE;

    iStartTagDataLine := m_iDataLine;
    if (not TagParser.Parse(DataCursor)) then
      exit;

    for i := iStartTagDataLine to m_iDataLine - 1 do
      wstrlPGNGame.Add(m_wstrlData[i]);
  end;

var
  wstrlPGNGame: TTntStringList;
begin // .FParseFile
  if (FIsEndOfData) then
    exit;

  wstrlPGNGame := nil;
  TagParser := nil;

  DataCursor := TPGNDataCursorAdaptor.Create(self);
  try
    TagParser := TPGNTagParser.Create;
    wstrlPGNGame := TTntStringList.Create;

    if (NParseGame(wstrlPGNGame)) then
      FAddGame(wstrlPGNGame, NO_NAME_GAME);

    repeat
      wstrlPGNGame.Clear;

      if (NParseTag(wstrlPGNGame)) then
        exit;
      if (NParseGame(wstrlPGNGame)) then
        FAddGame(wstrlPGNGame, FCreateGameName(TagParser));

    until (FIsEndOfData);

  finally
    wstrlPGNGame.Free;
    TagParser.Free;
    DataCursor.Free;
  end;

end;


procedure TGamesManager.FAddGame(const APGNGame: TTntStrings;
  const wstrGameName: WideString);
var
  GameItem: TGameItem;
begin
  GameItem := TGameItem.FCreate(APGNGame.Text, wstrGameName);
  m_Games.Add(GameItem);
end;


class function TGamesManager.FCreateGameName(const ATagParser: TPGNTagParser): WideString;
var
  wstrWhite, wstrBlack: WideString;
begin
  if (Assigned(ATagParser)) then
  begin
    if (ATagParser.White <> '') then
      wstrWhite := ATagParser.White
    else
      wstrWhite := NO_NAME;

    if (ATagParser.Black <> '') then
      wstrBlack := ATagParser.Black
    else
      wstrBlack := NO_NAME;

    Result := Tnt_WideFormat('%s - %s', [wstrWhite, wstrBlack])
  end
  else
    Result := NO_NAME_GAME;
end;


function TGamesManager.GetGamesCount: integer;
begin
  Result := m_Games.Count;
end;


function TGamesManager.FGetLine: WideString;
begin
  if (m_iDataLine < m_wstrlData.Count) then
    Result := m_wstrlData[m_iDataLine]
  else
    Result := '';
end;


function TGamesManager.FGetNextLine: WideString;
begin
  inc(m_iDataLine);
  
  if (m_iDataLine < (m_wstrlData.Count)) then
    Result := m_wstrlData[m_iDataLine]
  else
    Result := '';
end;


function TGamesManager.FIsEndOfData: boolean;
begin
  Result := (m_iDataLine >= m_wstrlData.Count);
end;


function TGamesManager.FGetGame(iIndex: integer): TGameItem;
begin
  Result := m_Games[iIndex] as TGameItem;
end;


procedure TGamesManager.FDoChanged;
begin
  if (not Assigned(FOnChanged)) then
    exit;

  FOnChanged(self);
end;


procedure TGamesManager.Clear;
begin
  m_Games.Clear;
  m_iCurrentGameIndex := -1;

  FDoChanged;
end;


procedure TGamesManager.FGetGameData(iIndex: integer; out AGameData: TGameData);
var
  GameItem: TGameItem;
begin
  GameItem := Games[iIndex];

  AGameData.Name := GameItem.Name;
  AGameData.DataError := GameItem.DataError;
end;


function TGamesManager.FGetCurrentGameIndex: integer;
begin
  Result := m_iCurrentGameIndex;
end;


procedure TGamesManager.FSetCurrentGameIndex(iValue: integer);
begin
  if ((iValue = m_iCurrentGameIndex) or
      (not ((iValue >= 0) and (iValue < GamesCount)))) then
    exit;

  m_iCurrentGameIndex := iValue;

  FDoChanged;
end;


function TGamesManager.ParseGame(var PGNParser: TPGNParser; iGameIndex: integer): boolean;
var
  AGameItem: TGameItem;
begin
  Result := FALSE;

  if (not (Assigned(PGNParser) and
           (iGameIndex >= 0) and (iGameIndex < GamesCount))) then
    exit;

  AGameItem := Games[iGameIndex];

  Result := PGNParser.Parse(AGameItem.PGNData);

  if (not Result) then
    AGameItem.FSetDataError(TRUE);
end;


procedure TGamesManager.AddGame(const APGNWriter: TPGNWriter);
begin
  FAddGame(APGNWriter.Data, NO_NAME_GAME);
  if (GamesCount = 1) then
    FSetCurrentGameIndex(0);
end;

////////////////////////////////////////////////////////////////////////////////
// TPGNDataCursorAdaptor

constructor TPGNDataCursorAdaptor.Create(const AGamesManager: TGamesManager);
begin
  inherited Create;
  m_GamesManager := AGamesManager;
end;


function TPGNDataCursorAdaptor.GetLine: WideString;
begin
  Result := m_GamesManager.FGetLine;
end;


function TPGNDataCursorAdaptor.GetNextLine: WideString;
begin
  Result := m_GamesManager.FGetNextLine;
end;


function TPGNDataCursorAdaptor.IsEndOfData: boolean;
begin
  Result := m_GamesManager.FIsEndOfData;
end;

////////////////////////////////////////////////////////////////////////////////
// TGameItem

constructor TGameItem.Create;
begin
  raise Exception.Create('TIniSettings cannot be instantiated directly!');
end;


constructor TGameItem.FCreate(const wstrPGNData, wstrGameName: WideString);
begin
  inherited Create;

  m_wstrPGNData := wstrPGNData;
  m_wstrName := wstrGameName;
end;


destructor TGameItem.Destroy;
begin
  m_Data.Free; 
  inherited;
end;


procedure TGameItem.FSetDataError(bValue: boolean);
begin
  if (m_bDataError or (not bValue)) then
    exit;

  m_bDataError := bValue;

  m_wstrPGNData := '';
  m_FileName := '';
end;


procedure TGameItem.SetData(var AData: TObject);
begin
  m_Data := AData;
end;

end.
