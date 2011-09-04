unit GamesManagerUnit;

interface

uses
  SysUtils, TntSysUtils, Classes, TntClasses, Contnrs,
  //
  PGNParserUnit, GamesListFormUnit, NonRefInterfacedObjectUnit;

type
  TGameItem = class
  private
    m_wstrPGNData: WideString;
    m_wstrName: WideString;
    constructor FCreate(const wstrPGNData, wstrGameName: WideString);
  public
    constructor Create;
    property PGNData: WideString read m_wstrPGNData;
    property Name: WideString read m_wstrName;
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

    function FGetGamesCount: integer;
    function FGetGame(iIndex: integer): TGameItem;

    procedure FDoChanged;

    function IGamesListProvider.GetGameName = FGetGameName;
    function FGetGameName(iIndex: integer): WideString;

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

    property GamesCount: integer read FGetGamesCount;
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
    m_iDataLine := 0;

    FParseFile;

  finally
    m_wstrlData.Clear;
    m_iDataLine := 0;
  end;

  Result := (m_Games.Count > 0);
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

  procedure NAddGame(const APGNGame: TTntStrings; const wstrGameName: WideString);
  var
    GameItem: TGameItem;
  begin
    GameItem := TGameItem.FCreate(APGNGame.Text, wstrGameName);
    m_Games.Add(GameItem);
  end;

var
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
      NAddGame(wstrlPGNGame, NO_NAME);

    repeat
      wstrlPGNGame.Clear;

      if (NParseTag(wstrlPGNGame)) then
        exit;
      if (NParseGame(wstrlPGNGame)) then
        NAddGame(wstrlPGNGame, FCreateGameName(TagParser));

    until (FIsEndOfData);

  finally
    wstrlPGNGame.Free;
    TagParser.Free;
    DataCursor.Free;
  end;

end;


class function TGamesManager.FCreateGameName(const ATagParser: TPGNTagParser): WideString;
begin
  if (Assigned(ATagParser)) then
    Result := Tnt_WideFormat('%s - %s', [ATagParser.White, ATagParser.Black])
  else
    Result := NO_NAME;
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


function TGamesManager.FGetGamesCount: integer;
begin
  Result := m_Games.Count;
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


function TGamesManager.FGetGameName(iIndex: integer): WideString;
begin
  Result := Games[iIndex].Name;
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

end.
