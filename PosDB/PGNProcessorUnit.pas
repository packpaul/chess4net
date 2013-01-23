////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit PGNProcessorUnit;

interface

uses
  PGNTraverserUnit, PosBaseCollectorUnit, MoveTreeCollectorUnit, PosBaseUnit,
  MoveTreeBaseUnit;

type
  TPGNProcessor = class
  private
    m_strBasename: string;
    m_bVariants: boolean;
    m_bChngest: boolean;
    m_bUniquePos: boolean;
    m_Color: TFigureColors;
    m_iNumPlys: integer;
    m_strPlayerName: string;
    m_Opening: TOpening;
    m_bExcludeLoopsInExtensions: boolean;
    m_bStatPrunning: boolean;
    m_strRefBaseName: string;
    m_bMoveTreeDB: boolean;
    m_bUniqueGames: boolean;

    m_PosBaseCollector: TPosBaseCollector;
    m_MoveTreeCollector: TMoveTreeCollector;
    m_RefPosBase: TPosBase;
    m_RefMoveTreeBase: TMoveTreeBase;

    constructor FCreate(const strBasename: string; bVariants, bChngest: boolean; bUniquePos: boolean;
      const Color: TFigureColors; iNumPlys: integer; const strPlayerName: string;
      Opening: TOpening; bExcludeLoopsInExtensions: boolean; bStatPrunning: boolean;
      strRefBaseName: string; bMoveTreeDB: boolean; bUniqueGames: boolean);

    function FGetPosBaseCollector: TPosBaseCollector;
    function FGetMoveTreeCollector: TMoveTreeCollector;

    procedure FProceedPGN;

    property PosBaseCollector: TPosBaseCollector read FGetPosBaseCollector;
    property MoveTreeCollector: TMoveTreeCollector read FGetMoveTreeCollector;

  public
    constructor Create;
    destructor Destroy; override;

    class procedure Proceed(const basename: string; variants, chngest: boolean; uniquePos: boolean;
                            const color: TFigureColors; numPlys: integer; const player_name: string;
                            opening: TOpening; bExcludeLoopsInExtensions: boolean; bStatPrunning: boolean;
                            refBaseName: string; bMoveTreeDB: boolean; bUniqueGames: boolean);
  end;

implementation

uses
  SysUtils,
  //
  LoggerUnit;

////////////////////////////////////////////////////////////////////////////////
// TPGNProcessor

constructor TPGNProcessor.Create;
begin
  raise Exception.Create('TPGNProcessor cannot be instantiated directly!');
end;


constructor TPGNProcessor.FCreate(const strBasename: string; bVariants, bChngest: boolean;
  bUniquePos: boolean; const Color: TFigureColors; iNumPlys: integer; const strPlayerName: string;
  Opening: TOpening; bExcludeLoopsInExtensions: boolean; bStatPrunning: boolean; strRefBaseName: string;
  bMoveTreeDB: boolean; bUniqueGames: boolean);
begin
  inherited Create;

  m_strBasename := strBasename;
  m_bVariants := bVariants;
  m_bChngest := bChngest;
  m_bUniquePos := bUniquePos;
  m_Color := color;
  m_iNumPlys := iNumPlys;
  m_strPlayerName := strPlayerName;
  m_Opening := opening;
  m_bExcludeLoopsInExtensions := bExcludeLoopsInExtensions; 
  m_bStatPrunning := bStatPrunning;
  m_strRefBaseName := strRefBaseName;
  m_bMoveTreeDB := bMoveTreeDB;
  m_bUniqueGames := bUniqueGames;
end;


destructor TPGNProcessor.Destroy;
begin
  m_PosBaseCollector.Free;
  m_MoveTreeCollector.Free;

  m_RefPosBase.Free;
  m_RefMoveTreeBase.Free;

  inherited;
end;


function TPGNProcessor.FGetPosBaseCollector: TPosBaseCollector;
begin
  if (not Assigned(m_PosBaseCollector)) then
  begin
    if (TMoveTreeBase.Exists(m_strRefBaseName)) then
      m_RefMoveTreeBase := TMoveTreeBase.Create(m_strRefBaseName);

    m_RefPosBase := TPosBase.Create(m_strRefBaseName, m_RefMoveTreeBase);

    m_PosBaseCollector := TPosBaseCollector.Create(m_strBasename, m_RefPosBase);
    with m_PosBaseCollector do
    begin
      ProceedColors := m_Color;
      PlayerName := m_strPlayerName;
      ChangeEstimation := m_bChngest;
      UseUniquePositions := m_bUniquePos;
      GeneratedOpening := m_Opening;
      ExcludeLoopsInExtensions := m_bExcludeLoopsInExtensions;
      UseStatisticalPrunning := m_bStatPrunning;
      UseNumberOfPlys := m_iNumPlys;
      UniqueGames := m_bUniqueGames;
      if (m_bMoveTreeDB) then
        MoveTreeBase := MoveTreeCollector.DataBase;
    end;
  end;

  Result := m_PosBaseCollector;
end;


function TPGNProcessor.FGetMoveTreeCollector: TMoveTreeCollector;
begin
  if (not Assigned(m_MoveTreeCollector)) then
  begin
    if (m_bMoveTreeDB) then
      m_MoveTreeCollector := TMoveTreeCollector.Create(m_strBasename);
  end;

  Result := m_MoveTreeCollector;
end;


class procedure TPGNProcessor.Proceed(const basename: string; variants, chngest: boolean; uniquePos: boolean;
  const color: TFigureColors; numPlys: integer; const player_name: string;
  opening: TOpening; bExcludeLoopsInExtensions: boolean; bStatPrunning: boolean; refBaseName: string;
  bMoveTreeDB: boolean; bUniqueGames: boolean);
begin
  with TPGNProcessor.FCreate(basename, variants, chngest, uniquePos, color,
    numPlys, player_name, opening, bExcludeLoopsInExtensions, bStatPrunning, refBaseName, bMoveTreeDB, bUniqueGames) do
  try
    FProceedPGN;
  finally
    Free;
  end;
end;


procedure TPGNProcessor.FProceedPGN;
begin
  with TPGNTraverser.Create(Input, [MoveTreeCollector, PosBaseCollector]) do // PP: Visitable order is important here!
  try
    ProceedColors := m_Color;
    PlayerName := m_strPlayerName;
    IncludeVariants := m_bVariants;

    Traverse;

    TLogger.GetInstance.Info('Games viewed: ' + IntToStr(NumberOfGamesViewed));
    TLogger.GetInstance.Info('Positions viewed: ' + IntToStr(NumberOfPositionsViewed));
  finally
    Free;
  end;
  
end;

end.
