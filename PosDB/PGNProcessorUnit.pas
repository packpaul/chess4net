////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit PGNProcessorUnit;

interface

uses
  PGNTraverserUnit, PosBaseCollectorUnit, MoveTreeCollectorUnit, PosBaseUnit;

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
    m_bStatPrunning: boolean;
    m_strRefBaseName: string;
    m_bMoveTreeDB: boolean;

    m_PosBaseCollector: TPosBaseCollector;
    m_MoveTreeCollector: TMoveTreeCollector;
    m_RefPosBase: TPosBase;

    constructor FCreate(const strBasename: string; bVariants, bChngest: boolean; bUniquePos: boolean;
      const Color: TFigureColors; iNumPlys: integer; const strPlayerName: string;
      Opening: TOpening; bStatPrunning: boolean; strRefBaseName: string; bMoveTreeDB: boolean);

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
                            opening: TOpening; statPrunning: boolean; refBaseName: string;
                            bMoveTreeDB: boolean);
  end;

implementation

uses
  SysUtils;

////////////////////////////////////////////////////////////////////////////////
// TPGNProcessor

constructor TPGNProcessor.Create;
begin
  raise Exception.Create('TPGNProcessor cannot be instantiated directly!');
end;


constructor TPGNProcessor.FCreate(const strBasename: string; bVariants, bChngest: boolean; bUniquePos: boolean;
  const Color: TFigureColors; iNumPlys: integer; const strPlayerName: string;
  Opening: TOpening; bStatPrunning: boolean; strRefBaseName: string; bMoveTreeDB: boolean);
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
  m_bStatPrunning := bStatPrunning;
  m_strRefBaseName := strRefBaseName;
  m_bMoveTreeDB := bMoveTreeDB;
end;


destructor TPGNProcessor.Destroy;
begin
  m_PosBaseCollector.Free;
  m_MoveTreeCollector.Free;
  m_RefPosBase.Free;

  inherited;
end;


function TPGNProcessor.FGetPosBaseCollector: TPosBaseCollector;
begin
  if (not Assigned(m_PosBaseCollector)) then
  begin
    m_RefPosBase := TPosBase.Create(m_strRefBaseName);
    m_PosBaseCollector := TPosBaseCollector.Create(m_strBasename, m_RefPosBase);
    with m_PosBaseCollector do
    begin
      ProceedColors := m_Color;
      PlayerName := m_strPlayerName;
      ChangeEstimation := m_bChngest;
      UseUniquePositions := m_bUniquePos;
      GeneratedOpening := m_Opening;
      UseStatisticalPrunning := m_bStatPrunning;
      UseNumberOfPlys := m_iNumPlys;
      if (m_bMoveTreeDB) then
        PosBaseCollector.MoveTreeBase := MoveTreeCollector.DataBase;
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
  opening: TOpening; statPrunning: boolean; refBaseName: string; bMoveTreeDB: boolean);
begin
  with TPGNProcessor.FCreate(basename, variants, chngest, uniquePos, color,
    numPlys, player_name, opening, statPrunning, refBaseName, bMoveTreeDB) do
  try
    FProceedPGN;
  finally
    Free;
  end;
end;


procedure TPGNProcessor.FProceedPGN;
begin
  with TPGNTraverser.Create(Input, [PosBaseCollector, MoveTreeCollector]) do
  try
    ProceedColors := m_Color;
    PlayerName := m_strPlayerName;
    IncludeVariants := m_bVariants;

    Traverse;

    writeln('Games viewed: ', NumberOfGamesViewed);
    writeln('Positions viewed: ', NumberOfPositionsViewed);
  finally
    Free;
  end;
  
end;

end.
