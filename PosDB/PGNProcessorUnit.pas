////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit PGNProcessorUnit;

interface

uses
  PGNTraverserUnit, PosBaseCollectorUnit;

type
  TPGNProcessor = class
  private
    constructor FCreate;
    procedure FProceedPGN(const strBasename: string; bVariants, bChngest: boolean; bUniquePos: boolean;
      const color: TFigureColors; iNumPlys: integer; const strPlayerName: string;
      opening: TOpening; bStatPrunning: boolean; strRefBaseName: string; bMoveTreeDB: boolean);

  public
    constructor Create;
    class procedure Proceed(const basename: string; variants, chngest: boolean; uniquePos: boolean;
                            const color: TFigureColors; numPlys: integer; const player_name: string;
                            opening: TOpening; statPrunning: boolean; refBaseName: string;
                            bMoveTreeDB: boolean);
  end;

implementation

uses
  SysUtils,
  //
  MoveTreeCollectorUnit;

////////////////////////////////////////////////////////////////////////////////
// TPGNProcessor

constructor TPGNProcessor.Create;
begin
  raise Exception.Create('TPGNProcessor cannot be instantiated directly!');
end;


constructor TPGNProcessor.FCreate;
begin
  inherited Create;
end;


class procedure TPGNProcessor.Proceed(const basename: string; variants, chngest: boolean; uniquePos: boolean;
  const color: TFigureColors; numPlys: integer; const player_name: string;
  opening: TOpening; statPrunning: boolean; refBaseName: string; bMoveTreeDB: boolean);
begin
  with TPGNProcessor.FCreate do
  try
    FProceedPGN(basename, variants, chngest, uniquePos, color, numPlys, player_name,
      opening, statPrunning, refBaseName, bMoveTreeDB);
  finally
    Free;
  end;
end;


procedure TPGNProcessor.FProceedPGN(const strBasename: string; bVariants, bChngest: boolean; bUniquePos: boolean;
      const color: TFigureColors; iNumPlys: integer; const strPlayerName: string;
      opening: TOpening; bStatPrunning: boolean; strRefBaseName: string; bMoveTreeDB: boolean);
var
  PosBaseCollector: TPosBaseCollector;
  PGNCollector: IPGNTraverserVisitable;
begin
  if (not bMoveTreeDB) then
  begin
    PosBaseCollector := TPosBaseCollector.Create(strBasename, strRefBaseName);
    PosBaseCollector.ProceedColors := color;
    PosBaseCollector.PlayerName := strPlayerName;
    PosBaseCollector.ChangeEstimation := bChngest;
    PosBaseCollector.UseUniquePositions := bUniquePos;
    PosBaseCollector.GeneratedOpening := opening;
    PosBaseCollector.UseStatisticalPrunning := bStatPrunning;
    PosBaseCollector.UseNumberOfPlys := iNumPlys;

    PGNCollector := PosBaseCollector;
  end
  else
  begin
    PGNCollector := TMoveTreeCollector.Create(strBasename);
  end;

  with TPGNTraverser.Create(Input, PGNCollector) do
  try
    ProceedColors := color;
    PlayerName := strPlayerName;
    IncludeVariants := bVariants;
    Traverse;
    writeln('Games viewed: ', NumberOfGamesViewed);
    writeln('Positions viewed: ', NumberofPositionsViewed);
  finally
    Free;
  end;

end;

end.
