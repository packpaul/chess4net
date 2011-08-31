unit GamesManagerUnit;

interface

uses
  SysUtils, Classes, TntClasses;

type
  TGamesManager = class
  private
    m_wstrlData: TTntStringList;
    procedure FParseFile;
  public
    constructor Create;
    destructor Destroy; override;

    function LoadFromFile(const AFileName: TFileName): boolean;
    procedure GetGameData(iGameNumber: integer; out Data: TTntStrings); // 0-based
    function GetGamesCount: integer;
  end;

implementation

////////////////////////////////////////////////////////////////////////////////
// TGamesManager

constructor TGamesManager.Create;
begin
  inherited Create;
  m_wstrlData := TTntStringList.Create;
end;


destructor TGamesManager.Destroy;
begin
  m_wstrlData.Free;
  inherited;
end;


function TGamesManager.LoadFromFile(const AFileName: TFileName): boolean;
begin
  Result := FALSE;

  m_wstrlData.LoadFromFile(AFileName);
  FParseFile;

  // TODO:

  Result := TRUE;
end;


procedure TGamesManager.FParseFile;
begin
  // TODO:
end;


procedure TGamesManager.GetGameData(iGameNumber: integer; out Data: TTntStrings);
begin
  Data := TTntStringList.Create;
  Data.Assign(m_wstrlData);
end;


function TGamesManager.GetGamesCount: integer;
begin
  Result := 1;
end;

end.
