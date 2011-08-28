////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit PosBaseChessBoardUnit;

interface

uses
  Classes,
  //
  ChessBoardUnit, PosBaseChessBoardLayerUnit;

type
  TPosBaseChessBoard = class(TChessBoard)
  private
    m_Layer: TPosBaseChessBoardLayer;

    function FGetTrainingMode: boolean;
    procedure FSetTrainingMode(bEnabled: boolean);
    function FGetUseUserBase: boolean;
    procedure FSetUseUserBase(bValue: boolean);

  public
    constructor Create(voOwner: TComponent; vfHandler: TChessBoardHandler;
      const strPosBaseName: string = '');
    destructor Destroy; override;
    procedure WriteGameToBase(AGameResult: TGameResult);
    procedure SetExternalBase(const strExtPosBaseName: string);
    procedure UnsetExternalBase;
    property pTrainingMode: boolean read FGetTrainingMode write FSetTrainingMode;
    property pUseUserBase: boolean read FGetUseUserBase write FSetUseUserBase;
  end;

implementation

////////////////////////////////////////////////////////////////////////////////
// TPosBaseChessBoard

constructor TPosBaseChessBoard.Create(voOwner: TComponent; vfHandler: TChessBoardHandler;
  const strPosBaseName: string = '');
begin
  inherited Create(voOwner, vfHandler);

  m_Layer := TPosBaseChessBoardLayer.Create(strPosBaseName);
  Layer := m_Layer;
end;


destructor TPosBaseChessBoard.Destroy;
begin
  pTrainingMode := FALSE;
  m_Layer.Free;

  inherited;
end;


function TPosBaseChessBoard.FGetTrainingMode: boolean;
begin
  Result := m_Layer.TrainingMode;
end;


procedure TPosBaseChessBoard.FSetTrainingMode(bEnabled: boolean);
begin
   m_Layer.TrainingMode := bEnabled;
end;


function TPosBaseChessBoard.FGetUseUserBase: boolean;
begin
  Result := m_Layer.UseUserBase;
end;


procedure TPosBaseChessBoard.FSetUseUserBase(bValue: boolean);
begin
  m_Layer.UseUserBase := bValue;
end;


procedure TPosBaseChessBoard.SetExternalBase(const strExtPosBaseName: string);
begin
  m_Layer.SetExternalBase(strExtPosBaseName); 
end;


procedure TPosBaseChessBoard.WriteGameToBase(AGameResult: TGameResult);
begin
  m_Layer.WriteGameToBase(AGameResult);
end;


procedure TPosBaseChessBoard.UnsetExternalBase;
begin
  m_Layer.UnsetExternalBase;
end;

end.
