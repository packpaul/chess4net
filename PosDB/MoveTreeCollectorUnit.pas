////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit MoveTreeCollectorUnit;

interface

uses
  SysUtils, Contnrs,
  //
  PGNTraverserUnit, ChessRulesEngine, MoveTreeBaseUnit;

type
  EMoveTreeCollector = class(Exception);

  TMoveTreeCollector = class(TInterfacedObject, IPGNTraverserVisitable)
  private
    m_DataBase: TMoveTreeBase;
    m_bOwnsDataBase: boolean;
    m_DataBags: TObjectList;
    m_bCollecting: boolean;

    procedure FAddDataToBase;

  protected
    constructor Create(ADataBase: TMoveTreeBase); overload;

  public
    constructor Create(const strBaseName: string); overload;
    destructor Destroy; override;

    procedure Start(const Visitor: IPGNTraverserVisitor);
    procedure DoPosMove(iPlyNumber: integer; const APosMove: TPosMove;
      const AResultingPos: TChessPosition);
    procedure StartLine(bFromPreviousPos: boolean);
    procedure EndLine;
    procedure Finish;

    property DataBase: TMoveTreeBase read m_DataBase;
  end;

implementation

type
  TMoveAbsItem = class
  private
    m_Move: TMoveAbs;
  public
    constructor Create(const AMove: TMoveAbs);
    property Move: TMoveAbs read m_Move;
  end;

////////////////////////////////////////////////////////////////////////////////
// TMoveTreeCollector

constructor TMoveTreeCollector.Create(ADataBase: TMoveTreeBase);
begin
  inherited Create;
  m_DataBase := ADataBase;
  m_DataBags := TObjectList.Create;
end;


constructor TMoveTreeCollector.Create(const strBaseName: string);
begin
  Create(TMoveTreeBase.Create(strBaseName));
  m_bOwnsDataBase := TRUE;
end;


destructor TMoveTreeCollector.Destroy;
begin
  m_DataBags.Free;
  if (m_bOwnsDataBase) then
    m_DataBase.Free;
    
  inherited;
end;


procedure TMoveTreeCollector.Start(const Visitor: IPGNTraverserVisitor);
begin
  if (m_bCollecting) then
    raise EMoveTreeCollector.Create('Incorrect state!');

  m_DataBags.Clear;

  m_bCollecting := TRUE;
end;


procedure TMoveTreeCollector.DoPosMove(iPlyNumber: integer; const APosMove: TPosMove;
  const AResultingPos: TChessPosition);
begin
  m_DataBags.Add(TMoveAbsItem.Create(APosMove.move));
end;


procedure TMoveTreeCollector.StartLine(bFromPreviousPos: boolean);
begin
  raise EMoveTreeCollector.Create('TMoveTreeCollector.StartLine cannot be called!');
end;


procedure TMoveTreeCollector.EndLine;
begin
  raise EMoveTreeCollector.Create('TMoveTreeCollector.EndLine cannot be called!');
end;


procedure TMoveTreeCollector.Finish;
begin
  if (not m_bCollecting) then
    raise EMoveTreeCollector.Create('Incorrect state!');

  FAddDataToBase;

  m_bCollecting := FALSE;
end;


procedure TMoveTreeCollector.FAddDataToBase;
var
  arrMoves: TMoveAbsArr;
  i: integer;
begin
  SetLength(arrMoves, m_DataBags.Count);
  for i := 0 to m_DataBags.Count - 1 do
    arrMoves[i] := TMoveAbsItem(m_DataBags[i]).Move;

  m_DataBase.Add(arrMoves);
end;

////////////////////////////////////////////////////////////////////////////////
// TMoveAbsItem

constructor TMoveAbsItem.Create(const AMove: TMoveAbs);
begin
  inherited Create;
  m_Move := AMove;
end;

end.
