////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit MoveHintsChessBoardLayerUnit;

interface

uses
  Classes,
  //
  ChessBoardHeaderUnit, ChessBoardUnit, ChessRulesEngine;

type
  TPlyHintPrecision = (phpNone, phpLow, phpMid, phpFull);

  TPlyHintData = class
  private
    m_MoveAbs: TMoveAbs;
    m_bMainLineFlag: boolean;
    m_bNextPlyOfLineFlag: boolean;
    m_bShow: boolean;
    m_PlyHintPrecision: TPlyHintPrecision;
    constructor FCreate(AMoveAbs: PMoveAbs);
    function FGetMoveAbs: PMoveAbs;
    procedure FIncreaseHintPrecision;
    property MoveAbs: PMoveAbs read FGetMoveAbs;
    property MainLineFlag: boolean read m_bMainLineFlag write m_bMainLineFlag;
    property NextPlyOfLineFlag: boolean read m_bNextPlyOfLineFlag write m_bNextPlyOfLineFlag;
    property Show: boolean read m_bShow write m_bShow;
    property PlyHintPrecision: TPlyHintPrecision read m_PlyHintPrecision;
  public
    constructor Create;
  end;

  // Layer for showing move hints
  TMoveHintsChessBoardLayer = class(TChessBoardLayerBase)
  private
    m_ChessRulesEngine: TChessRulesEngine;
    m_bHasDataFlag: boolean;
    m_strlPlys: TStringList;
    procedure FClearPlysList;
    function FGetPlyHintData(const strPly: string): TPlyHintData; overload;
    function FGetPlyHintData(iPlyIndex: integer): TPlyHintData; overload;
    procedure FSetPlyHintData(const strPly: string; AData: TPlyHintData);
    procedure FDrawPlyHint(const APlyHintData: TPlyHintData);
  protected
    procedure RDraw; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetData(const strFEN, strUserPly: string; const NextPlys: TStrings;
      iMainLinePlyIndex: integer);
    procedure UnsetData;
    procedure Reset;
  end;


implementation

uses
   Graphics, Math, SysUtils;

////////////////////////////////////////////////////////////////////////////////
// TMoveHintsChessBoardLayer

constructor TMoveHintsChessBoardLayer.Create;
begin
  inherited;

  m_ChessRulesEngine := TChessRulesEngine.Create;
  m_ChessRulesEngine.MoveNotationFormat := mnfCh4NEx;
  m_ChessRulesEngine.FENFormat := TRUE;

  m_strlPlys := TStringList.Create;
end;


destructor TMoveHintsChessBoardLayer.Destroy;
begin
  FClearPlysList;
  m_strlPlys.Free;

  m_ChessRulesEngine.Free;

  inherited;
end;


procedure TMoveHintsChessBoardLayer.FClearPlysList;
var
  i: integer;
  TmpObj: TObject;
begin
  for i := 0 to m_strlPlys.Count - 1 do
  begin
    TmpObj := m_strlPlys.Objects[i];
    m_strlPlys.Objects[i] := nil;
    TmpObj.Free;
  end;

  m_strlPlys.Clear;          
end;


procedure TMoveHintsChessBoardLayer.RDraw;
var
  i: integer;
  PlyHintData: TPlyHintData;
begin
  if (not m_bHasDataFlag) then
    exit;

  if (not (Assigned(ChessBoard) and Assigned(Canvas))) then
    exit;

  for i := m_strlPlys.Count - 1 downto 0 do
  begin
    PlyHintData := FGetPlyHintData(i);
    FDrawPlyHint(PlyHintData);
  end;

  // TODO: improve queue of drawing
end;


procedure TMoveHintsChessBoardLayer.FDrawPlyHint(const APlyHintData: TPlyHintData);

  procedure NDrawBase(i, j: integer);
  var
    iX0, iY0, iX, iY: integer;
  begin
    iX0 := CHB_X + SquareSize * i;
    iY0 := CHB_Y + SquareSize * j;
    iX := iX0 + SquareSize;
    iY := iY0 + SquareSize;

    Canvas.Ellipse(iX0, iY0, iX, iY);
  end;

  procedure NDrawPin(i0, j0, i, j: integer);
  const
    PIN_INDENT = 7;
  var
    iX0, iY0, iX, iY: integer;
    dRad, dCos, dSin: Double;  
  begin
    iX0 := CHB_X + SquareSize * i0;
    iY0 := CHB_Y + SquareSize * j0;
    iX := iX0 + SquareSize;
    iY := iY0 + SquareSize;

    dRad := sqrt(sqr(i - i0) + sqr(j - j0));
    dCos := (i - i0) / dRad;
    dSin := (j - j0) / dRad;

    dRad := PIN_INDENT;

    iX0 := Round((iX0 + iX + dCos * SquareSize) / 2);
    iY0 := Round((iY0 + iY + dSin * SquareSize) / 2);
    iX := iX0 + Round(dRad * dCos);
    iY := iY0 + Round(dRad * dSin);

    Canvas.MoveTo(iX0, iY0);
    Canvas.LineTo(iX, iY);
  end;

  procedure NDrawArrow(i0, j0, i, j: integer);
  const
    ARROW_END_LENGTH = 10; // in pixels
    ARROW_END_ANGLE = 15 * (Pi / 180); // angle of the arrow top
    ARROW_INDENT = 7;
  var
    iX0, iY0, iX, iY: integer;
    dRad, dCos, dSin: Double;
    iXA, iYA: integer;  
  begin
    iX0 := CHB_X + SquareSize * i0;
    iY0 := CHB_Y + SquareSize * j0;
    iX := iX0 + SquareSize;
    iY := iY0 + SquareSize;

    dRad := sqrt(sqr(i - i0) + sqr(j - j0));
    dCos := (i - i0) / dRad;
    dSin := (j - j0) / dRad;

    dRad := SquareSize * (dRad - 0.5) - ARROW_INDENT;

    iX0 := Round((iX0 + iX + dCos * SquareSize) / 2);
    iY0 := Round((iY0 + iY + dSin * SquareSize) / 2);
    iX := iX0 + Round(dRad * dCos);
    iY := iY0 + Round(dRad * dSin);

    Canvas.MoveTo(iX0, iY0);
    Canvas.LineTo(iX, iY);

    iXA := iX + Round((-ARROW_END_LENGTH * cos(ARROW_END_ANGLE)) * dCos -
                       (ARROW_END_LENGTH * sin(ARROW_END_ANGLE)) * dSin);
    iYA := iY + Round((-ARROW_END_LENGTH * cos(ARROW_END_ANGLE)) * dSin +
                       (ARROW_END_LENGTH * sin(ARROW_END_ANGLE)) * dCos);

    Canvas.LineTo(iXA, iYA);

    iXA := iX + Round((-ARROW_END_LENGTH * cos(ARROW_END_ANGLE)) * dCos -
                      (-ARROW_END_LENGTH * sin(ARROW_END_ANGLE)) * dSin);
    iYA := iY + Round((-ARROW_END_LENGTH * cos(ARROW_END_ANGLE)) * dSin +
                      (-ARROW_END_LENGTH * sin(ARROW_END_ANGLE)) * dCos);

    Canvas.MoveTo(iX, iY);
    Canvas.LineTo(iXA, iYA);
  end;

var
  i0, j0, i, j: integer;
begin // .FDrawPlyHint
  if (not APlyHintData.Show) then
    exit;

  with APlyHintData do
  begin
    if (not ChessBoard.Flipped) then
    begin
      i0 := MoveAbs.i0 - 1;
      j0 := 8 - MoveAbs.j0;
      i := MoveAbs.i - 1;
      j := 8 - MoveAbs.j;
    end
    else
    begin
      i0 := 8 - MoveAbs.i0;
      j0 := MoveAbs.j0 - 1;
      i := 8 - MoveAbs.i;
      j := MoveAbs.j - 1;
    end;
  end;

  Canvas.Pen.Style := psSolid;

  if (APlyHintData.MainLineFlag or APlyHintData.NextPlyOfLineFlag) then
    Canvas.Pen.Width := 2
  else
    Canvas.Pen.Width := 1;

  if (APlyHintData.MainLineFlag) then
    Canvas.Pen.Color := clRed
  else
    Canvas.Pen.Color := clOlive;


  case APlyHintData.PlyHintPrecision of
    phpLow:
      NDrawBase(i0, j0);

    phpMid:
    begin
      NDrawBase(i0, j0);
      NDrawPin(i0, j0, i, j);
    end;

    phpFull:
    begin
      NDrawBase(i0, j0);
      NDrawArrow(i0, j0, i, j);
    end;

  else
    Assert(FALSE);
  end;

end;


procedure TMoveHintsChessBoardLayer.SetData(const strFEN,
  strUserPly: string; const NextPlys: TStrings; iMainLinePlyIndex: integer);
var
  i: integer;
  UserMoveAbs: TMoveAbs;
  strPly: string;
  PlyHintData: TPlyHintData;
  bRes: boolean;
  iAmountOfPlysToShow: integer;
begin
  m_bHasDataFlag := ((NextPlys.Count > 0) and (NextPlys.IndexOf(strFEN) < 0));
  if (not m_bHasDataFlag) then
  begin
    RDoUpdate;
    exit;
  end;

  if (m_ChessRulesEngine.GetPosition <> strFEN) then
  begin
    FClearPlysList;
    m_ChessRulesEngine.SetPosition(strFEN);
  end;

  bRes := m_ChessRulesEngine.DoMove(strUserPly);
  Assert(bRes);
  UserMoveAbs := m_ChessRulesEngine.lastMove^;
  m_ChessRulesEngine.TakeBack;

  iAmountOfPlysToShow := 0;
  
  for i := 0 to NextPlys.Count - 1 do
  begin
    strPly := NextPlys[i];

    PlyHintData := FGetPlyHintData(strPly);

    if (not Assigned(PlyHintData)) then
    begin
      bRes := m_ChessRulesEngine.DoMove(strPly);
      Assert(bRes);

      PlyHintData := TPlyHintData.FCreate(m_ChessRulesEngine.lastMove);
      FSetPlyHintData(strPly, PlyHintData);

      m_ChessRulesEngine.TakeBack;
    end;

    PlyHintData.NextPlyOfLineFlag := (i = 0);
    PlyHintData.MainLineFlag := (i = iMainLinePlyIndex);

    if ((PlyHintData.MoveAbs.i0 = UserMoveAbs.i0) and (PlyHintData.MoveAbs.j0 = UserMoveAbs.j0)) then
    begin
      PlyHintData.Show := TRUE;
      inc(iAmountOfPlysToShow);
    end
    else
      PlyHintData.Show := FALSE;
  end;

  for i := 0 to m_strlPlys.Count - 1 do
  begin
    PlyHintData := FGetPlyHintData(i);

    if (iAmountOfPlysToShow = 0) then
      PlyHintData.Show := TRUE;

    if (PlyHintData.Show) then
      PlyHintData.FIncreaseHintPrecision;
  end;

  RDoUpdate;
end;


function TMoveHintsChessBoardLayer.FGetPlyHintData(const strPly: string): TPlyHintData;
var
  iIndex: integer;
begin
  iIndex := m_strlPlys.IndexOf(strPly);
  if (iIndex >= 0) then
    Result := FGetPlyHintData(iIndex)
  else
    Result := nil;
end;


function TMoveHintsChessBoardLayer.FGetPlyHintData(iPlyIndex: integer): TPlyHintData;
begin
  Result := m_strlPlys.Objects[iPlyIndex] as TPlyHintData
end;


procedure TMoveHintsChessBoardLayer.FSetPlyHintData(const strPly: string; AData: TPlyHintData);
var
  iIndex: integer;
  TmpObj: TObject;
begin
  iIndex := m_strlPlys.IndexOf(strPly);
  if (iIndex >= 0) then
  begin
    TmpObj := m_strlPlys.Objects[iIndex];
    m_strlPlys.Objects[iIndex] := AData;
    TmpObj.Free;
  end
  else
    m_strlPlys.AddObject(strPly, AData); 
end;


procedure TMoveHintsChessBoardLayer.UnsetData;
begin
  FClearPlysList;
  m_bHasDataFlag := FALSE;
end;


procedure TMoveHintsChessBoardLayer.Reset;
begin
  if (not m_bHasDataFlag) then
    exit;

  UnsetData;    

  RDoUpdate;
end;

////////////////////////////////////////////////////////////////////////////////
// TPlyHintData

constructor TPlyHintData.Create;
begin
  raise Exception.Create('TPlyHintData cannot be instantiated directly!');
end;


constructor TPlyHintData.FCreate(AMoveAbs: PMoveAbs);
begin
  inherited Create;

  m_MoveAbs := AMoveAbs^;
end;


function TPlyHintData.FGetMoveAbs: PMoveAbs;
begin
  Result := @m_MoveAbs;
end;


procedure TPlyHintData.FIncreaseHintPrecision;
begin
  if (m_PlyHintPrecision < phpFull) then
    m_PlyHintPrecision := Succ(m_PlyHintPrecision);
end;

end.
