unit MoveTreeBaseUnit;

interface

uses
  Classes, SysUtils, Contnrs,
  //
  ChessRulesEngine;

type
  PDataBag = ^TDataBag;
  TDataBag = packed object
  public
    btFirst: byte;
    btSecond: byte;
  private
    function FEquals(const Other: TDataBag): boolean;
    procedure FConvertFromMove(const AMove: TMoveAbs);
    function FIsMove: boolean;
    function FToMove: TMoveAbs;
    function FConvertFromNearPointer(lwPointer: LongWord; out Data: TDataBag): boolean;
    function FIsNearPointer: boolean;
    function FToNearPointer: Word;
    function FConvertFromFarPointer(lwPointer: LongWord; out HiData, LowData: TDataBag): boolean;
    function FIsFarPointer: boolean;
    function FToFarPointer(const HiData, LowData: TDataBag): LongWord;
    function FIsEndDataTag: boolean;
  end;

  TMoveAbsArr = array of TMoveAbs;

  TDataBagsIterator = class
  private
    m_Moves: TMoveAbsArr;
    m_iIndex: integer;
    constructor FCreate(const Moves: TMoveAbsArr);
  public
    constructor Create;
    function HasNext: boolean;
    function GetNext: TDataBag;
    function GetLast: TDataBag;
  end;

  TInsertionPoint = object
  public
    lwAddress1: LongWord;
    lwAddress2: LongWord;
  private
    procedure FInit(lwAddress: LongWord); overload;
    procedure FInit(lwAAddress1, lwAAddress2: LongWord); overload;
  end;

  TMoveTreeAddress = object
  public
    strPos: string;
    lwPosition: LongWord;
    wOffset: Word;
  private
    procedure FInit(const strAPos: string; lwAPosition: LongWord; wAOffset: Word);
  end;

  TMoveTreeBaseCache = class
  private
    m_InitialPos: TChessPosition;
    m_InitialAddress: TMoveTreeAddress;
    m_PosAddresses: TObjectList;
    m_iLastItemIndex: integer; // PP: optimization?
    constructor FCreate(const InitialPos: TChessPosition; const InitialAddress: TMoveTreeAddress);
    function FGet(const Pos: TChessPosition; out Address: TMoveTreeAddress): boolean;
    procedure FAdd(const Pos: TChessPosition; const Address: TMoveTreeAddress);
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TMovePosAddress = object
  public
    move: TMoveAbs;
    pos: TChessPosition;
    address: TMoveTreeAddress;
  private
    procedure FInit(const AMove: TMoveAbs; const APos: TChessPosition;
      const AAddress: TMoveTreeAddress);
  end;

  TMovePosAddressArr = array of TMovePosAddress;

  EMoveTreeBase = class(Exception);

  TMoveTreeBase = class
  private
    m_BaseStream: TStream;
    m_ChessRulesEngine: TChessRulesEngine;
    m_PosCache: TMoveTreeBaseCache;

    constructor FCreate;

    class function FGetBaseFileName(const strBaseName: string): TFileName;

    procedure FCreateFileStream(const BaseFileName: TFileName);
    procedure FCreateMemoryStream;
    procedure FDestroyStream;

    function FReadBagFromStream(out ABag: TDataBag): boolean; overload;
    function FReadBagFromStream(lwPosition: LongWord; out ABag: TDataBag): boolean; overload;
    procedure FWriteBagToStream(const ABag: TDataBag); overload;
    procedure FWriteBagToStream(lwPosition: LongWord; const ABag: TDataBag); overload;
    procedure FWriteBagToStreamEnd(const ABag: TDataBag);

    function FFindData(lwPosition: LongWord; const DataIterator: TDataBagsIterator;
      out InsertionPoint: TInsertionPoint): boolean; overload;
    procedure FFindData(const Address: TMoveTreeAddress; out Datas: TMovePosAddressArr); overload;

    procedure FSaveData(const InsertionPoint: TInsertionPoint; const DataIterator: TDataBagsIterator);
    procedure FStartNearBranch(const InsertionPoint: TInsertionPoint; const Data: TDataBag);
    procedure FStartFarBranch(const InsertionPoint: TInsertionPoint; const DataHi, DataLow: TDataBag);

    property ChessRulesEngine: TChessRulesEngine read m_ChessRulesEngine;

  protected
    constructor CreateForTest;
    constructor CreateForTestFarJump;

  public
    constructor Create(const strBaseName: string);
    destructor Destroy; override;
    class function Exists(const strBaseName: string): boolean;
    procedure Add(const Moves: TMoveAbsArr);
    procedure Find(const Pos: TChessPosition; out Moves: TMoveAbsArr);
  end;

const
  _TDataBag: PDataBag = nil; // helper

implementation

type
  TPosAddressItem = class
  private
    m_Pos: TChessPosition;
    m_Address: TMoveTreeAddress;
  public
    constructor Create(const APos: TChessPosition; const AAddress: TMoveTreeAddress);
    property Pos: TChessPosition read m_Pos;
    property Address: TMoveTreeAddress read m_Address;
  end;

  TDataFinder = class
  private
    m_Base: TMoveTreeBase;
    m_lwPosition: LongWord;
    m_lwLastPosition: LongWord;
    function FReadBagFromStream(out ABag: TDataBag): boolean; overload;
    function FReadBagFromStream(lwPosition: LongWord; out ABag: TDataBag): boolean; overload;
    function FJumpNear(const Data: TDataBag; out NextDataBag: TDataBag): boolean;
    function FJumpFar(const HiData: TDataBag; out NextDataBag: TDataBag): boolean;
    property Base: TMoveTreeBase read m_Base;
  protected
    function RFind(lwPosition: LongWord): boolean;
    function RIsDataFromJump: boolean;
    
    function RF1(const DataBag: TDataBag): boolean; virtual; abstract;
    function RF2(const DataBag: TDataBag): boolean; virtual; abstract;
    function RF3: boolean; virtual; abstract;
    procedure RP4; virtual;

    property Position: LongWord read m_lwPosition;
    property LastPosition: LongWord read m_lwLastPosition;
  public
    constructor Create(ABase: TMoveTreeBase);
  end;

  TInsertionPointDataFinder = class(TDataFinder)
  private
    m_DataIterator: TDataBagsIterator;
    m_InsertionPoint: TInsertionPoint;
  protected
    function RF1(const DataBag: TDataBag): boolean; override;
    function RF2(const DataBag: TDataBag): boolean; override;
    function RF3: boolean; override;
  public
    function Find(lwPosition: LongWord; const DataIterator: TDataBagsIterator;
      out InsertionPoint: TInsertionPoint): boolean;
  end;

  TNextLinesBuilderDataFinder = class(TDataFinder)
  private
    m_Datas: TMovePosAddressArr;
    m_wMovesCount: Word;
    m_Address: TMoveTreeAddress;
    function FGetChessRulesEngine: TChessRulesEngine;
    procedure FCollectDatas;
    function FF5(const DataBag: TDataBag; bJump: boolean): boolean;
    property ChessRulesEngine: TChessRulesEngine read FGetChessRulesEngine;
  protected
    function RF1(const DataBag: TDataBag): boolean; override;
    function RF2(const DataBag: TDataBag): boolean; override;
    function RF3: boolean; override;
    procedure RP4; override;
  public
    procedure Find(const Address: TMoveTreeAddress; out Datas: TMovePosAddressArr);
  end;

const
  BASE_FILE_EXT = 'mvt';

  PROM_FIG_MARKER: array[TFigureName] of byte = ($00, $00, $40, $80, $C0, $00); // K, Q, R, B, N, P

  MOVE_DATA_MARKER = $00;
  MOVE_DATA_MASK = $3F;
  NEAR_POINTER_DATA_MARKER = $80;
  NEAR_POINTER_DATA_MASK = NEAR_POINTER_DATA_MARKER - 1;
  FAR_POINTER_DATA_MARKER = $40;
  FAR_POINTER_DATA_MASK = FAR_POINTER_DATA_MARKER - 1;
  DATA_KIND_MASK = MOVE_DATA_MARKER or NEAR_POINTER_DATA_MARKER or FAR_POINTER_DATA_MARKER;

  END_DATA_TAG: TDataBag = (btFirst: 0; btSecond: 0);

  INITIAL_ADDRESS: TMoveTreeAddress = (strPos: INITIAL_CHESS_POSITION; lwPosition: 0; wOffset: 0);

var
  g_bFarPointerTests: boolean = FALSE;

////////////////////////////////////////////////////////////////////////////////
// TMoveTreeBase

constructor TMoveTreeBase.FCreate;
begin
  inherited Create;

  m_ChessRulesEngine := TChessRulesEngine.Create;
  m_PosCache := TMoveTreeBaseCache.FCreate(m_ChessRulesEngine.Position^, INITIAL_ADDRESS);
end;


constructor TMoveTreeBase.Create(const strBaseName: string);
begin
  FCreate;
  FCreateFileStream(FGetBaseFileName(strBaseName));
end;


constructor TMoveTreeBase.CreateForTest;
begin
  FCreate;
  FCreateMemoryStream;
end;


constructor TMoveTreeBase.CreateForTestFarJump;
begin
  CreateForTest;
  g_bFarPointerTests := TRUE;  
end;


destructor TMoveTreeBase.Destroy;
begin
  FDestroyStream;
  
  m_PosCache.Free;
  m_ChessRulesEngine.Free;
  
  inherited;
end;


class function TMoveTreeBase.Exists(const strBaseName: string): boolean;
begin
  Result := FileExists(FGetBaseFileName(strBaseName));
end;


class function TMoveTreeBase.FGetBaseFileName(const strBaseName: string): TFileName;
begin
  Result := strBaseName + '.' + BASE_FILE_EXT;
end;


procedure TMoveTreeBase.FCreateFileStream(const BaseFileName: TFileName);
var
  FileHandle: THandle;
begin
  if (not FileExists(BaseFileName)) then
  begin
    FileHandle := FileCreate(BaseFileName);
    FileClose(FileHandle);
  end;

  m_BaseStream := TFileStream.Create(BaseFileName, fmOpenReadWrite, fmShareDenyWrite);
end;


procedure TMoveTreeBase.FCreateMemoryStream;
begin
  m_BaseStream := TMemoryStream.Create;
end;


procedure TMoveTreeBase.FDestroyStream;
begin
  FreeAndNil(m_BaseStream);
end;


function TMoveTreeBase.FReadBagFromStream(out ABag: TDataBag): boolean;
begin
  Result := (m_BaseStream.Read(ABag, SizeOf(ABag)) = SizeOf(ABag));
end;


function TMoveTreeBase.FReadBagFromStream(lwPosition: LongWord;
  out ABag: TDataBag): boolean;
begin
  m_BaseStream.Position := lwPosition;
  Result := (m_BaseStream.Read(ABag, SizeOf(ABag)) = SizeOf(ABag));
end;


procedure TMoveTreeBase.FWriteBagToStream(const ABag: TDataBag);
begin
  m_BaseStream.WriteBuffer(ABag, SizeOf(ABag));
end;


procedure TMoveTreeBase.FWriteBagToStream(lwPosition: LongWord; const ABag: TDataBag);
begin
  m_BaseStream.Position := lwPosition;
  m_BaseStream.WriteBuffer(ABag, SizeOf(ABag));
end;


procedure TMoveTreeBase.FWriteBagToStreamEnd(const ABag: TDataBag);
begin
  m_BaseStream.Seek(0, soEnd);
  m_BaseStream.WriteBuffer(ABag, SizeOf(ABag));
end;


procedure TMoveTreeBase.Add(const Moves: TMoveAbsArr);
var
  Iterator: TDataBagsIterator;
  InsertionPoint: TInsertionPoint;
begin
  if (Length(Moves) = 0) then
    exit;

  Iterator := TDataBagsIterator.FCreate(Moves);
  try
    if (not FFindData(0, Iterator, InsertionPoint)) then
      FSaveData(InsertionPoint, Iterator);
  finally
    Iterator.Free;
  end;
end;


function TMoveTreeBase.FFindData(lwPosition: LongWord;
  const DataIterator: TDataBagsIterator; out InsertionPoint: TInsertionPoint): boolean;
begin
  with TInsertionPointDataFinder.Create(self) do
  try
    Result := Find(lwPosition, DataIterator, InsertionPoint);
  finally
    Free;
  end;
end;


procedure TMoveTreeBase.FSaveData(const InsertionPoint: TInsertionPoint;
  const DataIterator: TDataBagsIterator);
var
  lwAddressOffset: LongWord;
  DataOffsetHi, DataOffsetLow: TDataBag;
begin
  lwAddressOffset := m_BaseStream.Size - (InsertionPoint.lwAddress2 - SizeOf(TDataBag));
  if (lwAddressOffset > 0) then
  begin
    if (_TDataBag.FConvertFromNearPointer(lwAddressOffset, DataOffsetLow)) then
      FStartNearBranch(InsertionPoint, DataOffsetLow)
    else if (_TDataBag.FConvertFromFarPointer(lwAddressOffset, DataOffsetHi, DataOffsetLow)) then
      FStartFarBranch(InsertionPoint, DataOffsetHi, DataOffsetLow)
    else
      raise EMoveTreeBase.Create('Base file has become too big!');
  end;

  FWriteBagToStreamEnd(DataIterator.GetLast);
  while (DataIterator.HasNext) do
    FWriteBagToStream(DataIterator.GetNext);
  FWriteBagToStream(END_DATA_TAG);
end;


procedure TMoveTreeBase.FStartNearBranch(const InsertionPoint: TInsertionPoint;
  const Data: TDataBag);
var
  SavedData: TDataBag;
  bRead: boolean;
begin
  bRead := FReadBagFromStream(InsertionPoint.lwAddress1, SavedData);
  Assert(bRead);

  FWriteBagToStreamEnd(SavedData);

  FWriteBagToStream(InsertionPoint.lwAddress1, Data);
end;


procedure TMoveTreeBase.FStartFarBranch(const InsertionPoint: TInsertionPoint;
  const DataHi, DataLow: TDataBag);
var
  SavedData1, SavedData2: TDataBag;
  bRead: boolean;
begin
  bRead := FReadBagFromStream(InsertionPoint.lwAddress1, SavedData1);
  Assert(bRead);

  bRead := FReadBagFromStream(InsertionPoint.lwAddress2, SavedData2);
  Assert(bRead);

  FWriteBagToStreamEnd(SavedData1);
  FWriteBagToStream(SavedData2);

  FWriteBagToStream(InsertionPoint.lwAddress1, DataHi);
  FWriteBagToStream(InsertionPoint.lwAddress2, DataLow);
end;


procedure TMoveTreeBase.Find(const Pos: TChessPosition; out Moves: TMoveAbsArr);
var
  Address: TMoveTreeAddress;
  Datas: TMovePosAddressArr;
  i: integer;
begin
  if (not m_PosCache.FGet(Pos, Address)) then
  begin
    SetLength(Moves, 0);
    exit;
  end;

  FFindData(Address, Datas);

  SetLength(Moves, Length(Datas));
  for i := Low(Datas) to High(Datas) do
  begin
    Moves[i] := Datas[i].move;
    m_PosCache.FAdd(Datas[i].pos, Datas[i].address);
  end;
end;


procedure TMoveTreeBase.FFindData(const Address: TMoveTreeAddress; out Datas: TMovePosAddressArr);
begin
  with TNextLinesBuilderDataFinder.Create(self) do
  try
    Find(Address, Datas);
  finally
    Free;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// TDataBagsIterator

constructor TDataBagsIterator.Create;
begin
  raise Exception.Create(ClassName + ' cannot be instaniated directly!');
end;


constructor TDataBagsIterator.FCreate(const Moves: TMoveAbsArr);
begin
  inherited Create;
  m_Moves := Moves;
end;


function TDataBagsIterator.HasNext: boolean;
begin
  Result := (m_iIndex < Length(m_Moves));
end;


function TDataBagsIterator.GetNext: TDataBag;
begin
  Result.FConvertFromMove(m_Moves[m_iIndex]);
  inc(m_iIndex)
end;


function TDataBagsIterator.GetLast: TDataBag;
begin
  Result.FConvertFromMove(m_Moves[m_iIndex - 1]);
end;

////////////////////////////////////////////////////////////////////////////////
// TDataBag

procedure TDataBag.FConvertFromMove(const AMove: TMoveAbs);
begin
  btFirst := ((AMove.j0 - 1) shl 3) or (AMove.i0 - 1);
  btSecond := (((AMove.j - 1) shl 3) or (AMove.i - 1)) or PROM_FIG_MARKER[AMove.prom_fig];
end;


function TDataBag.FToMove: TMoveAbs;
const
  COORD_MASK = $07;
  PROM_FIG_MASK = $C0;
var
  f: TFigureName;
begin
  Result.i0 := (btFirst and COORD_MASK) + 1;
  Result.j0 := ((btFirst shr 3) and COORD_MASK) + 1;
  Result.i := (btSecond and COORD_MASK) + 1;
  Result.j := ((btSecond shr 3) and COORD_MASK) + 1;

  for f := Q to N do
  begin
    if (PROM_FIG_MARKER[f] = (btSecond and PROM_FIG_MASK)) then
    begin
      Result.prom_fig := f;
      break;
    end;
  end;
end;


function TDataBag.FEquals(const Other: TDataBag): boolean;
begin
  Result := ((btFirst = Other.btFirst) and (btSecond = Other.btSecond));
end;


function TDataBag.FIsMove: boolean;
const
  _DATA_KIND_MASK = (DATA_KIND_MASK xor MOVE_DATA_MASK) and DATA_KIND_MASK;
begin
  Result := (((btFirst and _DATA_KIND_MASK) = MOVE_DATA_MARKER) and
             ((btFirst and MOVE_DATA_MASK) <> btSecond));
end;


function TDataBag.FIsNearPointer: boolean;
const
  _DATA_KIND_MASK = (DATA_KIND_MASK xor NEAR_POINTER_DATA_MASK) and DATA_KIND_MASK;
begin
  Result := ((btFirst and _DATA_KIND_MASK) = NEAR_POINTER_DATA_MARKER);
end;


function TDataBag.FIsFarPointer: boolean;
const
  _DATA_KIND_MASK = (DATA_KIND_MASK xor FAR_POINTER_DATA_MASK) and DATA_KIND_MASK;
begin
  Result := ((btFirst and _DATA_KIND_MASK) = FAR_POINTER_DATA_MARKER);
end;


function TDataBag.FToNearPointer: Word;
begin
  Result := ((btFirst and NEAR_POINTER_DATA_MASK) shl 8) or btSecond;
end;


function TDataBag.FToFarPointer(const HiData, LowData: TDataBag): LongWord;
begin
  Result := ((((((HiData.btFirst and FAR_POINTER_DATA_MASK) shl 8) or HiData.btSecond) shl 8) or
              LowData.btFirst) shl 8) or LowData.btSecond;
end;


function TDataBag.FConvertFromFarPointer(lwPointer: LongWord; out HiData, LowData: TDataBag): boolean;
const
  MAX_VALUE = (FAR_POINTER_DATA_MARKER shl 24) - 1;
begin
  Result := (lwPointer <= MAX_VALUE);
  if (not Result) then
    exit;

  LowData.btSecond := lwPointer and $FF;
  LowData.btFirst := (lwPointer shr 8) and $FF;
  HiData.btSecond := (lwPointer shr 16) and $FF;
  HiData.btFirst := ((lwPointer shr 24) and FAR_POINTER_DATA_MASK) or FAR_POINTER_DATA_MARKER;
end;


function TDataBag.FConvertFromNearPointer(lwPointer: LongWord; out Data: TDataBag): boolean;
const
  MAX_VALUE = (NEAR_POINTER_DATA_MARKER shl 8) - 1;
begin
  Result := (lwPointer <= MAX_VALUE) and (not g_bFarPointerTests);
  if (not Result) then
    exit;

  Data.btSecond := lwPointer and $FF;
  Data.btFirst := ((lwPointer shr 8) and NEAR_POINTER_DATA_MASK) or NEAR_POINTER_DATA_MARKER;
end;


function TDataBag.FIsEndDataTag: boolean;
begin
  Result := ((btFirst = END_DATA_TAG.btFirst) and (btSecond = END_DATA_TAG.btSecond));
end;

////////////////////////////////////////////////////////////////////////////////
// TInsertionPoint

procedure TInsertionPoint.FInit(lwAddress: LongWord);
begin
  lwAddress1 := lwAddress;
  lwAddress2 := lwAddress;
end;

procedure TInsertionPoint.FInit(lwAAddress1, lwAAddress2: LongWord);
begin
  lwAddress1 := lwAAddress1;
  lwAddress2 := lwAAddress2;
end;

////////////////////////////////////////////////////////////////////////////////
// TMoveTreeBaseCache

constructor TMoveTreeBaseCache.Create;
begin
  raise Exception.Create(ClassName + ' cannot be instaniated directly!');
end;


constructor TMoveTreeBaseCache.FCreate(const InitialPos: TChessPosition;
  const InitialAddress: TMoveTreeAddress);
begin
  inherited Create;

  m_InitialPos := InitialPos;
  m_InitialAddress := InitialAddress;
  m_PosAddresses := TObjectList.Create;
end;


destructor TMoveTreeBaseCache.Destroy;
begin
  m_PosAddresses.Free;
  inherited;
end;


function TMoveTreeBaseCache.FGet(const Pos: TChessPosition; out Address: TMoveTreeAddress): boolean;
var
  i: integer;
  Item: TPosAddressItem;
begin
  Result := TRUE;

  if (m_InitialPos.Equals(Pos)) then
  begin
    Address := m_InitialAddress;
    exit;
  end;

  for i := m_iLastItemIndex to m_PosAddresses.Count - 1 do
  begin
    Item := TPosAddressItem(m_PosAddresses[i]);
    if (Pos.Equals(Item.Pos)) then
    begin
      Address := Item.Address;
      m_iLastItemIndex := i;
      exit;
    end;
  end;

  for i := m_iLastItemIndex - 1 downto 0 do
  begin
    Item := TPosAddressItem(m_PosAddresses[i]);
    if (Pos.Equals(Item.Pos)) then
    begin
      Address := Item.Address;
      m_iLastItemIndex := i;
      exit;
    end;
  end;

  Result := FALSE;
end;


procedure TMoveTreeBaseCache.FAdd(const Pos: TChessPosition; const Address: TMoveTreeAddress);
var
  DummyAddress: TMoveTreeAddress;
begin
  if (FGet(Pos, DummyAddress)) then
    exit;

  m_PosAddresses.Add(TPosAddressItem.Create(Pos, Address));
  m_iLastItemIndex := m_PosAddresses.Count - 1;
end;

////////////////////////////////////////////////////////////////////////////////
// TPosAddressItem

constructor TPosAddressItem.Create(const APos: TChessPosition; const AAddress: TMoveTreeAddress);
begin
  inherited Create;
  m_Pos := APos;
  m_Address := AAddress;
end;

////////////////////////////////////////////////////////////////////////////////
// TMoveTreeAddress

procedure TMoveTreeAddress.FInit(const strAPos: string; lwAPosition: LongWord; wAOffset: Word);
begin
  self.strPos := strAPos;
  self.lwPosition := lwAPosition;
  self.wOffset := wAOffset;
end;

////////////////////////////////////////////////////////////////////////////////
// TMovePosAddress

procedure TMovePosAddress.FInit(const AMove: TMoveAbs; const APos: TChessPosition;
  const AAddress: TMoveTreeAddress);
begin
  self.move := AMove;
  self.pos := APos;
  self.address := AAddress;
end;


////////////////////////////////////////////////////////////////////////////////
// TDataFinder

constructor TDataFinder.Create(ABase: TMoveTreeBase);
begin
  inherited Create;
  m_Base := ABase;
end;

function TDataFinder.FReadBagFromStream(out ABag: TDataBag): boolean;
begin
  Result := m_Base.FReadBagFromStream(ABag);
end;


function TDataFinder.FReadBagFromStream(lwPosition: LongWord; out ABag: TDataBag): boolean;
begin
  Result := m_Base.FReadBagFromStream(lwPosition, ABag);
end;


function TDataFinder.FJumpNear(const Data: TDataBag; out NextDataBag: TDataBag): boolean;
var
  lwPositionBase, lwJumpPosition: LongWord;
  DataBag: TDataBag;
  bRead: boolean;
begin
  lwPositionBase := m_lwPosition - SizeOf(TDataBag);

  lwJumpPosition := lwPositionBase + Data.FToNearPointer;

  FReadBagFromStream(lwJumpPosition, DataBag);
  Assert(DataBag.FIsMove);

  Result := RF1(DataBag);
  if (Result) then
  begin
    m_lwLastPosition := m_lwPosition;
    FReadBagFromStream(m_lwPosition, NextDataBag);
  end
  else
  begin
    bRead := FReadBagFromStream(NextDataBag);
    Assert(bRead);
    m_lwLastPosition := lwJumpPosition + SizeOf(TDataBag);
  end;

  m_lwPosition := m_lwLastPosition + SizeOf(TDataBag);

  if (not Result) then
    RP4;
end;


function TDataFinder.FJumpFar(const HiData: TDataBag; out NextDataBag: TDataBag): boolean;
var
  LowData: TDataBag;
  lwPositionBase, lwJumpPosition: LongWord;
  DataBag: TDataBag;
  bRead: boolean;
begin
  lwPositionBase := m_lwPosition - SizeOf(TDataBag);

  FReadBagFromStream(m_lwPosition, LowData);
  inc(m_lwPosition, SizeOf(TDataBag));

  lwJumpPosition := lwPositionBase + _TDataBag.FToFarPointer(HiData, LowData);

  FReadBagFromStream(lwJumpPosition, DataBag);
  Assert(DataBag.FIsMove);

  FReadBagFromStream(NextDataBag);
  m_lwLastPosition := lwJumpPosition + SizeOf(TDataBag);

  Result := RF1(DataBag);
  if (Result) then
    exit;

  bRead := FReadBagFromStream(NextDataBag);
  Assert(bRead);
  inc(m_lwLastPosition, SizeOf(TDataBag));

  m_lwPosition := m_lwLastPosition + SizeOf(TDataBag);

  RP4;
end;


function TDataFinder.RFind(lwPosition: LongWord): boolean;
var
  DataBagFromStream: TDataBag;
  bHasDataFlag: boolean;
begin
  Result := FALSE;

  m_lwPosition := lwPosition;
  m_lwLastPosition := lwPosition;

  if (not RF3) then
    exit;

  if (not FReadBagFromStream(m_lwPosition, DataBagFromStream)) then
    exit;

  bHasDataFlag := TRUE;
  inc(m_lwPosition, SizeOf(TDataBag));

  while (bHasDataFlag) do
  begin
    if (DataBagFromStream.FIsMove) then
    begin
      if (not RF2(DataBagFromStream)) then
        exit;
      if (RIsDataFromJump) then
        bHasDataFlag := FReadBagFromStream(m_lwPosition, DataBagFromStream)
      else
        bHasDataFlag := FReadBagFromStream(DataBagFromStream);

      Assert(bHasDataFlag);
      m_lwLastPosition := m_lwPosition;
      inc(m_lwPosition, SizeOf(TDataBag));
    end
    else if (DataBagFromStream.FIsNearPointer) then
    begin
      if (not FJumpNear(DataBagFromStream, DataBagFromStream)) then
        continue;
    end
    else if (DataBagFromStream.FIsFarPointer) then
    begin
      if (not FJumpFar(DataBagFromStream, DataBagFromStream)) then
        continue;
    end
    else if (DataBagFromStream.FIsEndDataTag) then
      break
    else
      Assert(FALSE);

    bHasDataFlag := RF3;
  end;

  Result := TRUE;

end;


function TDataFinder.RIsDataFromJump: boolean;
begin
  Result := (m_lwLastPosition > m_lwPosition)
end;


procedure TDataFinder.RP4;
begin
end;

////////////////////////////////////////////////////////////////////////////////
// TInsertionPointDataFinder

function TInsertionPointDataFinder.Find(lwPosition: LongWord;
  const DataIterator: TDataBagsIterator; out InsertionPoint: TInsertionPoint): boolean;
begin
  m_DataIterator := DataIterator;
  m_InsertionPoint.FInit(lwPosition, lwPosition + SizeOf(TDataBag));

  Result := RFind(lwPosition);

  InsertionPoint := m_InsertionPoint;
end;


function TInsertionPointDataFinder.RF1(const DataBag: TDataBag): boolean;
begin
  Result := DataBag.FEquals(m_DataIterator.GetLast);
end;


function TInsertionPointDataFinder.RF2(const DataBag: TDataBag): boolean;
begin
  Result := RF1(DataBag);
  if (not Result) then
    m_InsertionPoint.FInit(LastPosition, Position);
end;


function TInsertionPointDataFinder.RF3: boolean;
begin
  Result := m_DataIterator.HasNext;
  if (Result) then
    m_DataIterator.GetNext;
end;

////////////////////////////////////////////////////////////////////////////////
// TNextLinesBuilderDataFinder

procedure TNextLinesBuilderDataFinder.Find(const Address: TMoveTreeAddress;
  out Datas: TMovePosAddressArr);
begin
  SetLength(m_Datas, 0);

  m_Address := Address;
  ChessRulesEngine.SetPosition(m_Address.strPos);
  m_wMovesCount := m_Address.wOffset;
  m_Address.wOffset := 0;

  RFind(Address.lwPosition);

  Datas := m_Datas;
end;


function TNextLinesBuilderDataFinder.FF5(const DataBag: TDataBag; bJump: boolean): boolean;
var
  bRes: boolean;
begin
  with DataBag.FToMove do
    bRes := ChessRulesEngine.DoMove(i0, j0, i, j, prom_fig);
  Assert(bRes);

  if (bJump or RIsDataFromJump) then
    inc(m_Address.wOffset)
  else
    m_Address.FInit(ChessRulesEngine.GetPosition, Position, 0);

  Result := (m_wMovesCount > 0);
  if (Result)  then
    dec(m_wMovesCount)
  else
  begin
    FCollectDatas;
    ChessRulesEngine.TakeBack;
  end;
end;


function TNextLinesBuilderDataFinder.RF1(const DataBag: TDataBag): boolean;
begin
  Result := FF5(DataBag, TRUE);
end;


function TNextLinesBuilderDataFinder.RF2(const DataBag: TDataBag): boolean;
begin
  Result := FF5(DataBag, FALSE);
end;


function TNextLinesBuilderDataFinder.RF3: boolean;
begin
  Result := TRUE;
end;


procedure TNextLinesBuilderDataFinder.RP4;
begin
  m_Address.FInit(ChessRulesEngine.GetPosition, LastPosition, 0);
end;


function TNextLinesBuilderDataFinder.FGetChessRulesEngine: TChessRulesEngine;
begin
  Result := Base.ChessRulesEngine;
end;


procedure TNextLinesBuilderDataFinder.FCollectDatas;
var
  Data: TMovePosAddress;
begin
  Data.FInit(ChessRulesEngine.lastMove^, ChessRulesEngine.Position^ {?}, m_Address);
  SetLength(m_Datas, Length(m_Datas) + 1);
  m_Datas[High(m_Datas)] := Data;
end;

end.
