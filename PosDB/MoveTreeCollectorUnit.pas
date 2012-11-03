////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit MoveTreeCollectorUnit;

interface

uses
  Classes, SysUtils, Contnrs,
  //
  PGNTraverserUnit, ChessRulesEngine;

type
  PDataBag = ^TDataBag;
  TDataBag = packed object
  public
    btFirst: byte;
    btSecond: byte;
  private
    procedure FSetMove(const AMove: TMoveAbs);
    function FEquals(const Other: TDataBag): boolean;
    function FIsMove: boolean;
    function FIsNearPointer: boolean;
    function FToNearPointer: Word;
    function FIsFarPointer: boolean;
    function FToFarPointer(const HiData, LowData: TDataBag): LongWord;
    function FConvertFromNearPointer(lwPointer: LongWord; out Data: TDataBag): boolean;
    function FConvertFromFarPointer(lwPointer: LongWord; out HiData, LowData: TDataBag): boolean;
  end;

  TInsertionPoint = object
  public
    lwAddress1: LongWord;
    lwAddress2: LongWord;
  private
    procedure FInit(lwAddress: LongWord); overload;
    procedure FInit(lwAAddress1, lwAAddress2: LongWord); overload;
  end;

  TDataBagsIterator = class
  private
    m_DataBags: TObjectList;
    m_iIndex: integer;
    constructor FCreate(const ADataBags: TObjectList);
  public
    constructor Create;
    function HasNext: boolean;
    function GetNext: TDataBag;
    function GetLast: TDataBag;
  end;


  EMoveTreeCollector = class(Exception);

  TMoveTreeCollector = class(TInterfacedObject, IPGNTraverserVisitable)
  private
    m_BaseStream: TStream;
    m_DataBags: TObjectList;

    m_bCollecting: boolean;

    procedure FCreateStream(const BaseFileName: TFileName);
    procedure FDestroyStream;

    function FReadBagFromStream(out ABag: TDataBag): boolean; overload;
    function FReadBagFromStream(lwPosition: LongWord; out ABag: TDataBag): boolean; overload;
    procedure FWriteBagToStream(const ABag: TDataBag); overload;
    procedure FWriteBagToStream(lwPosition: LongWord; const ABag: TDataBag); overload;
    procedure FWriteBagToStreamEnd(const ABag: TDataBag);

    procedure FSaveDataToTree; overload;
    procedure FSaveDataToTree(const DataIterator: TDataBagsIterator); overload;
    function FFindDataFromPosition(const DataIterator: TDataBagsIterator;
      lwPosition: LongWord): boolean; overload;
    function FFindDataFromPosition(const DataIterator: TDataBagsIterator;
      lwPosition: LongWord; out InsertionPoint: TInsertionPoint): boolean; overload;
    procedure FSaveDataFromPosition(const DataIterator: TDataBagsIterator;
      const InsertionPoint: TInsertionPoint);
    procedure FStartNearBranch(const Data: TDataBag; const InsertionPoint: TInsertionPoint);
    procedure FStartFarBranch(const DataHi, DataLow: TDataBag;
      const InsertionPoint: TInsertionPoint);

    procedure FAddBagToData(const ABag: TDataBag);

  public
    constructor Create(const strBaseName: string);
    destructor Destroy; override;
    procedure Start(const Visitor: IPGNTraverserVisitor);
    procedure DoPosMove(iPlyNumber: integer; const APosMove: TPosMove;
      const AResultingPos: TChessPosition);
    procedure StartLine(bFromPreviousPos: boolean);
    procedure EndLine;
    procedure Finish;
  end;

const
  _TDataBag: PDataBag = nil; // helper

implementation

uses
  Math;

type
  TDataBagItem = class
  private
    m_Data: TDataBag;
    constructor FCreate(const AData: TDataBag);
  public
    constructor Create;
    property Data: TDataBag read m_Data;
  end;

const
  BASE_FILE_EXT = 'mvt';

  PROM_FIG_MARKER: array[TFigureName] of byte = ($00, $00, $40, $80, $C0, $00); // K, Q, R, B, N, P

  END_DATA_TAG: TDataBag = (btFirst: 0; btSecond: 0);

  MOVE_DATA_MARKER = $00;
  NEAR_POINTER_DATA_MARKER = $80;
  NEAR_POINTER_DATA_MASK = NEAR_POINTER_DATA_MARKER - 1;
  FAR_POINTER_DATA_MARKER = $40;
  FAR_POINTER_DATA_MASK = FAR_POINTER_DATA_MARKER - 1;
  DATA_KIND_MASK = $C0;
  DATA_MASK = $3F;

////////////////////////////////////////////////////////////////////////////////
// TMoveTreeCollector

constructor TMoveTreeCollector.Create(const strBaseName: string);
begin
  inherited Create;
  FCreateStream(strBaseName + '.' + BASE_FILE_EXT);
  m_DataBags := TObjectList.Create;
end;


destructor TMoveTreeCollector.Destroy;
begin
  m_DataBags.Free;
  FDestroyStream;
  inherited;
end;


procedure TMoveTreeCollector.FCreateStream(const BaseFileName: TFileName);
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


procedure TMoveTreeCollector.FDestroyStream;
begin
  FreeAndNil(m_BaseStream);
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
var
  MoveBag: TDataBag;
begin
  MoveBag.FSetMove(APosMove.move);
  FAddBagToData(MoveBag);
end;


procedure TMoveTreeCollector.FAddBagToData(const ABag: TDataBag);
begin
  m_DataBags.Add(TDataBagItem.FCreate(ABag));
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

  FSaveDataToTree;

  m_bCollecting := FALSE;
end;


function TMoveTreeCollector.FReadBagFromStream(out ABag: TDataBag): boolean;
begin
  Result := (m_BaseStream.Read(ABag, SizeOf(ABag)) = SizeOf(ABag));
end;


function TMoveTreeCollector.FReadBagFromStream(lwPosition: LongWord;
  out ABag: TDataBag): boolean;
begin
  m_BaseStream.Position := lwPosition;
  Result := (m_BaseStream.Read(ABag, SizeOf(ABag)) = SizeOf(ABag));
end;


procedure TMoveTreeCollector.FWriteBagToStream(const ABag: TDataBag);
begin
  m_BaseStream.WriteBuffer(ABag, SizeOf(ABag));
end;


procedure TMoveTreeCollector.FWriteBagToStream(lwPosition: LongWord; const ABag: TDataBag);
begin
  m_BaseStream.Position := lwPosition;
  m_BaseStream.WriteBuffer(ABag, SizeOf(ABag));
end;


procedure TMoveTreeCollector.FWriteBagToStreamEnd(const ABag: TDataBag);
begin
  m_BaseStream.Seek(0, soEnd);
  m_BaseStream.WriteBuffer(ABag, SizeOf(ABag));
end;


procedure TMoveTreeCollector.FSaveDataToTree;
var
  Iterator: TDataBagsIterator;
begin
  Iterator := TDataBagsIterator.FCreate(m_DataBags);
  try
    FSaveDataToTree(Iterator);
  finally
    Iterator.Free;
  end;
end;


procedure TMoveTreeCollector.FSaveDataToTree(const DataIterator: TDataBagsIterator);
var
  InsertionPoint: TInsertionPoint;
begin
  if (FFindDataFromPosition(DataIterator, 0, InsertionPoint)) then
    exit;
  FSaveDataFromPosition(DataIterator, InsertionPoint);
end;


function TMoveTreeCollector.FFindDataFromPosition(const DataIterator: TDataBagsIterator;
  lwPosition: LongWord): boolean;
var
  DummyInsertionPoint: TInsertionPoint;
begin
  Result := FFindDataFromPosition(DataIterator, lwPosition, DummyInsertionPoint);
end;


function TMoveTreeCollector.FFindDataFromPosition(const DataIterator: TDataBagsIterator;
  lwPosition: LongWord; out InsertionPoint: TInsertionPoint): boolean;

var
  lwLastPosition: LongWord;

  function NJumpFar(const HiData: TDataBag; out NextDataBag: TDataBag): boolean;
  var
    LowData: TDataBag;
    lwPositionBase, lwJumpPosition: LongWord;
    DataBag: TDataBag;
    bRead: boolean;
  begin
    lwPositionBase := lwPosition - SizeOf(TDataBag);

    FReadBagFromStream(lwPosition, LowData);
    inc(lwPosition, SizeOf(TDataBag));

    lwJumpPosition := lwPositionBase + _TDataBag.FToFarPointer(HiData, LowData);

    FReadBagFromStream(lwJumpPosition, DataBag);
    Assert(DataBag.FIsMove);

    lwLastPosition := lwJumpPosition + SizeOf(TDataBag);
    FReadBagFromStream(NextDataBag);

    Result := DataBag.FEquals(DataIterator.GetLast);
    if (Result) then
      exit;

    inc(lwLastPosition, SizeOf(TDataBag));
    bRead := FReadBagFromStream(NextDataBag);
    Assert(bRead);

    lwPosition := lwLastPosition + SizeOf(TDataBag);
  end;

var
  DataBag: TDataBag;
  DataBagFromStream: TDataBag;
  bHasDataFlag: boolean;
begin // .FFindDataFromPosition
  InsertionPoint.FInit(lwPosition, lwPosition + SizeOf(TDataBag));

  if (not DataIterator.HasNext) then
  begin
    Result := TRUE;
    exit;
  end;

  Result := FALSE;

  DataBag := DataIterator.GetNext;

  lwLastPosition := lwPosition;
  bHasDataFlag := FReadBagFromStream(lwPosition, DataBagFromStream);
  if (not bHasDataFlag) then
    exit;

  inc(lwPosition, SizeOf(TDataBag));

  repeat
    if (DataBagFromStream.FIsMove) then
    begin
      if (not DataBagFromStream.FEquals(DataBag)) then
      begin
        InsertionPoint.FInit(lwLastPosition, lwPosition);
        exit;
      end;
      if (lwLastPosition < lwPosition) then
        bHasDataFlag := FReadBagFromStream(DataBagFromStream)
      else
        bHasDataFlag := FReadBagFromStream(lwPosition, DataBagFromStream);
      Assert(bHasDataFlag);
      lwLastPosition := lwPosition;
      inc(lwPosition, SizeOf(TDataBag));
    end
    else if (DataBagFromStream.FIsNearPointer) then
    begin
      raise EMoveTreeCollector.Create('Implementation pending!');
    end
    else if (DataBagFromStream.FIsFarPointer) then
    begin
      if (not NJumpFar(DataBagFromStream, DataBagFromStream)) then
        continue;
    end;

    bHasDataFlag := DataIterator.HasNext;
    if (bHasDataFlag) then
      DataBag := DataIterator.GetNext;

  until (not bHasDataFlag);

  Result := TRUE;
end;


procedure TMoveTreeCollector.FSaveDataFromPosition(const DataIterator: TDataBagsIterator;
  const InsertionPoint: TInsertionPoint);
var
  lwAddressOffset: LongWord;
  DataOffsetHi, DataOffsetLow: TDataBag;
begin
  lwAddressOffset := m_BaseStream.Size - (InsertionPoint.lwAddress2 - SizeOf(TDataBag));
  if (lwAddressOffset > 0) then
  begin
    if (_TDataBag.FConvertFromNearPointer(lwAddressOffset, DataOffsetLow)) then
      FStartNearBranch(DataOffsetLow, InsertionPoint)
    else if (_TDataBag.FConvertFromFarPointer(lwAddressOffset, DataOffsetHi, DataOffsetLow)) then
      FStartFarBranch(DataOffsetHi, DataOffsetLow, InsertionPoint)
    else
      raise EMoveTreeCollector.Create('Base file has become too big!');
  end;

  FWriteBagToStreamEnd(DataIterator.GetLast);
  while (DataIterator.HasNext) do
    FWriteBagToStream(DataIterator.GetNext);
  FWriteBagToStream(END_DATA_TAG);
end;


procedure TMoveTreeCollector.FStartNearBranch(const Data: TDataBag;
  const InsertionPoint: TInsertionPoint);
begin
  raise EMoveTreeCollector.Create('Implementation pending!');
end;


procedure TMoveTreeCollector.FStartFarBranch(const DataHi, DataLow: TDataBag;
  const InsertionPoint: TInsertionPoint);
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

////////////////////////////////////////////////////////////////////////////////
// TDataBag

procedure TDataBag.FSetMove(const AMove: TMoveAbs);
begin
  btFirst := 8 * (AMove.j0 - 1) + AMove.i0 - 1;
  btSecond := (8 * (AMove.j - 1) + AMove.i - 1) or PROM_FIG_MARKER[AMove.prom_fig];
end;


function TDataBag.FEquals(const Other: TDataBag): boolean;
begin
  Result := ((btFirst = Other.btFirst) and (btSecond = Other.btSecond));
end;


function TDataBag.FIsMove: boolean;
begin
  Result := ((btFirst and DATA_KIND_MASK) = MOVE_DATA_MARKER);
end;


function TDataBag.FIsNearPointer: boolean;
begin
  Result := ((btFirst and DATA_KIND_MASK) = NEAR_POINTER_DATA_MARKER);
end;


function TDataBag.FIsFarPointer: boolean;
begin
  Result := ((btFirst and DATA_KIND_MASK) = FAR_POINTER_DATA_MARKER);
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
begin
  Result := FALSE;
  // TODO:
end;

////////////////////////////////////////////////////////////////////////////////
// TDataBagItem

constructor TDataBagItem.Create;
begin
  raise Exception.Create(ClassName + ' cannot be instaniated directly!');
end;

constructor TDataBagItem.FCreate(const AData: TDataBag);
begin
  inherited Create;
  m_Data := AData;
end;

////////////////////////////////////////////////////////////////////////////////
// TDataBagsIterator

constructor TDataBagsIterator.Create;
begin
  raise Exception.Create(ClassName + ' cannot be instaniated directly!');
end;


constructor TDataBagsIterator.FCreate(const ADataBags: TObjectList);
begin
  inherited Create;
  m_DataBags := ADataBags;
end;


function TDataBagsIterator.HasNext: boolean;
begin
  Result := (m_iIndex < m_DataBags.Count);
end;


function TDataBagsIterator.GetNext: TDataBag;
begin
  Result := TDataBagItem(m_DataBags[m_iIndex]).Data;
  inc(m_iIndex)
end;


function TDataBagsIterator.GetLast: TDataBag;
begin
  Result := TDataBagItem(m_DataBags[m_iIndex - 1]).Data;
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

end.
