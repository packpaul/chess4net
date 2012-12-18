////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit PosBaseUnit;

interface

uses
  Classes, SysUtils,
  //
  ChessRulesEngine, MoveTreeBaseUnit;

type
  PMoveEst = ^TMoveEst;
  TMoveEst = record
    move: TMoveAbs;
    estimate: LongWord;
  end;

  TMoveEstItem = class
  private
    m_pItem: PMoveEst;
    constructor FCreate(pItem: PMoveEst);
    function FGetMove: TMoveAbs;
    function FGetEstimate: LongWord;
  public
    constructor Create;
    destructor Destroy; override;
    property Move: TMoveAbs read FGetMove;
    property Estimate: LongWord read FGetEstimate;
  end;

  TMoveEstList = class(TList)
  private
    function FGetItem(iIndex: integer): TMoveEstItem;
    procedure FSetItem(iIndex: integer; AItem: TMoveEstItem);
  public
    destructor Destroy; override;
    procedure ClearWithContent;
    property Items[iIndex: integer]: TMoveEstItem read FGetItem write FSetItem; default;
  end;

  TReestimate = procedure(moveEsts: TList; nRec: integer);

  TPosBaseStream = class
  private
    m_iRecordSize: integer;
    m_iHeaderSize: integer;
    m_InnerStream: TStream;
    constructor FCreate(const strFileName: string; iRecordSize: integer); overload;
    constructor FCreateForTest(iRecordSize: integer);
    function FGetSize: integer;
  public
    destructor Destroy; override;
    procedure SeekHeader;
    procedure SeekRec(lwRecordNumber: LongWord);
    procedure SeekEnd;
    procedure Write(const Buffer); overload;
    procedure Write(const Buffer; Count: integer); overload;
    procedure Read(var Buffer); overload;
    procedure Read(var Buffer; Count: integer); overload;
    property Size: integer read FGetSize;
    property HeaderSize: integer read m_iHeaderSize write m_iHeaderSize;
  end;

  TPosBase = class;

  TPosBaseStrategy = class
  private
    m_Base: TPosBase;
    constructor FCreate(ABase: TPosBase);
  protected
    procedure RAdd(const posMove: TPosMove); virtual; abstract;
    function RFind(const pos: TChessPosition; var moveEsts: TList): boolean; virtual; abstract;
    class function REncodeMove(const move: TMoveAbs): word;
    class function RDecodeMove(enc_move: word): TMoveAbs;
    class function REncodeAddInf(const pos: TChessPosition): byte;
    property Base: TPosBase read m_Base;
  public
    constructor Create;
  end;

  EPosBaseException = class(Exception);

  TPosBase = class
  private
    m_wDBVersion: Word;
    m_MoveTreeBase: TMoveTreeBase;
    m_fPos: TPosBaseStream;
    m_fMov: TPosBaseStream;
    m_Strategy: TPosBaseStrategy;
    FReestimate: TReestimate;
    constructor FCreate(const AMoveTreeBase: TMoveTreeBase; AReestimate: TReestimate = nil);
    procedure FCreateStreams(const strPosFileName, strMovFileName: string);
    procedure FCreateStreamsForTest;
    procedure FDestroyStreams;
    procedure FSetDBVersion;
    function FCheckDBVersion: Boolean;
    procedure FCreateStrategy;
    procedure FDestroyStrategy;

    property fPos: TPosBaseStream read m_fPos;
    property fMov: TPosBaseStream read m_fMov;
    property Reestimate: TReestimate read FReestimate;

  protected
    constructor CreateForTest(const AMoveTreeBase: TMoveTreeBase; AReestimate: TReestimate = nil);
    
  public
    procedure Add(const posMove: TPosMove); // добавление позиции и хода в базу
    function Find(const pos: TChessPosition): boolean; overload;
    // Deprecated. Planned for removal after 2013.01
    function Find(const pos: TChessPosition; var moveEsts: TList): boolean; overload;
    // moveEsts - TMoveEstItem collection
    function Find(const pos: TChessPosition; out moveEsts: TMoveEstList): boolean; overload;
    constructor Create(fileNameNoExt: string; AReestimate: TReestimate = nil); overload;
    constructor Create(fileNameNoExt: string; const AMoveTreeBase: TMoveTreeBase;
      AReestimate: TReestimate = nil); overload;
    destructor Destroy; override;
  end;

implementation

type
  TFieldNode = packed object
  public
    btField: byte;
  private
    m_btNextNode: byte; // сл. узел
    m_wNextNode: word;
    m_btNextValue: byte; // сл. значение данных
    m_wNextValue: word;
    function FGetNextNode: LongWord;
    procedure FSetNextNode(lwValue: LongWord);
    function FGetNextValue: LongWord;
    procedure FSetNextValue(lwValue: LongWord);
  public
    property NextNode: LongWord read FGetNextNode write FSetNextNode;
    property NextValue: LongWord read FGetNextValue write FSetNextValue;
  end;

  TMoveNode = packed object
  public
    wMove: word;
    estimate: LongWord;
  private
    m_btNextValue: byte; // сл. значение данных
    m_wNextValue: word;
    function FGetNextValuePos: LongWord;
    procedure FSetNextValuePos(lwValue: LongWord);
  public
    procedure EmptyNode;
    property NextValue: LongWord read FGetNextValuePos write FSetNextValuePos;
  end;

  TCoord = record
    i, j: integer;
  end;

  TPosBaseStrategy1 = class(TPosBaseStrategy)
  protected
    procedure RAdd(const posMove: TPosMove); override;
    function RFind(const pos: TChessPosition; var moveEsts: TList): boolean; override;
  end;

  TPosBaseStrategy2 = class(TPosBaseStrategy)
  private
    procedure FAddPosNodes(const posMove: TPosMove;
      k: integer; r: integer = -1);
    function FGetFieldData(const pos: TChessPosition; iIndex: integer): byte;
  protected
    procedure RAdd(const posMove: TPosMove); override;
    function RFind(const pos: TChessPosition; var moveEsts: TList): boolean; override;
  end;

const
  POS_FILE_EXT = 'pos';
  MOV_FILE_EXT = 'mov';

  DB_VERSION_1 = 1;
  DB_VERSION_2 = 2;

  FIELD_SEQ: array[1..64] of TCoord = // 13617 kb
    ((i: 1; j: 1), (i: 1; j: 2), (i: 1; j: 3), (i: 1; j: 4),
     (i: 1; j: 5), (i: 1; j: 6), (i: 1; j: 7), (i: 1; j: 8),
     (i: 8; j: 8), (i: 8; j: 7), (i: 8; j: 6), (i: 8; j: 5),
     (i: 8; j: 4), (i: 8; j: 3), (i: 8; j: 2), (i: 8; j: 1),
     (i: 2; j: 1), (i: 2; j: 2), (i: 2; j: 3), (i: 2; j: 4),
     (i: 2; j: 5), (i: 2; j: 6), (i: 2; j: 7), (i: 2; j: 8),
     (i: 7; j: 8), (i: 7; j: 7), (i: 7; j: 6), (i: 7; j: 5),
     (i: 7; j: 4), (i: 7; j: 3), (i: 7; j: 2), (i: 7; j: 1),
     (i: 3; j: 1), (i: 3; j: 2), (i: 3; j: 3), (i: 3; j: 4),
     (i: 3; j: 5), (i: 3; j: 6), (i: 3; j: 7), (i: 3; j: 8),
     (i: 6; j: 8), (i: 6; j: 7), (i: 6; j: 6), (i: 6; j: 5),
     (i: 6; j: 4), (i: 6; j: 3), (i: 6; j: 2), (i: 6; j: 1),
     (i: 4; j: 1), (i: 4; j: 2), (i: 4; j: 3), (i: 4; j: 4),
     (i: 4; j: 5), (i: 4; j: 6), (i: 4; j: 7), (i: 4; j: 8),
     (i: 5; j: 1), (i: 5; j: 2), (i: 5; j: 3), (i: 5; j: 4),
     (i: 5; j: 5), (i: 5; j: 6), (i: 5; j: 7), (i: 5; j: 8));

////////////////////////////////////////////////////////////////////////////////
// TPosBase

constructor TPosBase.FCreate(const AMoveTreeBase: TMoveTreeBase; AReestimate: TReestimate = nil);
begin
  m_MoveTreeBase := AMoveTreeBase;
  FReestimate := AReestimate;
    
  FSetDBVersion;
  FCreateStrategy;
end;


constructor TPosBase.Create(fileNameNoExt: string; AReestimate: TReestimate = nil);
begin
  Create(fileNameNoExt, nil, AReestimate);
end;


constructor TPosBase.Create(fileNameNoExt: string; const AMoveTreeBase: TMoveTreeBase;
  AReestimate: TReestimate = nil);
begin
  inherited Create;

  FCreateStreams(fileNameNoExt + '.' + POS_FILE_EXT,
    fileNameNoExt + '.' + MOV_FILE_EXT);

  FCreate(AMoveTreeBase, AReestimate);
end;


constructor TPosBase.CreateForTest(const AMoveTreeBase: TMoveTreeBase; AReestimate: TReestimate = nil);
begin
  inherited Create;

  FCreateStreamsForTest;

  FCreate(AMoveTreeBase, AReestimate);
end;


destructor TPosBase.Destroy;
begin
  FDestroyStrategy;
  FDestroyStreams;
  
  inherited;
end;


procedure TPosBase.FSetDBVersion;
var
  btData: byte;
  wVersion: word;
begin
  // default version
  if (Assigned(m_MoveTreeBase)) then
    m_wDBVersion := DB_VERSION_2
  else
    m_wDBVersion := DB_VERSION_1;

  if (fPos.Size > 0) then
  begin
    fPos.SeekHeader;
    fPos.Read(btData, SizeOf(btData));
    if (btData <> $FF) then
    begin
      m_wDBVersion := 0;
      fPos.HeaderSize := 0;
      exit;
    end;
    fPos.Read(wVersion, SizeOf(wVersion));
    m_wDBVersion := wVersion;
  end
  else
  begin
    btData := $FF;
    wVersion := m_wDBVersion;
    fPos.Write(btData, SizeOf(btData));
    fPos.Write(wVersion, SizeOf(wVersion));
  end;

  fPos.HeaderSize := SizeOf(byte) + SizeOf(word);
end;


procedure TPosBase.FCreateStrategy;
begin
  case m_wDBVersion of
    DB_VERSION_1:
      m_Strategy := TPosBaseStrategy1.FCreate(self);
    DB_VERSION_2:
      m_Strategy := TPosBaseStrategy2.FCreate(self);
  else
    raise EPosBaseException.Create('No suitable strategy found!');
  end;
end;


procedure TPosBase.FDestroyStrategy;
begin
  FreeAndNil(m_Strategy);
end;


function TPosBase.FCheckDBVersion: Boolean;
begin
  Result := (m_wDBVersion <= DB_VERSION_2);
end;


procedure TPosBase.FCreateStreams(const strPosFileName, strMovFileName: string);
begin
  m_fPos := TPosBaseStream.FCreate(strPosFileName, SizeOf(TFieldNode));
  m_fMov := TPosBaseStream.FCreate(strMovFileName, SizeOf(TMoveNode));
end;


procedure TPosBase.FCreateStreamsForTest;
begin
  m_fPos := TPosBaseStream.FCreateForTest(SizeOf(TFieldNode));
  m_fMov := TPosBaseStream.FCreateForTest(SizeOf(TMoveNode));
end;


procedure TPosBase.FDestroyStreams;
begin
  FreeAndNil(m_fMov);
  FreeAndNil(m_fPos);
end;


procedure TPosBase.Add(const posMove: TPosMove);
begin
  if (FCheckDBVersion) then
    m_Strategy.RAdd(posMove);
end;


function TPosBase.Find(const pos: TChessPosition): boolean;
var
  lstDummy: TList;
begin
  lstDummy := nil;
  Result := Find(pos, lstDummy);
end;


function TPosBase.Find(const pos: TChessPosition; var moveEsts: TList): boolean;
begin
  if (FCheckDBVersion) then
    Result := m_Strategy.RFind(pos, moveEsts)
  else
    Result := FALSE;
end;


function TPosBase.Find(const pos: TChessPosition; out moveEsts: TMoveEstList): boolean;
var
  lstMoveEsts: TList;
  i: integer;
begin
  moveEsts := nil;

  lstMoveEsts := TList.Create;
  try
    Result := Find(pos, lstMoveEsts);
    if (not Result) then
      exit;

    moveEsts := TMoveEstList.Create;
    for i := 0 to lstMoveEsts.Count - 1 do
    begin
      moveEsts.Add(TMoveEstItem.FCreate(PMoveEst(lstMoveEsts[i])));
    end;

  finally
    lstMoveEsts.Free;
  end;

end;

////////////////////////////////////////////////////////////////////////////////
// TFieldNode

function TFieldNode.FGetNextNode: LongWord;
begin
  Result := (m_wNextNode shl 8) or m_btNextNode;
end;


procedure TFieldNode.FSetNextNode(lwValue: LongWord);
begin
  m_btNextNode := lwValue and $FF;
  m_wNextNode := lwValue shr 8;
end;


function TFieldNode.FGetNextValue: LongWord;
begin
  Result := (m_wNextValue shl 8) or m_btNextValue;
end;


procedure TFieldNode.FSetNextValue(lwValue: LongWord);
begin
  m_btNextValue := lwValue and $FF;
  m_wNextValue := lwValue shr 8;
end;

////////////////////////////////////////////////////////////////////////////////
// TMoveNode


function TMoveNode.FGetNextValuePos: LongWord;
begin
  Result := (m_wNextValue shl 8) or m_btNextValue;
end;


procedure TMoveNode.FSetNextValuePos(lwValue: LongWord);
begin
  m_btNextValue := lwValue and $FF;
  m_wNextValue := lwValue shr 8;
end;


procedure TMoveNode.EmptyNode;
begin
  FillChar(self, SizeOf(self), 0);
end;

////////////////////////////////////////////////////////////////////////////////
// TPosBaseStream

constructor TPosBaseStream.FCreate(const strFileName: string; iRecordSize: integer);
var
  FileHandle: Integer;
begin
  inherited Create;

  m_iRecordSize := iRecordSize;

  if (not FileExists(strFileName)) then
  begin
    FileHandle := FileCreate(strFileName);
    FileClose(FileHandle);
  end;

  m_InnerStream := TFileStream.Create(strFileName, fmOpenReadWrite,
    fmShareDenyWrite);
end;


constructor TPosBaseStream.FCreateForTest(iRecordSize: integer);
begin
  inherited Create;

  m_iRecordSize := iRecordSize;
  m_InnerStream := TMemoryStream.Create;
end;


destructor TPosBaseStream.Destroy;
begin
  m_InnerStream.Free;
  inherited;
end;


function TPosBaseStream.FGetSize: integer;
begin
  Result := (m_InnerStream.Size - m_iHeaderSize) div m_iRecordSize;
end;


procedure TPosBaseStream.SeekHeader;
begin
  m_InnerStream.Seek(0, soFromBeginning);
end;


procedure TPosBaseStream.SeekRec(lwRecordNumber: LongWord);
begin
  m_InnerStream.Seek(m_iHeaderSize + lwRecordNumber * m_iRecordSize, soFromBeginning);
end;


procedure TPosBaseStream.SeekEnd;
begin
  m_InnerStream.Seek(0, soFromEnd);
end;


procedure TPosBaseStream.Write(const Buffer);
begin
  m_InnerStream.WriteBuffer(Buffer, m_iRecordSize);
end;


procedure TPosBaseStream.Write(const Buffer; Count: integer);
begin
  m_InnerStream.WriteBuffer(Buffer, Count);
end;


procedure TPosBaseStream.Read(var Buffer);
begin
  m_InnerStream.ReadBuffer(Buffer, m_iRecordSize);
end;


procedure TPosBaseStream.Read(var Buffer; Count: integer);
begin
  m_InnerStream.ReadBuffer(Buffer, Count);
end;

////////////////////////////////////////////////////////////////////////////////
// TMoveEstItem

constructor TMoveEstItem.Create;
begin
  raise Exception.Create(ClassName + ' cannot be instantiated directly!');
end;


constructor TMoveEstItem.FCreate(pItem: PMoveEst);
begin
  inherited Create;
  m_pItem := pItem;
end;


destructor TMoveEstItem.Destroy;
begin
  Dispose(m_pItem);
  inherited;
end;


function TMoveEstItem.FGetMove: TMoveAbs;
begin
  Result := m_pItem.move;
end;


function TMoveEstItem.FGetEstimate: LongWord;
begin
  Result := m_pItem.estimate;
end;

////////////////////////////////////////////////////////////////////////////////
// TMoveEstList

destructor TMoveEstList.Destroy;
begin
  ClearWithContent;
  inherited;
end;

function TMoveEstList.FGetItem(iIndex: integer): TMoveEstItem;
begin
  Result := inherited Items[iIndex];
end;


procedure TMoveEstList.FSetItem(iIndex: integer; AItem: TMoveEstItem);
begin
  inherited Items[iIndex] := AItem;
end;


procedure TMoveEstList.ClearWithContent;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    (Items[i] as TMoveEstItem).Free;

  Clear;
end;

////////////////////////////////////////////////////////////////////////////////
// TPosBaseStrategy

constructor TPosBaseStrategy.Create;
begin
  raise Exception.Create(ClassName + ' cannot be instaniated directly!');
end;


constructor TPosBaseStrategy.FCreate(ABase: TPosBase);
begin
  inherited Create;
  m_Base := ABase;
end;


class function TPosBaseStrategy.REncodeMove(const move: TMoveAbs): word;
begin
  with move do
    Result := ((((((((i0-1) shl 3) or (j0-1)) shl 3) or (i-1)) shl 3) or (j-1)) shl 3) or Ord(prom_fig);
end;


class function TPosBaseStrategy.RDecodeMove(enc_move: word): TMoveAbs;
begin
 with Result do
 begin
   prom_fig := TFigureName(enc_move and $07);
   enc_move := enc_move shr 3;
   j := (enc_move and $07) + 1;
   enc_move := enc_move shr 3;
   i := (enc_move and $07) + 1;
   enc_move := enc_move shr 3;
   j0 := (enc_move and $07) + 1;
   enc_move := enc_move shr 3;
   i0 := (enc_move and $07) + 1;
 end;

end;


class function TPosBaseStrategy.REncodeAddInf(const pos: TChessPosition): byte;
begin
  Result := pos.en_passant;
  if WhiteKingSide in pos.castling then
    Result := Result or $80;
  if WhiteQueenSide in pos.castling then
    Result := Result or $40;
  if BlackKingSide in pos.castling then
    Result := Result or $20;
  if BlackQueenSide in pos.castling then
    Result := Result or $10;
end;

////////////////////////////////////////////////////////////////////////////////
// TPosBaseStrategy1

procedure TPosBaseStrategy1.RAdd(const posMove: TPosMove);
var
  addInf: byte;
  fn: TFieldNode;

  procedure NAddPosNodes(k: integer; r: integer = -1);
  var
    l, nr: integer;
    mn: TMoveNode;
    estList: TList;
  begin
    // Adding position nodes
    if (r >= 0) then
    begin
      nr := Base.fPos.Size;
      fn.NextValue := nr;
      Base.fPos.SeekRec(r);
      Base.fPos.Write(fn);
      Base.fPos.SeekRec(nr);
    end
    else
      nr := 0;

    for l := k to 66 do // 65 - additional info, 66 - color
    begin
      if l = 66 then
      begin
        fn.btField := ord(posMove.pos.color);
        nr := Base.fMov.Size;
      end
      else
      begin
        if (l <= 64) then
          fn.btField := ord(posMove.pos.board[FIELD_SEQ[l].i, FIELD_SEQ[l].j])
        else // l = 65
          fn.btField := addInf;
        inc(nr);
      end;
      fn.NextNode := nr;
      fn.NextValue := 0;
      Base.fPos.Write(fn);
    end;

    // forming a move record
    mn.EmptyNode;
    mn.wMove := REncodeMove(posMove.move);

    if Assigned(Base.Reestimate) then
    begin
      estList := TList.Create;
      try
        estList.Add(Pointer(mn.estimate));
        Base.Reestimate(estList, 0);
        mn.estimate := LongWord(estList[0]);
      finally
        estList.Free;
      end;

    end;
    Base.fMov.SeekEnd;
    Base.fMov.Write(mn);
  end;

var
  k, r, pr, rm, moveSet, moveCount: integer;
  mv: word;
  mn: TMoveNode;
  enc_mv: word;
  estList: TList;
begin   // .RAdd
  addInf := REncodeAddInf(posMove.pos);
  if (Base.fPos.Size = 0) then
  begin
    NAddPosNodes(1);
    exit;
  end;
  
  r := 0;
  for k := 1 to 66 do // 65 - additional info, 66 - color
  begin
    Base.fPos.SeekRec(r);
    Base.fPos.Read(fn);

    while ((k <= 64) and (fn.btField <> ord(posMove.pos.board[FIELD_SEQ[k].i, FIELD_SEQ[k].j]))) or
          ((k = 65) and (fn.btField <> addInf)) or
          ((k = 66) and (fn.btField <> ord(posMove.pos.color))) do
    begin
      pr := r;
      r := fn.NextValue;
      if (r = 0) then
      begin
        NAddPosNodes(k, pr);
        exit;
      end;
      Base.fPos.SeekRec(r);
      Base.fPos.Read(fn);
    end; { while }
    
    // the value is found in the chain
    r := fn.NextNode;
  end;

  moveCount := 0;
  moveSet := -1;
  estList := TList.Create;
  try
    rm := r;
    enc_mv := REncodeMove(posMove.move);
    repeat
      pr := r;
      Base.fMov.SeekRec(r);
      Base.fMov.Read(mn);

      mv := mn.wMove;
      if mv = enc_mv then
        moveSet := moveCount;

      if Assigned(Base.Reestimate) then
        estList.Add(Pointer(mn.estimate));

      inc(moveCount);
      r := mn.NextValue;
    until r = 0;

    if (moveSet < 0) then // there's no move in the list, hence adding it
    begin
      // binding a new node with the current one
      r := Base.fMov.Size;
      mn.NextValue := r;
      Base.fMov.SeekRec(pr);
      Base.fMov.Write(mn);

      // adding new move node
      mn.EmptyNode;
      mn.wMove := enc_mv;
      Base.fMov.SeekRec(r);
      Base.fMov.Write(mn);

      if Assigned(Base.Reestimate) then
        estList.Add(Pointer(mn.estimate));
      moveSet := moveCount;
    end;

    if Assigned(Base.Reestimate) then
    begin
      Base.Reestimate(estList, moveSet);
      for k := 0 to estList.Count - 1 do
      begin
        Base.fMov.SeekRec(rm);
        Base.fMov.Read(mn);
        if (mn.estimate <> LongWord(estList[k])) then
        begin
          mn.estimate := LongWord(estList[k]);
          Base.fMov.SeekRec(rm);
          Base.fMov.Write(mn);
        end;
        rm := mn.NextValue;
      end;
    end;

  finally
    estList.Free;
  end;
  
end;


function TPosBaseStrategy1.RFind(const pos: TChessPosition; var moveEsts: TList): boolean;
var
  k, r: integer;
  fn: TFieldNode;
  mn: TMoveNode;
  pme: PMoveEst;
label
  here;
begin
  Result := FALSE;

  if (Assigned(moveEsts)) then
  begin
    for k := 0 to moveEsts.Count - 1 do
      Dispose(moveEsts[k]);
    moveEsts.Clear;
  end;

  if (Base.fPos.Size = 0) then
    exit;

  r := 0;
  for k := 1 to 66 do // 65 - additional info, 66 - color
  begin
here:
    Base.fPos.SeekRec(r);
    Base.fPos.Read(fn);

    r := fn.NextNode;
    while ((k <= 64) and (fn.btField <> ord(pos.board[FIELD_SEQ[k].i, FIELD_SEQ[k].j]))) or
          ((k = 65) and (fn.btField <> REncodeAddInf(pos))) or
          ((k = 66) and (fn.btField <> ord(pos.color))) do
    begin
      r := fn.NextValue;
      if r = 0 then
        exit
      else
        goto here;
    end; { while }
  end; { for }

  Result := TRUE;
  if (not Assigned(moveEsts)) then
    exit;

  // Filling the moves list
  repeat
    Base.fMov.SeekRec(r);
    Base.fMov.Read(mn);

    new(pme);
    pme^.move := RDecodeMove(mn.wMove);
    pme^.estimate := mn.estimate;
    moveEsts.Add(pme);

    r := mn.NextValue;
  until (r = 0);
  
end;

////////////////////////////////////////////////////////////////////////////////
// TPosBaseStrategy2

procedure TPosBaseStrategy2.RAdd(const posMove: TPosMove);
var
  k, r, pr, rm, moveSet, moveCount: integer;
  fn: TFieldNode;
  mv: word;
  mn: TMoveNode;
  enc_mv: word;
  estList: TList;
begin
  if (Base.fPos.Size = 0) then
  begin
    FAddPosNodes(posMove, 1);
    exit;
  end;

  r := 0;
  for k := 1 to 66 do
  begin
    Base.fPos.SeekRec(r);
    Base.fPos.Read(fn);

    while (fn.btField <> FGetFieldData(posMove.pos, k)) do
    begin
      pr := r;
      r := fn.NextValue;
      if (r = 0) then
      begin
        FAddPosNodes(posMove, k, pr);
        exit;
      end;
      Base.fPos.SeekRec(r);
      Base.fPos.Read(fn);
    end; { while }
    
    // the value is found in the chain
    r := fn.NextNode;
  end;

  moveCount := 0;
  moveSet := -1;
  estList := TList.Create;
  try
    rm := r;
    enc_mv := REncodeMove(posMove.move);
    repeat
      pr := r;
      Base.fMov.SeekRec(r);
      Base.fMov.Read(mn);

      mv := mn.wMove;
      if mv = enc_mv then
        moveSet := moveCount;

      if Assigned(Base.Reestimate) then
        estList.Add(Pointer(mn.estimate));

      inc(moveCount);
      r := mn.NextValue;
    until r = 0;

    if (moveSet < 0) then // there's no move in the list, hence adding it
    begin
      // binding a new node with the current one
      r := Base.fMov.Size;
      mn.NextValue := r;
      Base.fMov.SeekRec(pr);
      Base.fMov.Write(mn);

      // adding new move node
      mn.EmptyNode;
      mn.wMove := enc_mv;
      Base.fMov.SeekRec(r);
      Base.fMov.Write(mn);

      if Assigned(Base.Reestimate) then
        estList.Add(Pointer(mn.estimate));
      moveSet := moveCount;
    end;

    if Assigned(Base.Reestimate) then
    begin
      Base.Reestimate(estList, moveSet);
      for k := 0 to estList.Count - 1 do
      begin
        Base.fMov.SeekRec(rm);
        Base.fMov.Read(mn);
        if (mn.estimate <> LongWord(estList[k])) then
        begin
          mn.estimate := LongWord(estList[k]);
          Base.fMov.SeekRec(rm);
          Base.fMov.Write(mn);
        end;
        rm := mn.NextValue;
      end;
    end;

  finally
    estList.Free;
  end;

end;


function TPosBaseStrategy2.FGetFieldData(const pos: TChessPosition; iIndex: integer): byte;
const
  COLOR_BIT: array[TFigureColor] of byte = ($00, $80);
begin
  case iIndex of
    1:
      Result := REncodeAddInf(pos);
    2..64:
      Result := ord(pos.board[FIELD_SEQ[iIndex - 1].i, FIELD_SEQ[iIndex - 1].j]);
    65:
      Result := ord(pos.board[FIELD_SEQ[iIndex - 1].i, FIELD_SEQ[iIndex - 1].j]) or
         COLOR_BIT[pos.color];
  else
    Result := $00;
  end;

end;


procedure TPosBaseStrategy2.FAddPosNodes(const posMove: TPosMove;
  k: integer; r: integer = -1);
var
  l, nr: integer;
  fn: TFieldNode;
  mn: TMoveNode;
  estList: TList;
begin
  if (r >= 0) then
  begin
    Base.fPos.SeekRec(r);
    Base.fPos.Read(fn);
    nr := Base.fPos.Size;
    fn.NextValue := nr;
    Base.fPos.SeekRec(r);
    Base.fPos.Write(fn);
    Base.fPos.SeekRec(nr);
  end
  else
    nr := 0;

  for l := k to 66 do
  begin
    fn.btField := FGetFieldData(posMove.pos, l);
    if l = 66 then
    begin
      fn.NextNode := Base.fMov.Size;
      fn.NextValue := $DEADDE; // TODO:
    end
    else
    begin
      inc(nr);
      fn.NextNode := nr;
      fn.NextValue := 0;
    end;
    Base.fPos.Write(fn);
  end;

  // forming a move record
  mn.EmptyNode;
  mn.wMove := REncodeMove(posMove.move);

  if Assigned(Base.Reestimate) then
  begin
    estList := TList.Create;
    try
      estList.Add(Pointer(mn.estimate));
      Base.Reestimate(estList, 0);
      mn.estimate := LongWord(estList[0]);
    finally
      estList.Free;
    end;

  end;
  Base.fMov.SeekEnd;
  Base.fMov.Write(mn);
end;


function TPosBaseStrategy2.RFind(const pos: TChessPosition; var moveEsts: TList): boolean;
var
  k, r: integer;
  fn: TFieldNode;
  lwMoveAddress, lwMoveTreeAddress: LongWord;
  mn: TMoveNode;
  pme: PMoveEst;
label
  here;
begin
  Result := FALSE;

  if (Assigned(moveEsts)) then
  begin
    for k := 0 to moveEsts.Count - 1 do
      Dispose(moveEsts[k]);
    moveEsts.Clear;
  end;

  if (Base.fPos.Size = 0) then
    exit;

  r := 0;
  for k := 1 to 66 do
  begin
here:
    Base.fPos.SeekRec(r);
    Base.fPos.Read(fn);

    r := fn.NextNode;

    while (fn.btField <> FGetFieldData(pos, k)) do
    begin
      r := fn.NextValue;
      if (r = 0) then
        exit
      else
        goto here;
    end; { while }
  end; { for }

  Result := TRUE;
  if (not Assigned(moveEsts)) then
    exit;

  lwMoveAddress := fn.NextNode;
  lwMoveTreeAddress := fn.NextValue;

  // Filling the moves list
  r := lwMoveAddress;
  repeat
    Base.fMov.SeekRec(r);
    Base.fMov.Read(mn);

    new(pme);
    pme^.move := RDecodeMove(mn.wMove);
    pme^.estimate := mn.estimate;
    moveEsts.Add(pme);

    r := mn.NextValue;
  until (r = 0);

  // TODO: Expand moveEsts with data from MoveTreeBase
  
end;

end.
