unit PGN2C4NConvertorUnit;

interface

uses
  SysUtils, Classes, TntClasses,
  //
  PGNParserUnit;

type
  IC4NDataCollector = interface
    procedure AddLine(const wstrLine: WideString);
    procedure AddLines(const Lines: TTntStrings);
    function HasData: boolean;
  end;

  EPGN2C4NConvertor = class(Exception);

  TPGN2C4NConvertor = class
  private
    m_PGNData: IPGNDataCursor;
    m_C4NData: IC4NDataCollector;
    m_PGNTagParser: TPGNTagParser;
    constructor FCreate(APGNData: IPGNDataCursor; AC4NData: IC4NDataCollector);
    procedure FProcess;
    function FParseTags(var wstrlTags: TTntStringList): boolean;
    function FParseGame(var wstrlPGNGame: TTntStringList): boolean; overload;
    function FParseGame(const wstrlTags: TTntStrings;
      var wstrlPGNGame: TTntStringList): boolean; overload;
    procedure FWriteToC4NData(const PGNTags, C4NGame: TTntStrings);
  public
    constructor Create;
    destructor Destroy; override;
    class procedure Convert(const InputPGNFile, OutputC4NFile: TFileName);
  end;

implementation

uses
  NonRefInterfacedObjectUnit, PGNWriterUnit;

type
  TPGNDataCursor = class(TInterfacedObject, IPGNDataCursor)
  private
    m_Data: TTntStrings;
    m_iDataLine: integer;
  public
    constructor Create(const AData: TTntStrings);
    function GetLine: WideString;
    function GetNextLine: WideString;
    function IsEndOfData: boolean;
  end;


  TC4NDataCollector = class(TNonRefInterfacedObject, IC4NDataCollector)
  private
    m_wstrlData: TTntStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddLine(const wstrLine: WideString);
    procedure AddLines(const Lines: TTntStrings);
    procedure SaveToFile(AFileName: TFileName);
    function HasData: boolean;
  end;

////////////////////////////////////////////////////////////////////////////////
// TPGN2C4NConvertor

constructor TPGN2C4NConvertor.Create;
begin
  raise Exception.Create('TPGN2C4NConvertor cannot be instantiated directly!');
end;


constructor TPGN2C4NConvertor.FCreate(APGNData: IPGNDataCursor; AC4NData: IC4NDataCollector);
begin
  inherited Create;

  m_PGNTagParser := TPGNTagParser.Create;

  m_PGNData := APGNData;
  m_C4NData := AC4NData;
end;


destructor TPGN2C4NConvertor.Destroy;
begin
  m_PGNTagParser.Free;

  inherited;
end;


class procedure TPGN2C4NConvertor.Convert(const InputPGNFile, OutputC4NFile: TFileName);
var
  wstrlInputData: TTntStringList;
  C4NDataCollector: TC4NDataCollector;
begin
  C4NDataCollector := nil;

  wstrlInputData := TTntStringList.Create;
  try
    try
      wstrlInputData.LoadFromFile(InputPGNFile);
    except
      on E: Exception do
        raise EPGN2C4NConvertor.Create(E.Message);
    end;

    C4NDataCollector := TC4NDataCollector.Create;    

    TPGN2C4NConvertor.FCreate(TPGNDataCursor.Create(wstrlInputData), C4NDataCollector).FProcess;

    C4NDataCollector.SaveToFile(OutputC4NFile);

  finally
    wstrlInputData.Free;
  end;
  
end;


procedure TPGN2C4NConvertor.FProcess;
var
  wstrlPGNTags, wstrlPGNGame: TTntStringList;
  PGNParser: TPGNParser;
  PGNWriter: TPGNWriter;
begin
  PGNParser := nil;
  PGNWriter := nil;

  wstrlPGNGame := nil;

  wstrlPGNTags := TTntStringList.Create;
  try
    wstrlPGNGame := TTntStringList.Create;

    PGNParser := TPGNParser.Create;
    PGNWriter := TPGNWriter.Create;

    FParseGame(wstrlPGNGame);
    repeat
      if (PGNParser.Parse(wstrlPGNGame)) then
      begin
        PGNWriter.WriteInChess4NetFormat(PGNParser.Tree);
        FWriteToC4NData(wstrlPGNTags, PGNWriter.Data);
      end;
    until (not (FParseTags(wstrlPGNTags) and FParseGame(wstrlPGNTags, wstrlPGNGame)));

  finally
    PGNWriter.Free;
    PGNParser.Free;
    wstrlPGNGame.Free;
    wstrlPGNTags.Free;
  end;
end;


function TPGN2C4NConvertor.FParseGame(var wstrlPGNGame: TTntStringList): boolean;
begin
  Result := FParseGame(nil, wstrlPGNGame);
end;


function TPGN2C4NConvertor.FParseGame(const wstrlTags: TTntStrings;
  var wstrlPGNGame: TTntStringList): boolean;
var
  wstr: WideString;
begin
  Result := FALSE;

  if (m_PGNData.IsEndOfData) then
    exit;

  if (Assigned(wstrlTags)) then
    wstrlPGNGame.Assign(wstrlTags)
  else
    wstrlPGNGame.Clear;

  wstr := m_PGNData.GetLine;
  repeat
    if (TPGNTagParser.IsTag(wstr)) then
      exit;

    if (TrimRight(wstr) <> '') then
      Result := TRUE;

    wstrlPGNGame.Add(wstr);

    wstr := m_PGNData.GetNextLine;

  until (m_PGNData.IsEndOfData)

end;


function TPGN2C4NConvertor.FParseTags(var wstrlTags: TTntStringList): boolean;
begin
  Result := m_PGNTagParser.Parse(m_PGNData);

  if (not Result) then
    exit;

  wstrlTags.Assign(m_PGNTagParser.Tags);
end;


procedure TPGN2C4NConvertor.FWriteToC4NData(const PGNTags, C4NGame: TTntStrings);
var
  iC4NIndex: integer;
  wstr: WideString;
  i, j: integer;
  bSameTagFlag: boolean;
begin
  if (m_C4NData.HasData) then
  begin
    m_C4NData.AddLine('');
    m_C4NData.AddLine('');
  end;

  iC4NIndex := 0;
  while (iC4NIndex < C4NGame.Count) do
  begin
    wstr := C4NGame[iC4NIndex];
    if (not TPGNTagParser.IsTag(wstr)) then
      break;

    m_C4NData.AddLine(wstr);

    inc(iC4NIndex)
  end;

  for i := 0 to PGNTags.Count - 1 do
  begin
    wstr := PGNTags[i];

    bSameTagFlag := FALSE;
    for j := 0 to iC4NIndex - 1 do
    begin
      bSameTagFlag := TPGNTagParser.IsSameTagLabel(wstr, C4NGame[j]);
      if (bSameTagFlag) then
        break;
    end;

    if (not bSameTagFlag) then
      m_C4NData.AddLine(wstr);
  end;

  for i := iC4NIndex to C4NGame.Count - 1 do
    m_C4NData.AddLine(C4NGame[i]);
    
end;

////////////////////////////////////////////////////////////////////////////////
// TPGNDataCursor

constructor TPGNDataCursor.Create(const AData: TTntStrings);
begin
  inherited Create;
  m_Data := AData;
end;


function TPGNDataCursor.GetLine: WideString;
begin
  if (m_iDataLine < m_Data.Count) then
    Result := m_Data[m_iDataLine]
  else
    Result := '';
end;


function TPGNDataCursor.GetNextLine: WideString;
begin
  inc(m_iDataLine);
  
  if (m_iDataLine < (m_Data.Count)) then
    Result := m_Data[m_iDataLine]
  else
    Result := '';
end;


function TPGNDataCursor.IsEndOfData: boolean;
begin
  Result := (m_iDataLine >= m_Data.Count);
end;

////////////////////////////////////////////////////////////////////////////////
// TC4NDataCollector

constructor TC4NDataCollector.Create;
begin
  inherited Create;
  m_wstrlData := TTntStringList.Create;
end;


destructor TC4NDataCollector.Destroy;
begin
  m_wstrlData.Free;

  inherited;
end;


procedure TC4NDataCollector.AddLine(const wstrLine: WideString);
begin
  m_wstrlData.Append(wstrLine);
end;


procedure TC4NDataCollector.AddLines(const Lines: TTntStrings);
var
  i: integer;
begin
  for i := 0 to Lines.Count - 1 do
    AddLine(Lines[i]);
end;


procedure TC4NDataCollector.SaveToFile(AFileName: TFileName);
begin
  m_wstrlData.SaveToFile(AFileName);
  // TODO: save to UTF8
end;


function TC4NDataCollector.HasData: boolean;
begin
  Result := (m_wstrlData.Count > 0);
end;

end.
