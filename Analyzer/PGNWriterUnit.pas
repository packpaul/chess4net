unit PGNWriterUnit;

interface

uses
  Classes,
  //
  PlysTreeUnit;

type
  TPGNWriter = class
  private
    m_strlData: TStringList;
    m_Tree: TPlysTree;

    function FGetData: TStrings;

    procedure FClear;
    procedure FWriteTag(const strTagName: string; const wstrTagData: WideString);
    procedure FWriteLine;
    procedure FWriteText(const wstr: WideString; iIndent: integer = 0);
    procedure FWriteWrappedText(const wstr: WideString; iIndent: integer = 0;
      bSplit: boolean = FALSE);

    procedure FWriteTreeInChess4NetFormat;

  public
    constructor Create;
    destructor Destroy; override;
    procedure WriteInChess4NetFormat(const SourceTree: TPlysTree);
    property Data: TStrings read FGetData;
  end;

implementation

uses
  SysUtils, StrUtils, Math,
  //
  ChessRulesEngine;

////////////////////////////////////////////////////////////////////////////////
// TPGNWriter

constructor TPGNWriter.Create;
begin
  inherited Create;
  m_strlData := TStringList.Create;
end;


destructor TPGNWriter.Destroy;
begin
  m_strlData.Free;
  inherited;
end;


procedure TPGNWriter.WriteInChess4NetFormat(const SourceTree: TPlysTree);

  procedure NWriteFENTag;
  var
    strStartPosition: string;
  begin
    if (m_Tree.Count = 0) then
      exit;

    strStartPosition := m_Tree.Position[0];

    if (strStartPosition <> INITIAL_FEN) then
      FWriteTag('FEN', strStartPosition);
  end;

begin // .WriteInChess4NetFormat
  FClear;

  if ((not Assigned(SourceTree)) or (SourceTree.Count = 0)) then
    exit;

  m_Tree := TPlysTree.Create;
  try
    m_Tree.Assign(SourceTree);

    FWriteTag('C4N', '2'); // versionning for future uses
    NWriteFENTag;
    FWriteLine;
    FWriteTreeInChess4NetFormat;
    
  finally
    FreeAndNil(m_Tree);
  end;
  
end;


procedure TPGNWriter.FClear;
begin
  m_strlData.Clear;
end;


procedure TPGNWriter.FWriteTag(const strTagName: string; const wstrTagData: WideString);
begin
  FWriteText(Format('[%s "%s"]', [strTagName, wstrTagData]));
  FWriteLine;
end;


procedure TPGNWriter.FWriteLine;
begin
  m_strlData.Append('');
end;


procedure TPGNWriter.FWriteText(const wstr: WideString; iIndent: integer = 0);

  function NGetIndent: WideString;
  begin
    Result := WideString(StringOfChar(' ', iIndent));
  end;

var
  iIndex: integer;
begin // .FWriteText
  if (wstr = '') then
    exit;

  iIndex := m_strlData.Count - 1;

  if (iIndex < 0) then
    m_strlData.Append(NGetIndent + wstr)
  else
  begin
    if (Length(m_strlData[iIndex]) > 0) then
      m_strlData[iIndex] := m_strlData[iIndex] + wstr
    else
      m_strlData[iIndex] := NGetIndent + wstr;
  end;
end;


procedure TPGNWriter.FWriteWrappedText(const wstr: WideString; iIndent: integer = 0;
  bSplit: boolean = FALSE);
const
  TEXT_WIDTH = 80;

  procedure NWriteSplitted;
  var
    wstrLine: WideString;
    iPos: integer;
  begin
    FWriteText(wstr, iIndent);

    wstrLine := m_strlData[m_strlData.Count - 1];

    if (Length(wstrLine) <= TEXT_WIDTH) then
      exit;

    repeat
      iPos := TEXT_WIDTH;

      while ((iPos > 0) and (wstrLine[iPos] = ' ')) do
        dec(iPos);

      if (iPos = 0) then
        iPos := TEXT_WIDTH;

      m_strlData[m_strlData.Count - 1] := Copy(wstrLine, 1, iPos);

      wstrLine := TrimLeft(Copy(wstrLine, iPos + 1, MaxInt));
      if (wstrLine <> '') then
      begin
        FWriteLine;
        FWriteText(wstrLine, iIndent);        
      end;

     until (Length(wstrLine) <= TEXT_WIDTH);
  end;

  procedure NWriteNonSplitted;
  begin
    if ((m_strlData.Count = 0) or
        ((Length(m_strlData[m_strlData.Count - 1]) + Length(wstr)) > TEXT_WIDTH)) then
    begin
      FWriteLine;
      FWriteText(TrimLeft(wstr), iIndent);
    end
    else
      FWriteText(wstr, iIndent);
  end;

begin
  if (wstr = '') then
    exit;

  if (bSplit) then
    NWriteSplitted
  else
    NWriteNonSplitted;
end;


procedure TPGNWriter.FWriteTreeInChess4NetFormat;
const
  MAX_INDENTED_TREE_DEPTH = 3;

var
  iTreeDepth: integer;

  function NGetTextIndent: integer;
  begin
    Result := 2 * Min(iTreeDepth, MAX_INDENTED_TREE_DEPTH);
  end;

var
  bExplicitNumberingFormatting: boolean;
  bAddIndentFormatting: boolean;

  procedure NWritePly(iPly: integer; const wstrPly: WideString);
  var
    bWhiteToMove: boolean;
    wstrMoveNumber: WideString;
  begin // \NWritePly
    bWhiteToMove := TPlysTree.IsWhiteToMove(iPly, m_Tree.WhiteStarts);
    if (bExplicitNumberingFormatting or bWhiteToMove) then
    begin
      wstrMoveNumber := Format('%d.', [TPlysTree.ConvertPlyToMove(iPly, m_Tree.WhiteStarts)]) +
        IfThen((not bWhiteToMove), ' ...');
    end
    else
    begin
      wstrMoveNumber := '';
      bAddIndentFormatting := FALSE;
    end;

    FWriteWrappedText(IfThen(bAddIndentFormatting, ' ') + wstrMoveNumber + ' ' + wstrPly,
      IfThen((NGetTextIndent > 0), NGetTextIndent + 1));

    bExplicitNumberingFormatting := FALSE;
    bAddIndentFormatting := TRUE;
  end;

  procedure NFConvertTreeToText(iPly: integer = 0);
  var
    i: integer;
    strlPlys: TStringList;
  begin // \NFConvertTreeToText
    strlPlys := TStringList.Create;
    try
      while (iPly < m_Tree.Count - 1) do
      begin
        inc(iPly);

        m_Tree.GetPlysForPlyIndex(iPly, TStrings(strlPlys));
        Assert(strlPlys.Count > 0);

        for i := 0 to strlPlys.Count - 1 do
        begin
          if (i = 0) then
          begin
            NWritePly(iPly, strlPlys[0]);
            continue;
          end;

          if (i = 1) then
          begin
            if (iTreeDepth < MAX_INDENTED_TREE_DEPTH) then
            begin
              FWriteLine;
//              if (iTreeDepth = 0) then
//                FWriteLine;
            end;

            FWriteWrappedText(' (', NGetTextIndent + 1, TRUE);
            
            bExplicitNumberingFormatting := TRUE;
            bAddIndentFormatting := FALSE;
          end;

          NWritePly(iPly, strlPlys[i]);

          m_Tree.SetPlyForPlyIndex(iPly, strlPlys[i]);

          inc(iTreeDepth);
          try
            NFConvertTreeToText(iPly);
          finally
            dec(iTreeDepth);
          end;

          FWriteWrappedText(')', NGetTextIndent + 3, TRUE);
          if ((iTreeDepth < MAX_INDENTED_TREE_DEPTH)) then
          begin
            FWriteLine;
            bAddIndentFormatting := FALSE;
          end;

          if (i = strlPlys.Count - 1) then
          begin
            if (iTreeDepth = 0) then
              FWriteLine;
          end;

          if (i < strlPlys.Count - 1) then
            FWriteWrappedText(' (', NGetTextIndent + 1, TRUE);

          bExplicitNumberingFormatting := TRUE;
        end; // for i

        m_Tree.SetPlyForPlyIndex(iPly, strlPlys[0]);

      end; // while

    finally
      strlPlys.Free;
    end;

  end;

begin // .FConvertTreeToText
  iTreeDepth := 0;

  bExplicitNumberingFormatting := TRUE;
  bAddIndentFormatting := FALSE;
  
  NFConvertTreeToText;
end;


function TPGNWriter.FGetData: TStrings;
begin
  Result := m_strlData as TStrings;
end;

end.
