unit PGNWriterUnit;

interface

uses
  Classes, TntClasses,
  //
  PlysTreeUnit;

type
  TPGNWriter = class
  private
    m_wstrlData: TTntStringList;
    m_Tree: TPlysTree;

    function FGetData: TTntStrings;

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
    property Data: TTntStrings read FGetData;
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
  m_wstrlData := TTntStringList.Create;
end;


destructor TPGNWriter.Destroy;
begin
  m_wstrlData.Free;
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
  m_wstrlData.Clear;
end;


procedure TPGNWriter.FWriteTag(const strTagName: string; const wstrTagData: WideString);
begin
  FWriteText(Format('[%s "%s"]', [strTagName, wstrTagData]));
  FWriteLine;
end;


procedure TPGNWriter.FWriteLine;
begin
  m_wstrlData.Append('');
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

  iIndex := m_wstrlData.Count - 1;

  if (iIndex < 0) then
    m_wstrlData.Append(NGetIndent + wstr)
  else
  begin
    if (Length(m_wstrlData[iIndex]) > 0) then
      m_wstrlData[iIndex] := m_wstrlData[iIndex] + wstr
    else
      m_wstrlData[iIndex] := NGetIndent + wstr;
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

    wstrLine := m_wstrlData[m_wstrlData.Count - 1];

    if (Length(wstrLine) <= TEXT_WIDTH) then
      exit;

    repeat
      iPos := TEXT_WIDTH;

      while ((iPos > 0) and (wstrLine[iPos] = ' ')) do
        dec(iPos);

      if (iPos = 0) then
        iPos := TEXT_WIDTH;

      m_wstrlData[m_wstrlData.Count - 1] := Copy(wstrLine, 1, iPos);

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
    if ((m_wstrlData.Count = 0) or
        ((Length(m_wstrlData[m_wstrlData.Count - 1]) + Length(wstr)) > TEXT_WIDTH)) then
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
  bAddIndentFormatting: boolean;
  bExplicitNumberingFormatting: boolean;

  function NWriteComment(wstrComment: WideString): boolean;
  var
    iIndent: integer;
    iPosLeft, iPos: integer;
    wstr: WideString;
  begin
    Result := FALSE;

    wstrComment := Trim(wstrComment);
    if (wstrComment = '') then
      exit;

    iIndent := IfThen((NGetTextIndent > 0), NGetTextIndent + 1);

    FWriteWrappedText(IfThen(bAddIndentFormatting, ' ') + '{', iIndent);

    iPosLeft := 1;
    repeat
      iPos := PosEx('}', wstrComment, iPosLeft);
      if (iPos >= 1) then
        wstr := Copy(wstrComment, iPosLeft, iPos - iPosLeft + 1)
      else
        wstr := Copy(wstrComment, iPosLeft, MaxInt);

      FWriteWrappedText(wstr, iIndent + 1, TRUE);
      if (iPos >= 1) then
        FWriteWrappedText('}', iIndent + 1);

      iPosLeft := iPos + 1;
    until (not (iPos >= 1));

    FWriteWrappedText('}', iIndent + 1);

    bExplicitNumberingFormatting := TRUE;

    Result := TRUE;
  end;

  procedure NWritePly(iPly: integer; const wstrPly: WideString);
  var
    bWhiteToMove: boolean;
    iMoveNumber: integer;
    wstrMoveNumber: WideString;
  begin // \NWritePly
    bWhiteToMove := TPlysTree.IsWhiteToMove(iPly, m_Tree.WhiteStarts);
    if (bExplicitNumberingFormatting or bWhiteToMove) then
    begin
      iMoveNumber := TPlysTree.ConvertPlyToMove(
        m_Tree.PlysOffset + iPly, m_Tree.WhiteStarts);
      wstrMoveNumber := Format('%d.', [iMoveNumber]) +
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
    if (iPly = 0) then
    begin
      if (NWriteComment(m_Tree.Comments[0])) then
        FWriteLine;
    end;

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
            NWriteComment(m_Tree.Comments[iPly]);
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

          m_Tree.SetPlyForPlyIndex(iPly, strlPlys[i]);

          NWritePly(iPly, strlPlys[i]);
          NWriteComment(m_Tree.Comments[iPly]);

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


function TPGNWriter.FGetData: TTntStrings;
begin
  Result := m_wstrlData as TTntStrings;
end;

end.
