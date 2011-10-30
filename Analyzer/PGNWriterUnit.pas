////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

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
      bSplit: boolean = FALSE; bLeaveSpacesWhenWrapped: boolean = FALSE);

  public
    constructor Create;
    destructor Destroy; override;
    procedure WriteInChess4NetFormat(const SourceTree: TPlysTree);
    property Data: TTntStrings read FGetData;
  end;

implementation

uses
  SysUtils, TntSysUtils, StrUtils, Math,
  //
  ChessRulesEngine;

type
  TGameTreeWriter = class
  private
    m_Writer: TPGNWriter;
    m_Tree: TPlysTree;

    m_iTreeDepth: integer;
    m_bAddIndentFormatting: boolean;
    m_bExplicitNumberingFormatting: boolean;

    procedure FWriteWrappedText(const wstr: WideString; iIndent: integer = 0;
      bSplit: boolean = FALSE; bLeaveSpacesWhenWrapped: boolean = FALSE);
    procedure FWriteLine;

    function FGetTextIndent: integer;

    function FWriteComment(wstrComment: WideString): boolean;

  public
    procedure WriteInC4NFormat;
    constructor Create(AWriter: TPGNWriter);
  end;

const
  MAX_INDENTED_TREE_DEPTH = 3;

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

    with TGameTreeWriter.Create(self) do
    try
      WriteInC4NFormat;
    finally
      Free;
    end;

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
  bSplit: boolean = FALSE; bLeaveSpacesWhenWrapped: boolean = FALSE);
const
  TEXT_WIDTH = 80;

  procedure NWriteSplitted;

    function NFindSplitPosition(const wstr: WideString): integer;
    var
      iPos: integer;
      bFindReserveSplit: boolean;
    begin
      Assert(Length(wstr) > TEXT_WIDTH);

      Result := TEXT_WIDTH;

      iPos := Result;

      if (bLeaveSpacesWhenWrapped) then
      begin
        if ((wstr[iPos] = ' ') and (wstr[iPos + 1] <> ' ')) then
          exit;
      end;

      while ((iPos > iIndent) and (wstr[iPos] = ' ')) do
        dec(iPos);

      bFindReserveSplit := TRUE;

      if (bLeaveSpacesWhenWrapped) then
      begin
        while ((iPos > iIndent) and (wstr[iPos] <> ' ')) do
        begin
          if (bFindReserveSplit and (wstr[iPos + 1] <> ' ')) then
          begin
            bFindReserveSplit := FALSE;
            Result := iPos;
          end;
          dec(iPos);
        end;
      end;

      if (iPos > iIndent) then
        Result := iPos;
    end;

  var
    wstrLine: WideString;
    iPos: integer;
  begin // NWriteSplitted
    wstrLine := m_wstrlData[m_wstrlData.Count - 1];

    iPos := Pos(' ', wstr) - 1;
    if (iPos < 0) then
      iPos := Length(wstr);

    if (((Length(wstrLine) + iPos) > TEXT_WIDTH) and
        ((iPos + iIndent) <= TEXT_WIDTH)) then
      FWriteLine;

    FWriteText(wstr, iIndent);

    wstrLine := m_wstrlData[m_wstrlData.Count - 1];

    while (Length(wstrLine) > TEXT_WIDTH) do
    begin
      iPos := NFindSplitPosition(wstrLine);

      m_wstrlData[m_wstrlData.Count - 1] := Copy(wstrLine, 1, iPos);
      wstrLine := TrimLeft(Copy(wstrLine, iPos + 1, MaxInt));

      if (wstrLine <> '') then
      begin
        FWriteLine;
        FWriteText(wstrLine, iIndent);
      end;

      wstrLine := m_wstrlData[m_wstrlData.Count - 1];
    end; // while

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

begin // .FWriteWrappedText
  if (wstr = '') then
    exit;

  if (bSplit) then
    NWriteSplitted
  else
    NWriteNonSplitted;
end;


function TPGNWriter.FGetData: TTntStrings;
begin
  Result := m_wstrlData;
end;

////////////////////////////////////////////////////////////////////////////////
// TGameTreeWriter

constructor TGameTreeWriter.Create(AWriter: TPGNWriter);
begin
  inherited Create;
  m_Writer := AWriter;
  m_Tree := AWriter.m_Tree;
end;


procedure TGameTreeWriter.FWriteWrappedText(const wstr: WideString; iIndent: integer = 0;
  bSplit: boolean = FALSE; bLeaveSpacesWhenWrapped: boolean = FALSE);
begin
  m_Writer.FWriteWrappedText(wstr, iIndent, bSplit, bLeaveSpacesWhenWrapped);
end;


procedure TGameTreeWriter.FWriteLine;
begin
  m_Writer.FWriteLine;
end;


procedure TGameTreeWriter.WriteInC4NFormat;

  procedure NWritePly(iPly: integer; const wstrPly: WideString);
  var
    bWhiteToMove: boolean;
    iMoveNumber: integer;
    wstrMoveNumber: WideString;
  begin
    bWhiteToMove := TPlysTree.IsWhiteToMove(iPly, m_Tree.WhiteStarts);
    if (m_bExplicitNumberingFormatting or bWhiteToMove) then
    begin
      iMoveNumber := TPlysTree.ConvertPlyToMove(
        m_Tree.PlysOffset + iPly, m_Tree.WhiteStarts);
      wstrMoveNumber := Format('%d.', [iMoveNumber]) +
        IfThen((not bWhiteToMove), ' ...');
    end
    else
    begin
      wstrMoveNumber := '';
      m_bAddIndentFormatting := FALSE;
    end;

    FWriteWrappedText(IfThen(m_bAddIndentFormatting, ' ') + wstrMoveNumber + ' ' + wstrPly,
      IfThen((FGetTextIndent > 0), FGetTextIndent + 1));

    m_bExplicitNumberingFormatting := FALSE;
    m_bAddIndentFormatting := TRUE;
  end;

  procedure NFConvertTreeToText(iPly: integer = 0);
  var
    i: integer;
    strlPlys: TStringList;
  begin
    if (iPly = 0) then
    begin
      if (FWriteComment(m_Tree.Comments[0])) then
        FWriteLine;
    end;

    strlPlys := TStringList.Create;
    try
      while (iPly < m_Tree.Count - 1) do
      begin
        inc(iPly);

        m_Tree.GetPlysForPlyIndex(iPly, TStrings(strlPlys), FALSE);
        Assert(strlPlys.Count > 0);

        for i := 0 to strlPlys.Count - 1 do
        begin
          if (i = 0) then
          begin
            NWritePly(iPly, strlPlys[0]);
            FWriteComment(m_Tree.Comments[iPly]);
            continue;
          end;

          if (i = 1) then
          begin
            if (m_iTreeDepth < MAX_INDENTED_TREE_DEPTH) then
            begin
              FWriteLine;
//              if (iTreeDepth = 0) then
//                FWriteLine;
            end;

            FWriteWrappedText(' (', FGetTextIndent + 1, TRUE);

            m_bExplicitNumberingFormatting := TRUE;
            m_bAddIndentFormatting := FALSE;
          end;

          m_Tree.SetPlyForPlyIndex(iPly, strlPlys[i]);

          inc(m_iTreeDepth);
          try
            NWritePly(iPly, strlPlys[i]);
            FWriteComment(m_Tree.Comments[iPly]);
            NFConvertTreeToText(iPly);
          finally
            dec(m_iTreeDepth);
          end;

          FWriteWrappedText(')', FGetTextIndent + 3, TRUE);
          if ((m_iTreeDepth < MAX_INDENTED_TREE_DEPTH)) then
          begin
            FWriteLine;
            m_bAddIndentFormatting := FALSE;
          end;

          if (i = strlPlys.Count - 1) then
          begin
            if (m_iTreeDepth = 0) then
              FWriteLine;
          end;

          if (i < strlPlys.Count - 1) then
            FWriteWrappedText(' (', FGetTextIndent + 1, TRUE);

          m_bExplicitNumberingFormatting := TRUE;
        end; // for i

        m_Tree.SetPlyForPlyIndex(iPly, strlPlys[0]);

      end; // while

    finally
      strlPlys.Free;
    end;

  end;

begin // .WriteInC4NFormat
  m_iTreeDepth := 0;

  m_bExplicitNumberingFormatting := TRUE;
  m_bAddIndentFormatting := FALSE;

  NFConvertTreeToText;
end;


function TGameTreeWriter.FGetTextIndent: integer;
begin
  Result := 2 * Min(m_iTreeDepth, MAX_INDENTED_TREE_DEPTH);
end;


function TGameTreeWriter.FWriteComment(wstrComment: WideString): boolean;
var
  iIndent: integer;

  procedure NWriteCommentLine(const wstrCommentLine: WideString);
  var
    wstr: WideString;
  begin
    wstr := Tnt_WideStringReplace(wstrCommentLine, '}', '}}', [rfReplaceAll]);
    wstr := Tnt_WideStringReplace(wstr, '|', '||', [rfReplaceAll], TRUE);

    FWriteWrappedText(wstr, iIndent + 1, TRUE, TRUE);
  end;

var
  i: integer;
begin // .FWriteComment
  Result := FALSE;

  wstrComment := Trim(wstrComment);
  if (wstrComment = '') then
    exit;

  iIndent := IfThen((FGetTextIndent > 0), FGetTextIndent + 1);

  FWriteWrappedText(IfThen(m_bAddIndentFormatting, ' ') + '{', iIndent);

  with TTntStringList.Create do
  try
    Text := wstrComment;

    NWriteCommentLine(Strings[0]);
    for i := 1 to Count - 1 do
    begin
      if (Strings[i - 1] = '') then
        FWriteWrappedText('| ', iIndent + 1, TRUE, TRUE)
      else
        FWriteWrappedText(' | ', iIndent + 1, FALSE, TRUE);
      NWriteCommentLine(Strings[i]);
    end;

  finally
    Free;
  end;

  FWriteWrappedText('}', iIndent + 1);

  m_bExplicitNumberingFormatting := TRUE;

  Result := TRUE;
end;


end.
