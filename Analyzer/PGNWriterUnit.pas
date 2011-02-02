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
    procedure FWriteWrappedText(const wstr: WideString);

    function FConvertTreeToText: WideString;
  public
    constructor Create;
    destructor Destroy; override;
    procedure WriteInChess4NetFormat(const SourceTree: TPlysTree);
    property Data: TStrings read FGetData;
  end;

implementation

uses
  SysUtils, StrUtils;

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
begin
  FClear;

  if ((not Assigned(SourceTree)) or (SourceTree.Count = 0)) then
    exit;

  m_Tree := TPlysTree.Create;
  try
    m_Tree.Assign(SourceTree);

    FWriteTag('C4N', '1'); // versionning for future uses
    FWriteLine;
    FWriteWrappedText(FConvertTreeToText);
    
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
  m_strlData.Append(Format('[%s "%s"]', [strTagName, wstrTagData]));
end;


procedure TPGNWriter.FWriteLine;
begin
  m_strlData.Append('');
end;


procedure TPGNWriter.FWriteWrappedText(const wstr: WideString);
begin
  m_strlData.Append(wstr);
  // TODO: Text wrapping
end;


function TPGNWriter.FConvertTreeToText: WideString;

var
  wstrData: WideString;
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

    wstrData := wstrData + IfThen(bAddIndentFormatting, ' ') + wstrMoveNumber + ' ' + wstrPly;

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
            wstrData := wstrData + ' (';
            bExplicitNumberingFormatting := TRUE;
            bAddIndentFormatting := FALSE;
          end;

          NWritePly(iPly, strlPlys[i]);

          m_Tree.SetPlyForPlyIndex(iPly, strlPlys[i]);
          NFConvertTreeToText(iPly);

          if (i = strlPlys.Count - 1) then
            wstrData := wstrData + ')'
          else
          begin
            wstrData := wstrData + ') ('; // ';';
            bAddIndentFormatting := FALSE;            
          end;

          bExplicitNumberingFormatting := TRUE;
        end; // for i

        m_Tree.SetPlyForPlyIndex(iPly, strlPlys[0]);

      end; // while

    finally
      strlPlys.Free;
    end;

  end;

begin // .FConvertTreeToText
  wstrData := '';

  bExplicitNumberingFormatting := TRUE;
  bAddIndentFormatting := FALSE;
  
  NFConvertTreeToText;

  Result := wstrData;
end;


function TPGNWriter.FGetData: TStrings;
begin
  Result := m_strlData as TStrings;
end;

end.
