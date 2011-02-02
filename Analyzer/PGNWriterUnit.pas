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
  public
    constructor Create;
    destructor Destroy; override;
    procedure WriteInChess4NetFormat(const SourceTree: TPlysTree);
    property Data: TStringList read m_strlData;
  end;

implementation

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
  // TODO:
end;

end.
