////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit FileLogAppenderUnit;

interface

uses
  Classes,
  //
  LoggerUnit;

type
  TFileLogAppender = class(TInterfacedObject, ILogAppender)
  private
    m_Stream: TFileStream;
  public
    constructor Create(const strLogFileName: string);
    destructor Destroy; override;
    procedure DoAppend(const Level: TLogLevel; const strMsg: string);
  end;

implementation

uses
  SysUtils;

const
  LEVEL_TAGS: array[TLogLevel] of string = ('[ERROR] ', '[WARN] ', '[INFO] ', '[DEBUG] ');

////////////////////////////////////////////////////////////////////////////////
// TFileLogAppender

constructor TFileLogAppender.Create(const strLogFileName: string);
var
  FileHandle: THandle;
begin
  inherited Create;

  if (not FileExists(strLogFileName)) then
  begin
    FileHandle := FileCreate(strLogFileName);
    FileClose(FileHandle);
  end;
  m_Stream := TFileStream.Create(strLogFileName, fmOpenReadWrite, fmShareDenyWrite);
  m_Stream.Seek(0, soFromEnd)
end;


destructor TFileLogAppender.Destroy;
begin
  m_Stream.Free;
  inherited;
end;


procedure TFileLogAppender.DoAppend(const Level: TLogLevel; const strMsg: string);
var
  str: string;
begin
  if (strMsg = '') then
    exit;
  str := LEVEL_TAGS[Level] + strMsg;
  m_Stream.Write(str[1], Length(str));
  m_Stream.Write(sLineBreak[1], Length(sLineBreak));
end;

end.
