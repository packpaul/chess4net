////////////////////////////////////////////////////////////////////////////////
// All code below is exclusively owned by author of Chess4Net - Pavel Perminov
// (packpaul@mail.ru, packpaul1@gmail.com).
// Any changes, modifications, borrowing and adaptation are a subject for
// explicit permition from the owner.

unit LoggerUnit;

interface

type
  TLogLevel = (llError, llWarn, llInfo, llDebug);

  ILogAppender = interface
    procedure DoAppend(const Level: TLogLevel; const strMsg: string);
  end;

  TLogger = class
  private
    m_Appender: ILogAppender;
    constructor FCreate;
  public
    constructor Create;
    destructor Destroy; override;
    class function GetInstance: TLogger;
    class procedure FreeInstance; reintroduce;

    procedure Log(const Level: TLogLevel; const strMsg: string);
    procedure Error(const strMsg: string);
    procedure Warn(const strMsg: string);
    procedure Info(const strMsg: string);
    procedure Debug(const strMsg: string);

    property Appender: ILogAppender read m_Appender write m_Appender;
  end;

implementation

uses
  SysUtils;

var
  g_Logger: TLogger = nil;

////////////////////////////////////////////////////////////////////////////////
// TLogger

constructor TLogger.Create;
begin
  raise Exception.Create(ClassName + ' cannot be instaniated directly!');
end;


constructor TLogger.FCreate;
begin
  inherited Create;
end;


destructor TLogger.Destroy;
begin
  g_Logger := nil;
  inherited;
end;


class function TLogger.GetInstance: TLogger;
begin
  if (not Assigned(g_Logger)) then
    g_Logger := TLogger.FCreate;
  Result := g_Logger;
end;


class procedure TLogger.FreeInstance;
begin
  g_Logger.Free;
end;


procedure TLogger.Log(const Level: TLogLevel; const strMsg: string);
begin
  if (Assigned(m_Appender)) then
    m_Appender.DoAppend(Level, strMsg);
end;


procedure TLogger.Error(const strMsg: string);
begin
  Log(llError, strMsg);
end;


procedure TLogger.Warn(const strMsg: string);
begin
  Log(llWarn, strMsg);
end;


procedure TLogger.Info(const strMsg: string);
begin
  Log(llInfo, strMsg);
end;


procedure TLogger.Debug(const strMsg: string);
begin
  Log(llDebug, strMsg);
end;

initialization

finalization
  TLogger.FreeInstance;

end.
